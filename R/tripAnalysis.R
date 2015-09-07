#' Trip Analysis
#'
#' Initializes database connection and calls series of functions used to analyze ORcycle trip data.
#'
#' @param db String indicating which db to use, default is "test", also can be "release"
#' @param db_path Relative or absolute file path to db_credentials.json
#'
#' @return Trip analysis results
#'
#' @export

tripAnalysis = function(db="test", db_path = "data/db_credentials.json"){

  ###Load neccesary libraries
  require(DBI)
  require(RMySQL)
  require(lubridate)
  require(mail)
  require(rjson)

  ###Read in db authentication info
  cred = rjson::fromJSON(file=db_path)
  if(db =="test"){
    db_cred = cred$db_credentials$test
  }else if(db=="release"){
    db_cred = cred$db_credentials$release
  }else{
    stop("db must be 'test' or 'release'")
  }

  ####Connect to ORcycle db
  ####Make sure you VPN into cecs network
  con <- DBI::dbConnect(dbDriver("MySQL"), host=db_cred$db_host, port= 3306, user=db_cred$db_user, password = db_cred$db_pass, dbname=db_cred$db_name)

  #Read raw tables from MySQL database
  print("reading tables from database, takes at ~1 minute or more")
  coords= DBI::dbReadTable(con, "coord")
  trips = DBI::dbReadTable(con, "trip")
  tripResponses = DBI::dbReadTable(con, "tripResponse")
  questions = DBI::dbReadTable(con, "questions")
  answers = DBI::dbReadTable(con,"answers")
  qaMap = DBI::dbReadTable(con,"question_answer_relate")
  users = DBI::dbReadTable(con,"user")
  print("Finished reading tables from database")

  #subset trips by
  #1.) Exclude flagged users and
  #2.) Exclude trips with trip details set to 'test'
  flagged = users$id[users$flagUser ==1]
  trips = subset(trips,!(trips$user_id %in% flagged))
  tripResponses = subset(tripResponses, tripResponses$trip_id %in% trips$id)
  trips = subset(trips,trips$n_coord>0)
  trips = subset(trips, trips$notes!="test")
  coords = subset(coords,coords$trip_id %in% trips$id)

  ###Combine useful data from db tables into one table for exploration/modeling/graphing
  ##trip statistics calculation takes a little while, progress will print to console
  tripSummary = tripPrep(trips=trips,tripResponses=tripResponses,
                         answers=answers,questions=questions,
                         qaMap=qaMap,coords=coords)

  ####Plot single variable distribution plots to results/plots_singleVar/trips, overwrite old results
  trip_barPlots(tripSummary)

  ###Add temporal category data from trip time
  tripSumImpTime = attach_TripTimeData(tripTable=tripSumImp,
                                       trips=trips)
  ###Caution: long function
  ###Add trip weather data from trip start time and starting location (grabs weather from nearest airport station)
  tripSumImpTimeWthr = attach_TripWeatherData(tripTable=tripSumImpTime,
                                              trips=trips,coords = coords)

  updateMapMatchCoords(coords, matchLinksPath = "working_data/orc_gpslinks_raw.csv")

  ###E-mail notification for script completion
  #sendmail("bblanc@pdx.edu", subject="Phase 1 of trip analysis complete",
  #         message="Calculation finished!", password="rmail")

  ###Map Match Python Script must be run if there are new trips to match
  ###Switch over to PyCharm (or another Python console) to run the map matching script on the new coordinates
  updateMapMatchLinks(matchLinksPath="working_data/orc_gpslinks_raw.csv",
                      matchTripsPath = "working_data/orc_gpstrips.csv")

  tripSumImpTimeWthrGeo = attach_TripGeoData(tripSummary=tripSumImpTimeWthr,
                                             matchLinksPath="working_data/orc_gpslinks_raw.csv",
                                             matchTripsPath = "working_data/orc_gpstrips.csv")

  ###Analyze trip similarity by user and trip purpose
  tripMatchSummary = subset(tripSumImpTimeWthrGeo, tripSumImpTimeWthrGeo$matched==TRUE)
  tripModel = similarityWeight(matchLinksPath = "working_data/orc_gpslinks_raw.csv", trips = trips, tripMatchSummmary = tripMatchSummary)

  ###Save out prepared trip model table
  date = substr(as.character(Sys.time()),1,10)
  saveRDS(tripModel,paste0("working_data/tripModelData/preparedTripModelTab_",date,".rds"))

  ###E-mail notification for script completion
  #sendmail("bblanc@pdx.edu", subject="Phase 2 of trip analysis complete",
  #         message="Calculation finished!", password="rmail")

  for (i in 1:nrow(tripTab)){
    tid = tripTab$trip_id[i]
    if(tid %in% tripSummary$trip_id){
      tripTab$routeFreq[i]=tripSummary$routeFreq[tripSummary$trip_id==tid]
      tripTab$routeComfort[i]=tripSummary$routeComfort[tripSummary$trip_id==tid]
      tripTab$purpose[i]=tripSummary$purpose[tripSummary$trip_id==tid]
    }else{
      tripTab$routeFreq[i]=NA
      tripTab$routeComfort[i]=NA
      tripTab$purpose[i]=NA
    }

  }
  tmImp = imputeResponses(data=tripTab,impCols = c(2:29,46:81))
  date = substr(as.character(Sys.time()),1,10)
  saveRDS(tmImp,paste0("working_data/tripModelData/preparedTripModelTab_",date,".rds"))
}



