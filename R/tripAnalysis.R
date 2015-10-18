#' Trip Analysis
#'
#' Initializes database connection and calls series of functions used to analyze ORcycle trip data.
#'
#' @param db String indicating which db to use, default is "test", also can be "release"
#' @param db_path Relative or absolute file path to db_credentials.json
#'
#' @return None
#'
#' @export

tripAnalysis = function(db="test", db_path = "source_data/db_credentials.json"){
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
  con <- DBI::dbConnect(DBI::dbDriver("MySQL"), host=db_cred$db_host, port= 3306, user=db_cred$db_user, password = db_cred$db_pass, dbname=db_cred$db_name)

  #Read raw tables from MySQL database
  print("reading tables from database, takes ~1 minute or more")
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
  trips = subset(trips,trips$n_coord>100)
  coords = subset(coords,coords$trip_id %in% trips$id)

  ###Combine useful data from db tables into one table for exploration/modeling/graphing
  ##trip statistics calculation takes a little while, progress will print to console
  tripSummary = tripPrep(trips=trips,tripResponses=tripResponses,
                         answers=answers,questions=questions,
                         qaMap=qaMap,coords=coords)

  ####Plot single variable distribution plots to results/{db}/plots_singleVar/trips, overwrite old results
  
  if(!dir.exists("results")){
    dir.create("results")
  }
  if(!dir.exists(db)){
    dir.create(paste0("results/",db))
  }
  if(!dir.exists(paste0("results/",db,"/plots_singleVar"))){
    dir.create(paste0("results/",db,"/plots_singleVar"))
  }
  if(!dir.exists(paste0("results/",db,"/plots_singleVar/trips"))){
    dir.create(paste0("results/",db,"/plots_singleVar/trips"))
  }
  ggplot2::theme_set(ggplot2::theme_bw(base_size = 25))
  trip_barPlots(tripSummary,outDir=paste0("results/",db,"/plots_singleVar/trips/"))
  print("Trip plots printed to results.")

  impCols = colnames(tripSummary)[!(colnames(tripSummary) %in% c("trip_id","cumLength","duration","avgSpeed"))]

  ##Impute Missing Responses
  print("Imputing missing responses...")
  tripSumImp = imputeResponses(data=tripSummary,impCols = impCols)

  ###Add temporal category data from trip time
  tripSumImpTime = attach_TripTimeData(tripTable=tripSumImp,
                                       trips=trips)
  ###Caution: long function
  ###Add trip weather data from trip start time and starting location
  # (grabs weather from nearest airport station)
  tripSumImpTimeWthr = attach_TripWeatherData(tripTable=tripSumImpTime,
                                              trips=trips,coords = coords)

  print("Finished matching weather info, now matching geographic data. This will only use trips that have been matched to the network. To match more trips, see the updateMapMatching() function.")
  #Attach geographic data from network
  tripSumImpTimeWthrGeo = attach_TripGeoData(tripSummary=tripSumImpTimeWthr,
                                             matchLinksPath=paste0("working_data/",dbFolder,"/orc_gpslinks_raw.csv"),
                                             matchTripsPath = paste0("working_data/",dbFolder,"/orc_gpstrips.csv"))
  
  print("Finished matching geographic information, analyzing trip similarity")
  ###Analyze trip similarity by user and trip purpose
  tripMatchSummary = subset(tripSumImpTimeWthrGeo, tripSumImpTimeWthrGeo$matched==TRUE)
  tripModel = similarityWeight(matchLinksPath = "working_data/orc_gpslinks_raw.csv", trips = trips, tripMatchSummmary = tripMatchSummary)

  ###Save out prepared trip model table
  print("Saving out prepared trip model table")
  date = substr(as.character(Sys.time()),1,10)
  saveRDS(tripModel,paste0("working_data/",db,"/tripModelData/preparedTripModelTab_",date,".rds"))

  # for (i in 1:nrow(tripTab)){
  #   tid = tripTab$trip_id[i]
  #   if(tid %in% tripSummary$trip_id){
  #     tripTab$routeFreq[i]=tripSummary$routeFreq[tripSummary$trip_id==tid]
  #     tripTab$routeComfort[i]=tripSummary$routeComfort[tripSummary$trip_id==tid]
  #     tripTab$purpose[i]=tripSummary$purpose[tripSummary$trip_id==tid]
  #   }else{
  #     tripTab$routeFreq[i]=NA
  #     tripTab$routeComfort[i]=NA
  #     tripTab$purpose[i]=NA
  #   }
  #
  # }
  #
  # date = substr(as.character(Sys.time()),1,10)
  # saveRDS(tmImp,paste0("working_data/tripModelData/preparedTripModelTab_",db,"_",date,".rds"))
}



