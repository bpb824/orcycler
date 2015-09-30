updateMapMatching = function(coords, db_type,
                    pythonPath = "/Users/bblanc/OneDrive/_BikeAppProject/ORcycle_Analysis_Tool_Suite/orcycler_environment/mapMatching/mapmatch_batch.py"){
  if(db_type=="test"){
    dbFolder = "testDatabase"
  }else if(db_type =="release"){
    dbFolder = "releaseDatabase"
  }

  updateMapMatchCoords(coords, matchLinksPath = paste0("working_data/",dbFolder,"/orc_gpslinks_raw.csv"), db_version="release")

  print("Running map matching process, this can take a long time. Intermediate output will follow.")
  system(paste0("python ",pythonPath))

  ###Map Match Python Script must be run if there are new trips to match
  ###Switch over to PyCharm (or another Python console) to run the map matching script on the new coordinates
  updateMapMatchLinks(matchLinksPath=paste0("working_data/",dbFolder,"/orc_gpslinks_raw.csv"),
                      matchTripsPath = paste0("working_data/",dbFolder,"/orc_gpslinks_raw.csv"))

}

#' Update links and trips files from map matching.
#'
#' This function updates map-matched links and trips files with results from python scripts stored in local PostgreSQL database.
#'
#' @param matchLinksPath Relative or absolute path to existing matched link set.
#' @param matchTripsPath Relative or absolute path to existing matched trip set.
#'
#' @return None
#'
#' @export
#'
updateMapMatchLinks = function(matchLinksPath,matchTripsPath){
  require(DBI)
  require(RPostgreSQL)
  require(dplyr)

  orc_gpslinks = read.csv(matchLinksPath)
  oldTrips = sort(unique(orc_gpslinks$gpstripid))
  drv = dbDriver("PostgreSQL")
  con = dbConnect(drv = drv, dbname = "orcycle",user = "orcycle",password ="bikesarefun!", port = 5432, host = "localhost")
  dbLinks = dbGetQuery(con,"SELECT * FROM data.orc_gpslinks")
  dbTrips = sort(unique(dbLinks$gpstripid))
  newTrips = dbTrips[!(dbTrips %in% oldTrips)]
  if (length(newTrips >0)){
    print(paste0(length(newTrips)," new trips found, added to raw links file."))
    newLinks = subset(dbLinks,dbLinks$gpstripid %in% newTrips)
    updatedLinks = rbind(orc_gpslinks,newLinks)
    write.csv(updatedLinks,matchLinksPath, row.names = FALSE)

    oldTripTab = read.csv(matchTripsPath,stringsAsFactors = FALSE)
    dbTripTab = dbGetQuery(con,"SELECT * FROM data.orc_gpstrips")
    dbTripTab = dbTripTab[,1:21]
    newTripTab = subset(dbTripTab,!(dbTripTab$gpstripid %in% oldTripTab$gpstripid))
    orc_gpstrips = rbind(oldTripTab,newTripTab)
    orc_gpstrips = orc_gpstrips[order(orc_gpstrips$gpstripid),]
    write.csv(orc_gpstrips,matchTripsPath,row.names = FALSE)

  }else{
    print("No new trips found.")
  }
}

#' Update coordinate file for map matching
#'
#' This function updates queued coordinates for map matching. Existing map match links are considered to filter coordinate file to only trips that have not been matched.
#'
#' @param coords 'coord' table from SQL database
#' @param matchLinksPath Relative or absolute path to existing matched link set.
#' @param db_version String indicating database version (i.e 'test' or 'release')
#'
#' @return None
#'
#' @export
#'
updateMapMatchCoords = function(coords,matchLinksPath, db_version){
  if(file.exists(matchLinksPath)){
    existingMatches = read.csv(matchLinksPath)
    matchedTrips = sort(unique(existingMatches$gpstripid))
    allTrips = sort(unique(coords$trip_id))
    unmatchedTrips = allTrips[allTrips > max(matchedTrips)]
  }else{
    unmatchedTrips = sort(unique(coords$trip_id))
  }
  if (length(unmatchedTrips>0)){
    print(paste0(length(unmatchedTrips)," new trips found for matching, adding to coordinate input file."))
    unmatchedCoords = subset(coords,coords$trip_id %in% unmatchedTrips)
    unmatchedCoords = subset(unmatchedCoords,unmatchedCoords$latitude!=0 & unmatchedCoords$longitude!=0)


  }else{
    "No new trips found in remote database for matching."
  }
  if (db_version =="test"){
    df = cbind(rownames(unmatchedCoords),unmatchedCoords)
    colnames(df)= c("id","trip_id","time", "lat", "long", "alt", "speed","hAcc","vAcc")
  }else if (db_version =="release"){
    df = unmatchedCoords
    colnames(df)= c("id","trip_id","time", "lat", "long", "alt", "speed","hAcc","vAcc")
  }
  write.csv(df,"mapMatching/data/coordsForMatching.csv",row.names = FALSE)
}
