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