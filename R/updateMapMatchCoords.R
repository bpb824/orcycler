#' Update coordinate file for map matching
#'
#' This function updates queued coordinates for map matching. Existing map match links are considered to filter coordinate file to only trips that have not been matched. 
#'
#' @param coords 'coord' table from SQL database
#' @param matchLinksPath Relative or absolute path to existing matched link set.
#'
#' @return None
#'
#' @export
#' 
updateMapMatchCoords = function(coords,matchLinksPath){
  existingMatches = read.csv("working_data/orc_gpslinks_raw.csv")
  matchedTrips = sort(unique(existingMatches$gpstripid))
  allTrips = sort(unique(coords$trip_id))
  unmatchedTrips = allTrips[allTrips > max(matchedTrips)]
  if (length(unmatchedTrips>0)){
    print(paste0(length(unmatchedTrips)," new trips found for matching, adding to coordinate input file."))
    unmatchedCoords = subset(coords,coords$trip_id %in% unmatchedTrips)
    unmatchedCoords = subset(unmatchedCoords,unmatchedCoords$latitude!=0 & unmatchedCoords$longitude!=0)
    df = cbind(rownames(unmatchedCoords),unmatchedCoords)
    colnames(df)= c("id","trip_id","time", "lat", "long", "alt", "speed","hAcc","vAcc")
    write.csv(df,"mapMatch_Broach/data/coordsForMatching.csv",row.names = FALSE)
  }else{
    "No new trips found in remote database for matching."
  }
}