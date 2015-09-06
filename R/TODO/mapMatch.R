library(rPython)

mapMatch = function(coords,matchLinksPath){
  existingMatches = read.csv("working_data/orc_gpslinks_raw.csv")
  matchedTrips = sort(unique(existingMatches$gpstripid))
  allTrips = sort(unique(coords$trip_id))
  unmatchedTrips = allTrips[allTrips > max(matchedTrips)]
  unmatchedCoords = subset(coords,coords$trip_id %in% unmatchedTrips)
  unmatchedCoords = subset(unmatchedCoords,unmatchedCoords$latitude!=0 & unmatchedCoords$longitude!=0)
  df = cbind(rownames(unmatchedCoords),unmatchedCoords)
  colnames(df)= c("id","trip_id","time", "lat", "long", "alt", "speed","hAcc","vAcc")
  write.csv(df,"mapMatch_Broach/data/coordsForMatching.csv",row.names = FALSE)
}