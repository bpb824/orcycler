library(rgdal)
library(rgeos)
library(geosphere)
library(timeSeries)
library(DBI)
library(RPostgreSQL)

setwd("/Users/bblanc/OneDrive/_BikeAppProject/ORcycle_Analysis_Tool_Suite")

netAtts = read.csv("source_data/modelNetwork/networkLink_Attributes.csv")
matchedLinks = read.csv("working_data/orc_gpslinks_raw.csv")
modelNetwork = readOGR("source_data/modelNetwork","wbnet2012")
modelNetwork = spTransform(modelNetwork,CRS("+init=epsg:4326"))

source("functions/googleMapUtils.R")

lines2points = function(linesShape,trip_id){
  numCoords = 0
  for (i in 1:length(linesShape)){
    numCoords = numCoords + nrow(linesShape@lines[[i]]@Lines[[1]]@coords)
  }
  
  crds = data.frame(matrix(nrow = numCoords,ncol = 9))
  colnames(crds)=c("id","trip_id","time","lat","long","alt","speed","hAcc","vAcc")
  crds$id = rownames(crds)
  crds$trip_id=trip_id
  startTime = as.POSIXct(strptime("2015-03-14  09:26:53",tz="America/Los_Angeles",format = "%Y-%m-%d %H:%M:%S"))
  endTime = startTime +(numCoords-1)*2
  timeVector = timeSequence(startTime,endTime,by="2 sec",FinCenter = "America/Los_Angeles")
  crds$time=as.character(timeVector)
  crds$alt=100
  crds$speed=5
  crds$hAcc=5
  crds$vAcc=5
  
  cn = 1
  for (i in 1:length(linesShape)){
    segCoords = linesShape@lines[[i]]@Lines[[1]]@coords
    crds[cn:(cn+nrow(segCoords)-1),c("lat","long")]=cbind(segCoords[,2],segCoords[,1])
    cn=cn+nrow((segCoords))
  }
  return(crds)
}

tripList = sort(unique(matchedLinks$gpstripid))
dirsShapeList = list()

for (i in 210:length(tripList)){
  tripLinks = subset(matchedLinks,matchedLinks$gpstripid==tripList[i])
  firstLink = tripLinks$featureid[tripLinks$gid == min(tripLinks$gid)]
  lastLink =tripLinks$featureid[tripLinks$gid == max(tripLinks$gid)]
  firstLinkLine = modelNetwork@lines[[which(modelNetwork$psuid == firstLink)]]
  if (tripLinks$direction[tripLinks$featureid==firstLink]==1){
    coords =firstLinkLine@Lines[[1]]@coords 
    firstCoord = coords[1,]
  }else{
    coords =firstLinkLine@Lines[[1]]@coords 
    firstCoord = firstLinkLine@Lines[[1]]@coords[nrow(coords),]
  }
  lastLinkLine = modelNetwork@lines[[which(modelNetwork$psuid == lastLink)]]
  if (tripLinks$direction[tripLinks$featureid==lastLink]==-1){
    coords =lastLinkLine@Lines[[1]]@coords 
    lastCoord = coords[1,]
  }else{
    coords =lastLinkLine@Lines[[1]]@coords 
    lastCoord = coords[nrow(coords),]
  }
  firstCoord=paste(firstCoord[2],firstCoord[1],sep=",")
  lastCoord = paste(lastCoord[2],lastCoord[1],sep=",")

  dirs = GetDirections(firstCoord,lastCoord)
  if(dirs$status!="ZERO_RESULTS"){
    dirsShapeList[[i]] = route2Shape(dirs)
  }else{
    dirsShapeList[[i]]=NA
  }
  print(paste0("Fetched directions for trip #",i," of ",length(tripList)))
  Sys.sleep(2)
}

dirsPointsList=list()
for (i in 1:length(dirsShapeList)){
  if(!is.na(dirsShapeList[[i]])){
    dirsPointsList[[i]] = lines2points(dirsShapeList[[i]],tripList[i])
  }else{
    dirsPointsList[[i]] = NA
  }
  
  print(paste0("Exploded line set ",i ," of ",length(tripList)))
}

coordCount = 0
for (i in 1:length(dirsPointsList)){
  if(!is.na(dirsPointsList[[i]])){
    coordCount = coordCount + nrow(dirsPointsList[[i]])
  }
}

cFrame = as.data.frame(matrix(nrow = coordCount,ncol = 9))
colnames(cFrame)=colnames(dirsPointsList[[i]])
ci = 1
for (i in 1:length(dirsPointsList)){
  if(!is.na(dirsPointsList[[i]])){
    cFrame[ci:(ci+nrow(dirsPointsList[[i]])-1),]= dirsPointsList[[i]]
    ci = ci + nrow(dirsPointsList[[i]])
  }
}



#tripShape = SpatialLines(modelNetwork@lines[which(modelNetwork$psuid %in% tripLinks$featureid)])

write.csv(cFrame,"mapMatch_Broach/data/googleRoutePoints.csv")

drv = dbDriver("PostgreSQL")
con = dbConnect(drv = drv, dbname = "ride",user = "orcycle",password ="bikesarefun!", port = 5432, host = "localhost")
dbLinks = dbGetQuery(con,"SELECT * FROM data.orc_gpslinks")
dbTrips = dbGetQuery(con,"SELECT * FROM data.orc_gpstrips")
write.csv(dbLinks,"working_data/gLinks.csv",row.names = FALSE)
write.csv(dbTrips,"working_data/gTrips.csv",row.names = FALSE)
colnames(dbLinks)[colnames(dbLinks)=="featureid"]="psuid"

gTripList = sort(unique(dbLinks$gpstripid))
gTripSummary = as.data.frame(gTripList)
colnames(gTripSummary)="trip_id"

source("functions/attach_TripGeoData.R")
gTripSummaryGeo = attach_TripGeoData(gTripSummary,matchLinksPath = "working_data/gLinks.csv", matchTripsPath ="working_data/gTrips.csv",linkType = "google")

saveRDS(gTripSummaryGeo,"working_data/googleTripTable.rds")



