#######
# Attribute Links Script
# Written by Bryan Blanc
#######

library(sp)
library(rgdal)
library(mail)
library(rgeos)

sysName = Sys.info()["sysname"]
if (sysName == "linux"){
  setwd("//stash/bikeapp/ORcycle_Analysis_Tool_Suite")
}else{
  setwd("/Users/bblanc/OneDrive/_BikeAppProject/ORcycle_Analysis_Tool_Suite")
}

modelNetwork = readOGR("source_data/modelNetwork","wbnet2012")
defCodes = read.csv("source_data/modelNetwork/metroDefCodes.csv")
linkType = read.csv("source_data/modelNetwork/link_attributes.csv")
modelNetData = modelNetwork@data

networkLinks = data.frame(matrix(nrow=nrow(modelNetData),ncol=10))
colnames(networkLinks) = c("psuid","linkType","bikeTypeLong","length_m","slope","traffic","trafficCat", "speed","speedCat","facility")
networkLinks$psuid=modelNetData$psuid

###Bicycle Type Classification
startTime = Sys.time()
for(i in 1:nrow(networkLinks)){
  linkid= networkLinks$psuid[i]
  if (linkid <0){
    linkid = linkid*-1
  }
  linkCode = linkType[linkType$psuid == linkid,]$type
  linkDef = as.character(defCodes[defCodes$value==linkCode,]$meaning)
  bikeDef = as.character(linkType[linkType$psuid == linkid,]$facility2013)
  if(length(linkDef) == 0|| nchar(linkDef) == 0){
    networkLinks$linkType[i] = "UNKNOWN."
  }else{
    networkLinks$linkType[i] = linkDef
  }
  if(nchar(bikeDef)>0){
    if(bikeDef == "SHL-WIDE"||bikeDef=="SHL-NRRW"){
      bikeText = "Shoulder"
    }else if(bikeDef=="PTH-LOMU"||bikeDef=="PTH-REMU"){
      bikeText = "Seperated Path"
    }else if(bikeDef=="BKE-TRAK"){
      bikeText = "Cycletrack"
    }else if(bikeDef=="BKE-BLVD"){
      bikeText = "Bicycle Boulevard"
    }else if(bikeDef=="BKE-BUFF"){
      bikeText = "Buffered Bike Lane"
    }else if(bikeDef=="BKE-LANE"){
      bikeText = "Bike Lane"
    }else if(bikeDef=="NO-BIKES"){
      bikeText = "Bicycles Prohibited"
    }else if(bikeDef=="OTH-XING"||bikeDef=="OTH-CONN"||bikeDef=="OTH-SWLK"){
      bikeText = "Other Connection"
    }else{
      bikeText="NONE"
    }
    networkLinks$bikeTypeLong[i] = bikeText
  }else{
    networkLinks$bikeTypeLong[i] = "NONE"
  }
  networkLinks$length_m[i] = modelNetData[modelNetData$psuid == linkid,]$length_m
  now = Sys.time()
  
  
  
  print(paste("Bike Type of Link #",i,"of ",nrow(networkLinks)," classified, ",round(as.numeric(difftime(now,startTime,units = "mins")),2)," minutes so far"))
}
sendmail("bblanc@pdx.edu", subject="Bicycle link classification finished!",
         message="Calculation finished!", password="rmail")

elevation = readOGR("source_data/modelNetwork","networkNodesInterp")
elevNodes = elevation@data
elevNodes$ORIG_FID = as.numeric(rownames(elevNodes))

is.even <- function(x) x %% 2 == 0

elevNodes$start[is.even(elevNodes$ORIG_FID)]=TRUE
elevNodes$start[!is.even(elevNodes$ORIG_FID)]=FALSE

n = nrow(networkLinks)

networkLinks$slope = NA

for (i in 1:nrow(networkLinks)){
  link = networkLinks$psuid[i]
  y1 = elevNodes$RASTERVALU[elevNodes$start==TRUE & elevNodes$psuid == link]
  y2 = elevNodes$RASTERVALU[elevNodes$start==FALSE & elevNodes$psuid == link]
  #direction = networkLinks$direction[i]
  length = networkLinks$length_m[i]*3.28084
  networkLinks$slope[i] = ((y2-y1)/(length))*100
  print(paste("Calculated slope for link #",i," of ",n))
}

sendmail("bblanc@pdx.edu", subject="Slope calculation finished!",
         message="Calculation finished!", password="rmail")
#networkLinks$slope = networkLinks$slope *-1

# ##Categorize slopes
# for (i in 1:nrow(networkLinks)){
#   slope = networkLinks$slope[i]
#   if(slope <=-2){
#     slopeCat = "slp1"
#   }else if(slope >-2 & slope <=2){
#     slopeCat = "slp2"
#   }else if(slope >2 & slope <=4){
#     slopeCat = "slp3"
#   }else if(slope >4 & slope <=6){
#     slopeCat = "slp4"
#   }else if(slope >6){
#     slopeCat = "slp5"
#   }else{
#     slopeCat = NA
#   }
#   networkLinks$slopeCat[i] = slopeCat
#   print(paste("Categorized slope for link #",i," of ",n))
# }

#####Bring in interpolated traffic volumes from PBOT
volumes = read.csv("source_data/modelNetwork/bikenet2012_est_vol.csv")
n = nrow(networkLinks)
for (i in 1:nrow(networkLinks)){
  link = networkLinks$psuid[i]
  if (length(volumes$est_vol[volumes$psuid == link])==1){
    networkLinks$traffic[i] = volumes$est_vol[volumes$psuid == link]
  }else{
    networkLinks$traffic[i] = NA
  }
  print(paste("Fetched traffic vol for link #",i," of ",n))
}

networkLinks$trafficCat[networkLinks$traffic < 5000]="t_less5k"
networkLinks$trafficCat[networkLinks$traffic >= 5000 & networkLinks$traffic <10000]="t_5to10k"
networkLinks$trafficCat[networkLinks$traffic >= 10000 & networkLinks$traffic <20000]="t_10to20k"
networkLinks$trafficCat[networkLinks$traffic >= 20000 & networkLinks$traffic <30000]="t_20to30k"
networkLinks$trafficCat[networkLinks$traffic >30000]="t_greater30k"

sendmail("bblanc@pdx.edu", subject="Traffic calculation finished!",
         message="Calculation finished!", password="rmail")

modelStreetsShape = readOGR("source_data/modelNetwork", "modelStreetNetwork")
modelStreets = modelStreetsShape@data

speedCats = unique(modelStreets$SPEED)
modelStreets$collector = FALSE
modelStreets$collector[modelStreets$TYPE == 1450 | modelStreets$TYPE == 5501]=TRUE
modelStreets$secArt = FALSE
modelStreets$secArt[modelStreets$TYPE == 1400 | modelStreets$TYPE == 5401]=TRUE
modelStreets$primArt = FALSE
modelStreets$primArt[modelStreets$TYPE == 1300 | modelStreets$TYPE == 5301]=TRUE
modelStreets$highway = FALSE
modelStreets$highway[modelStreets$TYPE == 1200 | modelStreets$TYPE == 1110]=TRUE
modelStreets$ramp= FALSE
modelStreets$ramp[modelStreets$TYPE == 1521 | modelStreets$TYPE == 1221 | modelStreets$TYPE == 1121]=TRUE

speedDF = subset(modelStreets,modelStreets$SPEED !=0)
speedModel = lm(formula = SPEED~collector+secArt+primArt+highway+ramp,data = speedDF)

modelStreets$SPEED[modelStreets$SPEED==0]=24.99537+2.37007*modelStreets$collector[modelStreets$SPEED==0]+5.74529*modelStreets$secArt[modelStreets$SPEED==0]+9.46375*modelStreets$primArt[modelStreets$SPEED==0]+17.86005*modelStreets$highway[modelStreets$SPEED==0]+7.43347*modelStreets$ramp[modelStreets$SPEED==0]

n = nrow(networkLinks)
for (i in 1:nrow(networkLinks)){
  link = networkLinks$psuid[i]
  if (length(modelStreets$SPEED[modelStreets$LOCALID == link])==1){
    networkLinks$speed[i] = modelStreets$SPEED[modelStreets$LOCALID == link]
  }else{
    networkLinks$speed[i] = NA
  }
  print(paste("Fetched speed for link #",i," of ",n))
}
hasVol = subset(networkLinks,networkLinks$traffic!=0)
noVol = subset(networkLinks,networkLinks$traffic==0)
nas = subset(networkLinks,is.na(networkLinks$traffic))
noVol$speed = 0
sm2 = lm(formula = speed~traffic,data=hasVol)
withSpeed = subset(hasVol,!is.na(hasVol$speed))
noSpeed = subset(hasVol,is.na(hasVol$speed))

noSpeed$speed =predict.lm(object=sm2,newdata =noSpeed)

newDF = rbind(withSpeed,noSpeed,noVol,nas)

networkLinks = newDF

networkLinks$speedCat[networkLinks$speed <= 20]="s_less.equal_20"
networkLinks$speedCat[networkLinks$speed > 20 & networkLinks$speed <35]="s_20to35"
networkLinks$speedCat[networkLinks$speed >=35]="s_greater35"

streetTypes = unique(networkLinks$linkType)
bikeTypes = unique(networkLinks$bikeTypeLong)
primArt = streetTypes[c(5,7,8,9,10,11,12)]
minArt = streetTypes[c(6,15,16,17)]
res = streetTypes[c(1,3)]
other = streetTypes[!(streetTypes %in% c(primArt,minArt,res))]
bikeLane = bikeTypes[2]
cycleTrack = bikeTypes[c(7,8)]
bikeBoul = bikeTypes[3]
sepPath = bikeTypes[9]
noBike = bikeTypes[c(1,4,5,6)]


n = nrow(networkLinks)
networkLinks$facility = NA
for (i in 1:nrow(networkLinks)){
  streetType = as.character(networkLinks$linkType[i])
  bikeType = as.character(networkLinks$bikeTypeLong[i])
  
  if (streetType %in% primArt & bikeType %in% bikeLane){
    networkLinks$facility[i] = "bl_primArt"
  }else if (streetType %in% minArt & bikeType %in% bikeLane){
    networkLinks$facility[i] = "bl_minArt"
  }else if (streetType %in% res & bikeType %in% bikeLane){
    networkLinks$facility[i] = "bl_res"
  }else if (streetType %in% other & bikeType %in% bikeLane){
    networkLinks$facility[i] = "bl_other"
  }else if (streetType %in% primArt & bikeType %in% noBike){
    networkLinks$facility[i] = "nb_primArt"
  }else if (streetType %in% minArt & bikeType %in% noBike){
    networkLinks$facility[i] = "nb_minArt"
  }else if (streetType %in% res & bikeType %in% noBike){
    networkLinks$facility[i] = "nb_res"
  }else if (streetType %in% other & bikeType %in% noBike){
    networkLinks$facility[i] = "nb_oth"
  }else if (bikeType %in% bikeBoul){
    networkLinks$facility[i] = "bb_all"
  }else if (bikeType %in% sepPath){
    networkLinks$facility[i] = "sp_all"
  }else if (bikeType %in% cycleTrack){
    networkLinks$facility[i] = "ct_all"
  }
  print(paste0("Classified link #",i," of ",n))  
  
}

networkLinks$speed[networkLinks$bikeTypeLong=="Bicycle Boulevard"]=20
networkLinks$speedCat[networkLinks$bikeTypeLong=="Bicycle Boulevard"] = "s_less.equal_20"
networkLinks$trafficCat[networkLinks$bikeTypeLong=="Bicycle Boulevard"] = "t_less5k"
networkLinks$speedCat[is.na(networkLinks$speed) & networkLinks$bikeTypeLong %in% bikeTypes[c(cycleTrack,bikeBoul,sepPath)]]= "s_less.equal_20"
networkLinks$trafficCat[is.na(networkLinks$traffic) & networkLinks$bikeTypeLong %in% bikeTypes[c(cycleTrack,bikeBoul,sepPath)]]= "t_less5k"

write.csv(networkLinks,"source_data/modelNetwork/networkLink_Attributes.csv",row.names = FALSE)

networkLinks = read.csv("source_data/modelNetwork/networkLink_Attributes.csv")

###Adding traffic calming data to links 
trafficCalm = readOGR("source_data/gis_data/City\ Level/Portland/PortlandTrafficCalming","Traffic_Calming_pdx")
calmData = trafficCalm@data

##TCTYPES
#3180 = Speedhump
#3820 = Curb Extension
#3830 = Enhanced Crossing
#3840 = warning hatching

speedHumps = trafficCalm[trafficCalm$TCTYPE==3810,]
speedHumpBuff = gBuffer(speedHumps,width=10,byid = TRUE)
speedHumpBuff = spTransform(speedHumpBuff, CRS("+init=epsg:4326"))
modelNetwork = spTransform(modelNetwork, CRS("+init=epsg:4326"))
networkLines = as(modelNetwork,"SpatialLines")
networkLinks$speedHump=FALSE

for (i in 1:nrow(speedHumpBuff)){
  humpBuff = speedHumpBuff[i,]
  #hump = speedHumps[i,]
  overResult =  which(!is.na(over(networkLines, as(humpBuff,"SpatialPolygons"))))
  linksIn = as.numeric(overResult)
#   if(length(linksIn)>1){
#     subNetwork = modelNetwork[linksIn,]
#     subNetwork = spTransform(subNetwork,CRS("+init=epsg:2913"))
#     point = spTransform(hump, subNetwork@proj4string)
#     dists = vector()
#     for (j in 1:length(linksIn)){
#       dists[j]=gDistance(point,subNetwork[j,])
#     }
#     nearestLink = subNetwork$psuid[which(min(dists)==dists)[1]]
#   }else if (length(linksIn)==1){
#     nearestLink=netAtts$psuid[linksIn]
#   }else{
#     nearestLink=NA
#   }
  networkLinks$speedHump[networkLinks$psuid %in% linksIn]=TRUE
  print(paste0("Found link for speed hump ",i," of ",nrow(speedHumpBuff)))
}

write.csv(networkLinks,"source_data/modelNetwork/networkLink_Attributes.csv",row.names = FALSE)

networkLinks = read.csv("source_data/modelNetwork/networkLink_Attributes.csv")

##Adding Intersection control info to links

intData= read.csv("source_data/modelNetwork/stop_signal2013.csv")
newNetLinks = join(networkLinks,intData,by="psuid")

write.csv(newNetLinks,"source_data/modelNetwork/networkLink_Attributes.csv",row.names = FALSE)

