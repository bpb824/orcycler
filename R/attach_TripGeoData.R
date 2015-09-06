#' Attach trip geographic data
#'
#' Use attributed network to add geographic data to trip summary table.
#'
#' @param tripSummary Trip summary table
#' @param matchLinksPath Absolute or relative path to map-matched trip links. 
#' @param matchTripsPath Absolute or relative path to map-matched trips. 
#' @param linkType Parameter used to determine if matched links are from ORcycle trips or map-matched directions from a Directions API (Google or MapBox)
#'
#' @return Trip table with geographic data attached. 
#'
#' @export
#' 

attach_TripGeoData = function(tripSummary,matchLinksPath,matchTripsPath,linkType = "orcycle"){
  
  #require(rgdal)
  #require(foreign)
  
  matchedLinks = read.csv(matchLinksPath,stringsAsFactors = FALSE)
  colnames(matchedLinks)[colnames(matchedLinks)=="featureid"]="psuid"
  modelNetwork = readOGR("source_data/modelNetwork","wbnet2012")
  modelNetData = modelNetwork@data
  netAtts = read.csv("source_data/modelNetwork/networkLink_Attributes.csv")
  netLines = modelNetwork@lines
  coordSys = modelNetwork@proj4string
  
  matchedTrips = sort(unique(matchedLinks$gpstripid))
  tripSummary$matched = FALSE
  tripSummary$matched[tripSummary$trip_id %in% matchedTrips] = TRUE
  
  orc_gpstrips= read.csv(matchTripsPath)
  
  if(linkType=="orcycle"){
    for (i in 1:nrow(tripSummary)){
      tid = tripSummary$trip_id[i]
      tripStages = subset(orc_gpstrips,orc_gpstrips$gpstripid==tid)
      tripStages$weightedSpeed = tripStages$avgspeed*tripStages$pointdistance
      totalDist = sum(tripStages$pointdistance)
      tripSummary$avgLinkSpeed[i] = sum(tripStages$weightedSpeed)/totalDist
      tripSummary$matchedDistance[i] = totalDist
      tripSummary$matchedDuration[i] = sum(tripStages$duration)
    }
  }else{
    for (i in 1:nrow(tripSummary)){
      tid = tripSummary$trip_id[i]
      tripStages = subset(orc_gpstrips,orc_gpstrips$gpstripid==tid)
      totalDist = sum(tripStages$pointdistance)
      tripSummary$matchedDistance[i] = totalDist
    }
  }
  
  ####Bike and Street Type Mileage
  sumDistBikeType = function(tripLinks,bikeType){
    return(sum(tripLinks$length_m[tripLinks$facility==bikeType])*0.000621371)
  }
  for (i in 1:nrow(tripSummary)){
    if (tripSummary$matched[i]==TRUE){
      tid = tripSummary$trip_id[i]
      tripLinksRaw = subset(matchedLinks,matchedLinks$gpstripid == tid)
      tripLinks = join(tripLinksRaw,netAtts,by="psuid")
      tripSummary$nb_primArt[i] = sumDistBikeType(tripLinks,"nb_primArt")
      tripSummary$nb_minArt[i] = sumDistBikeType(tripLinks,"nb_minArt")
      tripSummary$nb_res[i] = sumDistBikeType(tripLinks,"nb_res")
      tripSummary$nb_other[i] = sumDistBikeType(tripLinks,"nb_other")
      tripSummary$bl_primArt[i] = sumDistBikeType(tripLinks,"bl_primArt")
      tripSummary$bl_minArt[i] = sumDistBikeType(tripLinks,"bl_minArt")
      tripSummary$bl_res[i] = sumDistBikeType(tripLinks,"bl_res")
      tripSummary$bl_other[i] = sumDistBikeType(tripLinks,"bl_other")
      tripSummary$bb_all[i] = sumDistBikeType(tripLinks,"bb_all")
      tripSummary$ct_all[i] = sumDistBikeType(tripLinks,"ct_all")
      tripSummary$sp_all[i] = sumDistBikeType(tripLinks,"sp_all")
    }else{
      tripSummary$nb_primArt[i] = NA
      tripSummary$nb_minArt[i] = NA
      tripSummary$nb_res[i] = NA
      tripSummary$nb_other[i] = NA
      tripSummary$bl_primArt[i] =NA
      tripSummary$bl_minArt[i] = NA
      tripSummary$bl_res[i] = NA
      tripSummary$bl_other[i] = NA
      tripSummary$bb_all[i] = NA
      tripSummary$ct_all[i] = NA
      tripSummary$sp_all[i] = NA
    }
    print(paste0("Calculated bike mileage for trip ",i," of ",nrow(tripSummary)))
  }
    
    
    ####Traffic Volume Mileage
  sumDistTrafficType = function(tripLinks,trafficType){
    return(sum(tripLinks$length_m[tripLinks$trafficCat==trafficType])*0.000621371)
  }
  for (i in 1:nrow(tripSummary)){
    if (tripSummary$matched[i]==TRUE){
      tid = tripSummary$trip_id[i]
      tripLinksRaw = subset(matchedLinks,matchedLinks$gpstripid == tid)
      tripLinks = join(tripLinksRaw,netAtts,by="psuid")
      tripSummary$t_less5k[i] = sumDistTrafficType(tripLinks,"t_less5k")
      tripSummary$t_5to10k[i] = sumDistTrafficType(tripLinks,"t_5to10k")
      tripSummary$t_10to20k[i] = sumDistTrafficType(tripLinks,"t_10to20k")
      tripSummary$t_20to30k[i] = sumDistTrafficType(tripLinks,"t_20to30k")
      tripSummary$t_greater30k[i] = sumDistTrafficType(tripLinks,"t_greater30k")
    }else{
      tripSummary$t_less5k[i] = NA
      tripSummary$t_5to10k[i] = NA
      tripSummary$t_10to20k[i] = NA
      tripSummary$t_20to30k[i] = NA
      tripSummary$t_greater30k[i] = NA
    }
    
    print(paste0("Calculated traffic mileage for trip ",i," of ",nrow(tripSummary)))
  }
    
    ####Traffic Speed Mileage
  sumDistSpeedType = function(tripLinks,speedType){
    return(sum(tripLinks$length_m[tripLinks$speedCat==speedType])*0.000621371)
  }
  for (i in 1:nrow(tripSummary)){
    if (tripSummary$matched[i]==TRUE){
      tid = tripSummary$trip_id[i]
      tripLinksRaw = subset(matchedLinks,matchedLinks$gpstripid == tid)
      tripLinks = join(tripLinksRaw,netAtts,by="psuid")
      tripSummary$s_less.equal_20[i] = sumDistSpeedType(tripLinks,"s_less.equal_20")
      tripSummary$s_20to35[i] = sumDistSpeedType(tripLinks,"s_20to35")
      tripSummary$s_greater35[i] = sumDistSpeedType(tripLinks,"s_greater35")
    }else{
      tripSummary$s_less.equal_20[i] = NA
      tripSummary$s_20to35[i] = NA
      tripSummary$s_greater35[i] = NA
    }
    
    print(paste0("Calculated speed mileage for trip ",i," of ",nrow(tripSummary)))
  }
    
  
  ####Segment Grade Mileage
  sumDistGradeType = function(tripLinks,gradeType){
    return(sum(tripLinks$length_m[tripLinks$slopeCat==gradeType])*0.000621371)
  }
  for (i in 1:nrow(tripSummary)){
    if (tripSummary$matched[i]==TRUE){
      tid = tripSummary$trip_id[i]
      tripLinksRaw = subset(matchedLinks,matchedLinks$gpstripid == tid)
      tripLinks = join(tripLinksRaw,netAtts,by="psuid")
      tripLinks$slope = tripLinks$slope*tripLinks$direction*-1
      tripLinks$slopeCat = NA
      tripLinks$slopeCat[tripLinks$slope<=-2]="slp1"
      tripLinks$slopeCat[tripLinks$slope>-2 & tripLinks$slope <=2]="slp2"
      tripLinks$slopeCat[tripLinks$slope>2 & tripLinks$slope <=4]="slp3"
      tripLinks$slopeCat[tripLinks$slope>4 & tripLinks$slope <=6]="slp4"
      tripLinks$slopeCat[tripLinks$slope>6]="slp5"
      tripSummary$slp1[i] = sumDistGradeType(tripLinks,"slp1")
      tripSummary$slp2[i] = sumDistGradeType(tripLinks,"slp2")
      tripSummary$slp3[i] = sumDistGradeType(tripLinks,"slp3")
      tripSummary$slp4[i] = sumDistGradeType(tripLinks,"slp4")
      tripSummary$slp5[i] = sumDistGradeType(tripLinks,"slp5")
    }else{
      tripSummary$slp1[i] = NA
      tripSummary$slp2[i] = NA
      tripSummary$slp3[i] = NA
      tripSummary$slp4[i] = NA
      tripSummary$slp5[i] = NA
    }
    print(paste0("Calculated grade mileage for trip ",i," of ",nrow(tripSummary)))
  }
  
  ###Intersection Data
  for (i in 1:nrow(tripSummary)){
    if (tripSummary$matched[i]==TRUE){
      tid = tripSummary$trip_id[i]
      tripLinksRaw = subset(matchedLinks,matchedLinks$gpstripid == tid)
      tripLinks = join(tripLinksRaw,netAtts,by="psuid")
      tripLinks$stopSign = FALSE
      tripLinks$signal = FALSE
      tripLinks$stopSign[tripLinks$stop2013 == "ft" | (tripLinks$stop2013 == "t" & tripLinks$direction ==1)]=TRUE
      tripLinks$signal[tripLinks$signal2013 == "ft" | (tripLinks$signal2013 == "t" & tripLinks$direction ==1)]=TRUE
      tripSummary$numStopSigns[i]= sum(tripLinks$stopSign)
      tripSummary$numSignals[i]=sum(tripLinks$signal)
    }
    print(paste0("Fetched intersection control for trip ",i," of ",nrow(tripSummary)))
  }
  tripSummary$stopsPerMile = tripSummary$numStopSigns/tripSummary$matchedDistance
  tripSummary$signalsPerMile = tripSummary$numSignals/tripSummary$matchedDistance
   
  
  ##Speed Hump Link Mileage
  
  for (i in 1:nrow(tripSummary)){
    if (tripSummary$matched[i]==TRUE){
      tid = tripSummary$trip_id[i]
      tripLinksRaw = subset(matchedLinks,matchedLinks$gpstripid == tid)
      tripLinks = join(tripLinksRaw,netAtts,by="psuid")
      tripSummary$humpMiles[i]=sum(tripLinks$speedHump*tripLinks$length_m*0.000621371)
      tripSummary$numHumps[i]=sum(tripLinks$speedHump)
    }
    print(paste0("Fetched traffic calming for trip ",i," of ",nrow(tripSummary)))
  }
  
    ####Calculate number of turns for a trip
  for (i in 1:nrow(tripSummary)){
    if (tripSummary$matched[i]==TRUE){
      tid = tripSummary$trip_id[i]
      #linkRows = as.numeric(sapply(slot(modelNetwork, "lines"), function(x) slot(x, "ID")))+1
      tripLinks = subset(matchedLinks,matchedLinks$gpstripid ==tid)
      tripLinks = tripLinks[order(tripLinks$featureorder),]
      #turnFrame = as.data.frame(matrix(nrow = nrow(tripLinks)-1, ncol = 4))
      #colnames(turnFrame) = c("fromLink","toLink","angle","position")
      numTurns = 0
      numLefts = 0
      numRights = 0
      if (nrow(tripLinks)>1){
        for (j in 1:(nrow(tripLinks)-1)){
          currentLink_id =tripLinks$psuid[j]
          nextLink_id = tripLinks$psuid[j+1]
          #turnFrame$fromLink[j] = currentLink_id
          #turnFrame$toLink[j]=nextLink_id
          clRow = which(modelNetData$psuid == currentLink_id)
          nlRow = which(modelNetData$psuid == nextLink_id)
          clCoords = netLines[clRow][[1]]@Lines[[1]]@coords
          nlCoords = netLines[nlRow][[1]]@Lines[[1]]@coords
          a = c(clCoords[nrow(clCoords),1]-clCoords[nrow(clCoords)-1,1],clCoords[nrow(clCoords),2]-clCoords[nrow(clCoords)-1,2])*tripLinks$direction[j]
          b = c(nlCoords[nrow(nlCoords),1]-nlCoords[nrow(nlCoords)-1,1],nlCoords[nrow(nlCoords),2]-nlCoords[nrow(nlCoords)-1,2])*tripLinks$direction[j+1]
          theta <- acos( sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )*180/pi
          
          Bx = clCoords[nrow(clCoords),1]
          Ax = clCoords[1,1]
          Ay = clCoords[1,2]
          By = clCoords[nrow(clCoords),2]
          if (tripLinks$direction[j+1]==1){
            Cx = nlCoords[nrow(nlCoords),1]
            Cy = nlCoords[nrow(nlCoords),2]
          }else{
            Cx = nlCoords[1,1]
            Cy = nlCoords[1,2]
          }
          
          position = sign( (Bx-Ax)*(Cy-Ay) - (By-Ay)*(Cx-Ax) )
          #turnFrame$position[j] = position
          #turnFrame$angle[j] = theta
          if (theta>=30 & !is.nan(theta)){
            numTurns = numTurns +1
            
            if (position ==1){
              numRights = numRights +1
            }else if (position == -1){
              numLefts=numLefts+1
            }
            
          }
          #turnLinkShape = SpatialLines(netLines[c(clRow,nlRow)],coordSys)
          #plot(turnLinkShape)
          #linkRows = which(modelNetData$psuid %in% tripLinks$featureid)
          #tripLinkShape = SpatialLines(netLines[linkRows],coordSys)
          #plot(tripLinkShape)
        }
        tripSummary$numTurns[i] = numTurns
        tripSummary$numRights[i]=numRights
        tripSummary$numLefts[i]=numLefts
      }
    }else{
      tripSummary$numTurns[i] = NA
      tripSummary$numRights[i]=NA
      tripSummary$numLefts[i]=NA
    }
    print(paste0("Calculated turn numbers for trip ",i," of ",nrow(tripSummary)))
  }
   

  return(tripSummary)
}