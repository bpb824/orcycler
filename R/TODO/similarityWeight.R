
similarityWeight = function(matchLinksPath,trips,tripMatchSummmary){
  orc_gpslinks = read.csv(matchLinksPath)
  matchedTrips= sort(unique(orc_gpslinks$gpstripid))
  trips = subset(trips,trips$id %in% matchedTrips)
  orc_gpslinks = subset(orc_gpslinks,orc_gpslinks$gpstripid %in% trips$id)
  for (i in 1:nrow(orc_gpslinks)){
    tid = orc_gpslinks$gpstripid[i]
    orc_gpslinks$user_id[i]=trips$user_id[trips$id == tid]
    orc_gpslinks$purpose[i]=trips$purpose[trips$id == tid]
  }
  orc_gpslinks$dir_linkid = orc_gpslinks$featureid*orc_gpslinks$direction
  userList = sort(unique(trips$user_id))
  userTripList = list()
  n = length(userList)
  for (i in 1:length(userList)){
    uid = userList[i]
    userObj= list()
    userObj["user_id"]=uid
    userTrips = sort(unique(trips$id[trips$user_id==uid]))
    if (length(userTrips >1)){
      tripMat = matrix(nrow=length(userTrips),ncol = length(userTrips))
      colnames(tripMat)=paste0("t",userTrips)
      rownames(tripMat)=paste0("t",userTrips)
      for (j in 1:length(userTrips)){
        for (k in 1:length(userTrips)){
          trip_a = userTrips[j]
          trip_b = userTrips[k]
          if (trips$purpose[trips$id ==trip_a]==trips$purpose[trips$id ==trip_b]){
            tripLinks_a = subset(orc_gpslinks,orc_gpslinks$gpstripid==trip_a)
            tripLinks_b = subset(orc_gpslinks,orc_gpslinks$gpstripid==trip_b)
            compare = tripLinks_b$dir_linkid %in% tripLinks_a$dir_linkid
            similarity = mean(compare)
            tripMat[j,k]= similarity
          }else{
            tripMat[j,k]=0
          }
        }
      }
    }else{
      tripMat = NA
    }
    userObj[["tripMatrix"]]=tripMat
    userTripList[[i]]=userObj
    print(paste0("Calculated similarity for user #",i," of ",n))
  }
  
  for (i in 1:nrow(tripMatchSummary)){
    tid = tripMatchSummary$trip_id[i]
    uid = trips$user_id[trips$id == tid]
    tripMat = userTripList[[which(userList==uid)]][["tripMatrix"]]
    sims = tripMat[paste0("t",tid),]
    sims = sims[names(sims)!=paste0("t",tid)]
    similar = sims[sims>0.9]
    nsim = length(similar)
    if(nsim>0){
      weight = 1/(nsim+1)
    }else{
      weight = 1
    }
    tripMatchSummary$similarWeight[i] = weight
  }
  return(tripMatchSummary)
}