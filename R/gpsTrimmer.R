#' GPS Trimmer
#'
#' Trims GPS traces for ORcycle trips from both beginning and end of trip by a given distance (in miles)
#'
#' @param rawCoords Raw coordinate table in same format as 'coord' table in SQL database.
#' @param trimDist Distance to trim from origin and destination of trip. 
#'
#' @return Trimmed coordinate table
#'
#' @examples
#' gpsTrimmer(coords,0.05)
#'
#' @export

##Load in neccesary libraries
##require(geosphere)

##Function definition
gpsTrimmer = function (rawCoords,trimDist){
  require(geosphere)
  
  tids = sort(unique(rawCoords$trip_id))
  tripList = list()
  for (i in 1:length(tids)){
    tid = tids[i]
    tripCoords = list()
    tripCoords[["trip_id"]]=tid
    subCoords = subset(rawCoords, rawCoords$trip_id==tid)
    start = subCoords[1,c("longitude","latitude")]
    end = subCoords[nrow(subCoords),c("longitude","latitude")]
    
    ###trim from front
    flag = FALSE
    fDist = 0
    j = 2
    while(fDist<trimDist){
      testPoint = subCoords[j,c("longitude","latitude")]
      if(sum(is.na(testPoint))==0 & nrow(testPoint)>0){
        fDist = distMeeus(start,testPoint)/1609.34
        j = j+1
      }else{
        flag = TRUE
        break
      }
    }
    if(!flag){
      frontTrimCoords = subCoords[j:nrow(subCoords),]
      
      ##trim from back
      bFlag = FALSE
      bDist = 0
      j = nrow(frontTrimCoords)-1
      while(bDist<trimDist){
        testPoint = frontTrimCoords[j,c("longitude","latitude")]
        if(sum(is.na(testPoint))==0 & nrow(testPoint)>0){
          bDist = distMeeus(end,testPoint)/1609.34
          j = j-1
        }else{
          bFlag=TRUE
          break
        }
      }
      if(!bFlag){
        trimmedCoords = frontTrimCoords[1:j,]
        tripCoords[["trimmedCoords"]]=trimmedCoords
      }else{
        tripCoords[["trimmedCoords"]]=NA
      }
    }else{
      tripCoords[["trimmedCoords"]]=NA
    }
    
    tripList[[i]]=tripCoords
    
    print(paste("Trimmed trip ",i," of ",length(tids)))
  }
  
  rowCount = 0
  for (i in 1:length(tripList)){
    trimmedCoords = tripList[[i]][["trimmedCoords"]]
    if(!is.null(nrow(trimmedCoords))){
      rowCount = rowCount+ nrow(trimmedCoords)
    }
  }
  
  newCoords = data.frame(matrix(nrow=rowCount,ncol=ncol(coords)))
  colnames(newCoords)=colnames(coords)
  
  ri=1
  for(i in 1:length(tripList)){
    trimmedCoords = tripList[[i]][["trimmedCoords"]]
    if(!is.null(nrow(trimmedCoords))){
      newCoords[ri:(ri+nrow(trimmedCoords)-1),]= trimmedCoords
      ri = ri + nrow(trimmedCoords)
    }
    print(paste("pasted trip ",i," of ",length(tripList)))
  }
  
  return(newCoords)
}