#' Prepare trip data table
#'
#' Proccesses all trip-related SQL data into a single flat table for further exploratory and analysis tasks. 
#'
#' @param trips 'trip' table from SQL database
#' @param tripResponses 'tripResponse' table from SQL database
#' @param answers 'answer' table from SQL database 
#' @param questions 'question' table from SQL database 
#' @param qaMap 'question_answer_relate' table from SQL database 
#' @param coords 'coord' table from SQL database 
#'
#' @return Flat trip table for further exploration/analysis. 
#'
#' @export
#' 
tripPrep = function(trips,tripResponses,answers,questions,qaMap,coords){
  
  require(geosphere)
  
  cols = c("trip_id","cumLength","duration","avgSpeed", "routeFreq", "purpose","routeComfort","routeStress_NA","routeStress_1","routeStress_2","routeStress_3",
           "routeStress_4","routeStress_5","routeStress_6","routeStress_7","routeStress_8","routePref_NA","routePref_1",
           "routePref_2","routePref_3","routePref_4","routePref_5","routePref_6","routePref_7","routePref_8",
           "routePref_9","routePref_10","routePref_11","routePref_12")
  tripList = trips$id
  tripModelTab = data.frame(matrix(nrow=length(tripList),ncol = length(cols)))
  colnames(tripModelTab)=cols
  tripModelTab$trip_id=tripList
  s_qids = c(19,20,22)
  single = cols[5:7]
  m_qids=c(21,27)
  multiple=cols[8:length(cols)]
  offsets = c(103-18,143-9)
  ordered = c(T,F,T)
  tripResponses = subset(tripResponses,tripResponses$answer_id !=0)
  
  for (j in 1:length(s_qids)){
    qid = s_qids[j]
    var = single[j]
    possibleAnswers = subset(qaMap,qaMap$question_id == qid)$answer_id
    levels = subset(answers,answers$id %in% possibleAnswers)$text
    levels = levels[2:length(levels)]
    for (i in 1:nrow(tripModelTab)){
      tid = tripModelTab$trip_id[i]
      if (qid %in% subset(tripResponses,tripResponses$trip_id == tid)$question_id){
        response = subset(tripResponses, tripResponses$trip_id == tid & tripResponses$question_id == qid)
        text = answers$text[answers$id == response$answer_id]
        if(identical(text,"no data")){
          tripModelTab[i,var] = NA
        }else{
          tripModelTab[i,var] = text
        } 
      }else{
        tripModelTab[i,var] = NA
      }
    }
    tripModelTab[,var] = factor(tripModelTab[,var],levels = levels, ordered = ordered[j])
  }
  
  tripModelTab[,multiple]=FALSE
  
  for (j in 1:length(m_qids)){
    qid = m_qids[j]
    off = offsets[j]
    for (i in 1:nrow(tripModelTab)){
      tid = tripModelTab$trip_id[i]
      response = subset(tripResponses, tripResponses$trip_id == tid & tripResponses$question_id == qid)
      if (nrow(response)>0){
        adj = response$answer_id - off
        tripModelTab[i,adj]=TRUE
      }else{
        if(qid ==21){
          tripModelTab[i,17]=TRUE
        }else if(qid==27){
          tripModelTab[i,8]=TRUE
        }
      }
    }
  }
  
  startTime = Sys.time()
  n = nrow(tripModelTab)
  print("Beginning trip statistics calculation")
  for(i in 1:nrow(tripModelTab)){
    tid = tripModelTab$trip_id[i]
    tripCoords = subset(coords,coords$trip_id == tid)
    tripCoords = subset(tripCoords,tripCoords$latitude !=0 &tripCoords$longitude!=0)
    cumLength = 0
    if(nrow(tripCoords)>2){
      for (j in 1:(nrow(tripCoords)-1)){
        coord1 = c(tripCoords$longitude[j],tripCoords$latitude[j])
        coord2 = c(tripCoords$longitude[j+1],tripCoords$latitude[j+1])
        cumLength = cumLength + (distMeeus(p1 = coord1, p2 = coord2))*0.000621371
      }
      start = tripCoords$recorded[1]
      end = tripCoords$recorded[nrow(tripCoords)]
      duration = as.numeric(difftime(end,start, units = "mins"))
      tripModelTab$cumLength[i] = cumLength
      tripModelTab$duration[i] = duration
      tripModelTab$avgSpeed[i] = cumLength/(duration/60)
    }
    now = Sys.time()
    print(paste("Finished calculation for trip # ",i,"of ",n,", ",round(as.numeric(difftime(now,startTime,units = "mins")),2)," minutes so far"))
  }
  return(tripModelTab)
}
    
    
    
    
  