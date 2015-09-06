#' Prepare user data table
#'
#' Proccesses all user-related SQL data into a single flat table for further exploratory and analysis tasks. 
#'
#' @param users 'user' table from SQL database
#' @param userResponses 'userResponse' table from SQL database
#' @param answers 'answer' table from SQL database 
#' @param questions 'question' table from SQL database 
#' @param qaMap 'question_answer_relate' table from SQL database 
#' @param trips 'trip' table from SQL database 
#' @param notes 'note' table from SQL database 
#'
#' @return Flat user table for further exploration/analysis. 
#'
#' @export
#' 
userPrep = function(users,userResponses,answers,questions,qaMap,trips,notes){
  vars = c("user_id","numTrips","numReports","age", "gender", "ethnicity","occupation", "income","hhWorkers", "hhVehicles", "numBikes","bikeTypes", "cyclingFreq","cyclingWeather","riderAbility","riderType")
  modelVars = vars[2:length(vars)]
  qids = c(1,3,4,5,6,7,8,9,10,14,15,16,17)
  ordered = c(T,F,F,F,T,T,T,T,F,T,F,T,F)
  userList = users$id
  userModelTab = data.frame(matrix(nrow=length(userList),ncol = length(vars)))
  colnames(userModelTab)=vars
  userModelTab$user_id = userList
  userResponses = subset(userResponses,userResponses$answer_id !=0)
  
  for (i in 1:nrow(userModelTab)){
    uid = userModelTab$user_id[i]
    usersWithTrips = unique(trips$user_id)
    if (uid %in% usersWithTrips){
      userModelTab$numTrips[i] = sum(trips$user_id==uid)
    }else{
      userModelTab$numTrips[i] = 0
    }
    usersWithReports = unique(notes$user_id)
    if (uid %in% usersWithReports){
      userModelTab$numReports[i] = sum(notes$user_id==uid)
    }else{
      userModelTab$numReports[i] = 0
    }
    questionVars = vars[4:length(vars)]
    for (j in 1:length(questionVars)){
      qid = qids[j]
      var = questionVars[j]
      if(qid == 10){
        possibleAnswers = subset(qaMap,qaMap$question_id == qid)$answer_id
        if (qid %in% subset(userResponses,userResponses$user_id == uid)$question_id){
          response = subset(userResponses, userResponses$user_id == uid & userResponses$question_id == qid)
          text = answers$text[response$answer_id[1]]
          if (nrow(response)>1){
            for (k in 2:nrow(response)){
              text = paste(text,answers$text[response$answer_id[k]],sep = ", ")
            }
          }
          if(identical(text,"no data")){
            userModelTab[i,var] = NA
          }else{
            userModelTab[i,var] = text
          } 
        }else{
          userModelTab[i,var] = NA
        }
      }else{
        possibleAnswers = subset(qaMap,qaMap$question_id == qid)$answer_id
        levels = subset(answers,answers$id %in% possibleAnswers)$text
        levels = levels[2:length(levels)]
        
        if (qid %in% subset(userResponses,userResponses$user_id == uid)$question_id){
          response = subset(userResponses, userResponses$user_id == uid & userResponses$question_id == qid)
          text = answers$text[answers$id == response$answer_id]
          if(identical(text,"no data") || length(text)==0){
            userModelTab[i,var] = NA
          }else{
            userModelTab[i,var] = text
          } 
        }else{
          userModelTab[i,var] = NA
        }
      }
    }
  }
  for (i in 1:length(questionVars)){
    var = questionVars[i]
    qid = qids[i]
    toOrder = ordered[i]
    if (qid !=10){
      possibleAnswers = subset(qaMap,qaMap$question_id == qid)$answer_id
      levels = subset(answers,answers$id %in% possibleAnswers)$text
      levels = levels[2:length(levels)]
      userModelTab[,var] = factor(userModelTab[,var],levels = levels, ordered = toOrder)
    }
  }
  return(userModelTab)
}
