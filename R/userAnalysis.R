#' User Analysis
#'
#' Initializes database connection and calls series of functions used to analyze ORcycle user data. 
#'
#' @param db_info Absolute or relative path to db_info.txt file. 
#'
#' @return User analysis results
#'
#' @export

userAnalysis = function(db_info){
  ###Load neccesary libraries
  require(DBI)
  require(RMySQL)
  
  ###Read in db authentication info
  dbInfo = readLines(db_info)
  user_name = substr(dbInfo[9],12,nchar(dbInfo[9]))
  password = substr(dbInfo[10],11,nchar(dbInfo[10]))
  db_name = substr(dbInfo[11],11,nchar(dbInfo[11]))
  host = substr(dbInfo[12],7,nchar(dbInfo[7]))
  
  ####Connect to ORcycle db
  ####Make sure you VPN into cecs network 
  con <- dbConnect(dbDriver("MySQL"), host=host, port= 3306, user=user_name, password = password, dbname=db_name)
  
  ###Read in tables needed for user analyses
  users = dbReadTable(con,"user")
  userResponses = dbReadTable(con,"userResponse")
  trips = dbReadTable(con, "trip")
  notes = dbReadTable(con, "note")
  questions = dbReadTable(con, "questions")
  answers = dbReadTable(con,"answers")
  qaMap = dbReadTable(con,"question_answer_relate")
  
  flagged = users$id[users$flagUser==1]
  users = subset(users,!(users$id %in% flagged))
  trips = subset(trips,!(trips$user_id %in% flagged))
  notes = subset(notes,!(notes$user_id %in% flagged))
  userResponses = subset(userResponses,!(userResponses$user_id %in% flagged))
  
  ###Combine useful data from db tables into one table for exploration/modeling/graphing
  userSummary = userPrep(users=users,
                         userResponses = userResponses,
                         trips = trips,
                         notes = notes,
                         questions = questions,
                         answers = answers,
                         qaMap = qaMap)
  
  imUserSummary = userSummary[userSummary$user_id %in% imUsers,]
  
  if(!dir.exists("results")){
    dir.create("results")
  }
  if(!dir.exists("results/plots_singleVar")){
    dir.create("results/plots_singleVar")
  }
  if(!dir.exists("results/plots_singleVar/users")){
    dir.create("results/plots_singleVar/users")
  }
  
  ####Plot single variable distribution plots to results/plots_singleVar/users, overwrite old results
  user_barPlots(userSummary)
  
  ####Impute missing survey responses
  userSumImp = imputeResponses(data=userSummary,impCols = c(4:11,13:16))
  
  ####Add household vehicles/workers ratio
  userSumImp$veh2wrkRatio = as.numeric(userSumImp$hhVehicles)/as.numeric(userSumImp$hhWorkers)
  
  if(!dir.exists("working_data")){
    dir.create("working_data")
  }
  if(!dir.exists("working_data/userModelData")){
    dir.create("working_data/userModelData")
  }
  
  date = substr(as.character(Sys.time()),1,10)
  saveRDS(userSumImp,paste0("working_data/userModelData/preparedUserModelTab_",date,".rds"))
  
  ###Crosstab plots
  #crossTabPlot(userSumImp,"riderType","income","Rider Type","Household Income",brewer.pal(8,"Blues"))
  #crossTabPlot(userSumImp,"riderType","gender","Rider Type","Gender",brewer.pal(3,"Set1"))
}

