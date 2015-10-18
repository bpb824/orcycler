#' User Analysis
#'
#' Initializes database connection and calls series of functions used to analyze ORcycle user data.
#'
#' @param db String indicating which db to use, default is "test", also can be "release"
#' @param db_path Absolute or relative path to db_credentials.json file.
#'
#' @return User analysis results
#'
#' @export

userAnalysis = function(db = "test", db_path = "source_data/db_credentials.json"){

  ###Read in db authentication info
  cred = rjson::fromJSON(file=db_path)
  if(db =="test"){
    db_cred = cred$db_credentials$test
  }else if(db=="release"){
    db_cred = cred$db_credentials$release
  }else{
    stop("db must be 'test' or 'release'")
  }

  ####Connect to ORcycle db
  ####Make sure you VPN into cecs network
  print("Connecting to database...")
  con <- DBI::dbConnect(RMySQL::MySQL(), host=db_cred$db_host, port= 3306, user=db_cred$db_user, password = db_cred$db_pass, dbname=db_cred$db_name)
  
  ###Read in tables needed for user analyses
  users = DBI::dbReadTable(con,"user")
  userResponses = DBI::dbReadTable(con,"userResponse")
  trips = DBI::dbReadTable(con, "trip")
  notes = DBI::dbReadTable(con, "note")
  questions = DBI::dbReadTable(con, "questions")
  answers = DBI::dbReadTable(con,"answers")
  qaMap = DBI::dbReadTable(con,"question_answer_relate")
  print("Downloaded all tables, preparing results...")

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

  if(!dir.exists("results")){
    dir.create("results")
  }
  if(!dir.exists(paste0("results/",db))){
    dir.create(paste0("results/",db))
  }
  if(!dir.exists(paste0("results/",db,"/plots_singleVar"))){
    dir.create(paste0("results/",db,"/plots_singleVar"))
  }
  if(!dir.exists(paste0("results/",db,"/plots_singleVar/users"))){
    dir.create(paste0("results/",db,"/plots_singleVar/users"))
  }

  ####Plot single variable distribution plots to results/{db version}/plots_singleVar/users, overwrite old results
  user_barPlots(userSummary,outDir =paste0("results/",db,"/plots_singleVar/users/") )
  print(paste0("Output user plots to ",paste0("results/",db,"/plots_singleVar/users/")))

  ####Impute missing survey responses
  print("Imputing missing responses...")
  userSumImp = imputeResponses(data=userSummary,impCols = c(4:11,13:16))

  ####Add household vehicles/workers ratio
  userSumImp$veh2wrkRatio = as.numeric(userSumImp$hhVehicles)/as.numeric(userSumImp$hhWorkers)

  if(!dir.exists("working_data")){
    dir.create("working_data")
  }
  if(!dir.exists(paste0("working_data/",db))){
    dir.create(paste0("working_data/",db))
  }
  if(!dir.exists(paste0("working_data/",db,"/userModelData"))){
    dir.create(paste0("working_data/",db,"/userModelData"))
  }

  date = substr(as.character(Sys.time()),1,10)
  saveRDS(userSumImp,paste0("working_data/",db,"/userModelData","/preparedUserModelTab_",date,".rds"))
  print(paste0("Saved out model table to ",paste0("working_data/",db,"/userModelData","/preparedUserModelTab_",date,".rds")))
  ###Crosstab plots
  #crossTabPlot(userSumImp,"riderType","income","Rider Type","Household Income",brewer.pal(8,"Blues"))
  #crossTabPlot(userSumImp,"riderType","gender","Rider Type","Gender",brewer.pal(3,"Set1"))
}

