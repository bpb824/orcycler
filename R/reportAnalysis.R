#' Report Analysis
#'
#' Initializes database connection and calls series of functions used to analyze ORcycle trip data.
#'
#' @param db String indicating which db to use, default is "test", also can be "release"
#' @param db_path Relative or absolute file path to db_credentials.json
#'
#' @return None
#'
#' @export
reportAnalysis = function(db = "test", db_path = "data/db_credentials.json"){

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
  con <- DBI::dbConnect(dbDriver("MySQL"), host=db_cred$db_host, port= 3306, user=db_cred$db_user, password = db_cred$db_pass, dbname=db_cred$db_name)

  ###Read in tables needed for user analyses
  users = DBI::dbReadTable(con,"user")
  notes = DBI::dbReadTable(con, "note")
  noteResponses = DBI::dbReadTable(con, "noteResponse")
  questions = DBI::dbReadTable(con, "questions")
  answers = DBI::dbReadTable(con,"answers")
  qaMap = DBI::dbReadTable(con,"question_answer_relate")

  flagged = users$id[users$flagUser ==1]
  users = subset(users,!(users$id %in% flagged))
  notes = subset(notes,!(notes$user_id %in% flagged))
  noteResponses = subset(noteResponses, noteResponses$note_id %in% notes$id)

  ###Combine useful data from db tables into one table for exploration/modeling/graphing
  crashSummary = reportPrep(reportType =0,
                            users=users,
                            notes = notes,
                            noteResponses=noteResponses,
                            questions = questions,
                            answers = answers,
                            qaMap = qaMap)
  issueSummary = reportPrep(reportType =1,
                            users=users,
                            notes = notes,
                            noteResponses=noteResponses,
                            questions = questions,
                            answers = answers,
                            qaMap = qaMap)

  ####Plot single variable distribution plots to results/plots_singleVar/trips, overwrite old results
  if(!dir.exists("results")){
    dir.create("results")
  }
  if(!dir.exists("results/plots_singleVar")){
    dir.create("results/plots_singleVar")
  }
  if(!dir.exists("results/plots_singleVar/users")){
    dir.create("results/plots_singleVar/users")
  }

  report_barPlots(crashSummary,reportType = 0)
  report_barPlots(issueSummary,reportType = 1)

  ###Attach geographic data to notes
  crashSummary = attach_ReportGeoData(crashSummary,notes)
  issueSummary = attach_ReportGeoData(issueSummary,notes)

  ##Attach data sourced from manual inspection on Google Maps
  crashSummary = attach_GoogleData(crashSummary,sheetPath = "working_data/reportGoogleData/crashSheet.csv")
  issueSummary = attach_GoogleData(issueSummary,sheetPath = "working_data/reportGoogleData/issueSheet.csv")

  #Calculate Level of Traffic Stress
  crashSummary = trafficStress(crashSummary)
  issueSummary = trafficStress(issueSummary)

  ##Save out Report Model Table
  date = substr(as.character(Sys.time()),1,10)
  saveRDS(crashSummary,paste0("working_data/reportModelData/preparedCrashModelTab_",date,".rds"))
  saveRDS(issueSummary,paste0("working_data/reportModelData/preparedIssueModelTab_",date,".rds"))
}
