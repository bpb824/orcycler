#######
# Report Analysis Script
# Written by Bryan Blanc
#######

###Load neccesary libraries
library(DBI)
library(RMySQL)
library(knitr)
library(htmlwidgets)

###If loading these libraries causes errors, the libraries must be installed on your machine
###Installing RMySQL needs special steps, google for instructions


###Set Working directory to ORcycle Analysis Tool Suite root folder
###fill in string in setwd() function with where you've stored the folder in your file structure
sysName = Sys.info()["sysname"]
if (sysName == "Linux"){
  setwd("//stash/bikeapp/ORcycle_Analysis_Tool_Suite")
}else{
  setwd("/Users/bblanc/OneDrive/_BikeAppProject/ORcycle_Analysis_Tool_Suite")
}

###Read in db authentication info
dbInfo = readLines("db_info.txt")
user_name = substr(dbInfo[9],12,nchar(dbInfo[9]))
password = substr(dbInfo[10],11,nchar(dbInfo[10]))
db_name = substr(dbInfo[11],11,nchar(dbInfo[11]))
host = substr(dbInfo[12],7,nchar(dbInfo[7]))

####Connect to ORcycle db
####Make sure you VPN into cecs network 
con <- dbConnect(dbDriver("MySQL"), host=host, port= 3306, user=user_name, password = password, dbname=db_name)

###Read in tables needed for user analyses
users = dbReadTable(con,"user")
#trips = dbReadTable(con, "trip")
notes = dbReadTable(con, "note")
noteResponses =dbReadTable(con, "noteResponse")
questions = dbReadTable(con, "questions")
answers = dbReadTable(con,"answers")
qaMap = dbReadTable(con,"question_answer_relate")

flagged = users$id[users$flagUser ==1]
users = subset(users,!(users$id %in% flagged))
notes = subset(notes,!(notes$user_id %in% flagged))
noteResponses = subset(noteResponses, noteResponses$note_id %in% notes$id)

###Combine useful data from db tables into one table for exploration/modeling/graphing
source("functions/reportPrep.R")
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
source("functions/report_barPlots.R")
report_barPlots(crashSummary,reportType = 0)
report_barPlots(issueSummary,reportType = 1)

if (sysName != "Linux"){
  ###Download report photos to ORcycle Analysis workspace
  ### Make sure you are connected to bikeapp server
  source("functions/downloadReportPhotos.R")
  downloadReportPhotos(remotePath = "/Volumes/bikeapp/website/Web/Internal/notePhotos"
                       ,localPath = "working_data/reportPhotos")
  
  ###Sync local report photo repository with publicly hosted dropbox folder
  source("functions/syncPhotos.R")
  syncPhotos(localPath="working_data/reportPhotos")
}


###Create interactive map of reports
source("functions/reportMapper.R")
map = reportMapper(notes,issueSummary,crashSummary,answers,qaMap)
if(sysName == "Linux"){
  saveWidget(map,file = "//stash/bikeapp/website/Web/Internal/reportMap.html")
}else{
  saveWidget(map,file = "/Users/bblanc/OneDrive/_BikeAppProject/ORcycle_Analysis_Tool_Suite/results/reportMap.html")
}


###Attach geographic data to notes
source("functions/attach_ReportGeoData.R")
crashSummary = attach_ReportGeoData(crashSummary,notes)
issueSummary = attach_ReportGeoData(issueSummary,notes)

##Attach data sourced from manual inspection on Google Maps
source("functions/attach_GoogleData.R")
crashSummary = attach_GoogleData(crashSummary,sheetPath = "working_data/reportGoogleData/crashSheet.csv")
issueSummary = attach_GoogleData(issueSummary,sheetPath = "working_data/reportGoogleData/issueSheet.csv")

source("functions/trafficStress.R")
crashSummary = trafficStress(crashSummary)
issueSummary = trafficStress(issueSummary)

##Save out Report Model Table
date = substr(as.character(Sys.time()),1,10)
saveRDS(crashSummary,paste0("working_data/reportModelData/preparedCrashModelTab_",date,".csv"))
saveRDS(issueSummary,paste0("working_data/reportModelData/preparedIssueModelTab_",date,".csv"))

# source("functions/saveOutReports.R")
# saveOutReports(crashSummary = crashSummary,
#                issueSummary=issueSummary,
#                notes=notes,
#                saveFolder = "working_data/reportGoogleData")



