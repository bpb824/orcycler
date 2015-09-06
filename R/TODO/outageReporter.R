sysName = Sys.info()["sysname"]
if (sysName == "Linux"){
  setwd(setwd("~/_ODOT_Portal/investigations"))
}else{
  setwd("/Users/bblanc/OneDrive/_Portal_ODOT/investigations/")
}

valueFlagReports = readRDS("valueFlagReports.rds")
