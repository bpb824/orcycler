library(RCurl)
library(downloader)
library(XML)
library(devtools)
#install_github("duncantl/ROAuth")
#install_github("karthik/rdrop2")
library(ROAuth)
library(rdrop2)
library(dplyr)

syncPhotos = function(localPath){
  existingRemote = drop_dir("publicHost/reportPhotos")
  remoteFiles = substr(existingRemote$path,nchar("//publicHost/reportPhotos/"),nchar(existingRemote$path))
  
  localFiles = list.files("working_data/reportPhotos")
  compare = !(localFiles %in% remoteFiles)
  if (sum(compare>0)){
    newFiles = localFiles[which(compare)]
    for (i in 1:length(newFiles)){
      new = newFiles[i]
      fromPath = paste0(localPath,"/",new)
      toPath = paste0("publicHost/reportPhotos")
      drop_upload(fromPath,toPath)
    }
  }
  
}

