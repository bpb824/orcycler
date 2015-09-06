if (!require("packrat")) install.packages("packrat")
library(packrat)
packrat::install_github("rstudio/leaflet")
library(leaflet)
library(plyr)
library(shapefiles)
library(sp)
library(rgdal)
library(ggplot2)
library(RColorBrewer)
library(magrittr)
library(RCurl)
library(downloader)
library(jpeg)
library(adimpro)
library(ROAuth)
library(rdrop2)
library(dplyr)

reportMapper = function(notes,issueSummary,crashSummary,answers,qaMap,outFileName,photoPath="working_data/reportPhotos"){
  
  sysName = Sys.info()["sysname"]
  if (sysName == "Linux"){
    setwd("//stash/bikeapp/ORcycle_Analysis_Tool_Suite")
    crashSummary$notes = subset(notes,notes$id %in% crashSummary$note_id)$details
    issueSummary$notes = subset(notes,notes$id %in% issueSummary$note_id)$details
    crashSummary$date = subset(notes,notes$id %in% crashSummary$note_id)$details
    issueSummary$date = subset(notes,notes$id %in% issueSummary$note_id)$details
    for (i in 1:nrow(crashSummary)){
      nid =crashSummary$note_id[i]
      if(nchar(notes$reportDate[notes$id==nid])==0){
        crashSummary$reportDate[i]=substr(notes$recorded[notes$id==nid],1,10)
      }else{
        crashSummary$reportDate[i]=notes$reportDate[notes$id==nid]
      }
    }
    for (i in 1:nrow(issueSummary)){
      nid =issueSummary$note_id[i]
      if(nchar(notes$reportDate[notes$id==nid])==0){
        issueSummary$reportDate[i]=substr(notes$recorded[notes$id==nid],1,10)
      }else{
        issueSummary$reportDate[i]=notes$reportDate[notes$id==nid]
      }
    }
    crashSummary$sevNum = as.numeric(crashSummary$severity)
    issueSummary$urgNum = as.numeric(issueSummary$urgency)
    
    cols = colnames(crashSummary)
    conflictCols = grepl("conflict",cols)
    conflict = cbind(crashSummary$note_id,crashSummary[,conflictCols])
    colnames(conflict)[1]="note_id"
    answerText =c("No data",answers[answers$id %in% qaMap[qaMap$question_id==29,"answer_id"],"text"])
    for (i in 1:nrow(conflict)){
      row = conflict[i,]
      ans = row[,2:length(row)]
      if (ans[,1]){
        text = "No data"
      }else{
        text = ""
        text = answerText[2]
        for (j in 3:ncol(ans)){
          if (ans[,j]){
            text = paste(text,answerText[j],sep=", ")
          }
        }
      }
      crashSummary$cflictText[i]=text
    }
    
    actionCols = grepl("action",cols)
    action = cbind(crashSummary$note_id,crashSummary[,actionCols])
    colnames(action)[1]="note_id"
    answerText =c("No data",answers[answers$id %in% qaMap[qaMap$question_id==32,"answer_id"],"text"])
    for (i in 1:nrow(action)){
      row = action[i,]
      ans = row[,2:length(row)]
      if (ans[,1]){
        text = "No data"
      }else{
        text = ""
        text = answerText[2]
        for (j in 3:ncol(ans)){
          if (ans[,j]){
            text = paste(text,answerText[j],sep=", ")
          }
        }
      }
      crashSummary$actnText[i]=text
    }
    
    reasonCols = grepl("reason",cols)
    reason = cbind(crashSummary$note_id,crashSummary[,reasonCols])
    colnames(reason)[1]="note_id"
    answerText =c("No data",answers[answers$id %in% qaMap[qaMap$question_id==33,"answer_id"],"text"])
    for (i in 1:nrow(reason)){
      row = reason[i,]
      ans = row[,2:length(row)]
      if (ans[,1]){
        text = "No data"
      }else{
        text = ""
        text = answerText[2]
        for (j in 3:ncol(ans)){
          if (ans[,j]){
            text = paste(text,answerText[j],sep=", ")
          }
        }
      }
      crashSummary$rsnText[i]=text
    }
    
    cols = colnames(issueSummary)
    issueCols = grepl("issue",cols)
    issue = cbind(issueSummary$note_id,issueSummary[,issueCols])
    colnames(issue)[1]="note_id"
    answerText =c("No data",answers[answers$id %in% qaMap[qaMap$question_id==30,"answer_id"],"text"])
    for (i in 1:nrow(issue)){
      row = issue[i,]
      ans = row[,2:length(row)]
      if (ans[,1]){
        text = "No data"
      }else{
        text = ""
        text = answerText[2]
        for (j in 3:ncol(ans)){
          if (ans[,j]){
            text = paste(text,answerText[j],sep=", ")
          }
        }
      }
      issueSummary$issText[i]=text
    }
    
    
    #   photoList = drop_dir("publicHost/reportPhotos")
    #   for (i in 1:nrow(photoList)){
    #     share = drop_share(photoList$path[i],short_url = FALSE)
    #     photoList$link[i] = share$url
    #     photoList$link[i]=gsub(x = photoList$link[i],pattern="https://www.dropbox.com",replacement="https://dl.dropboxusercontent.com")
    #     photoList$note_id[i] = as.numeric(substr(photoList$path[i],nchar("//publicHost/reportPhotos/note_image_"),nchar(photoList$path[i])-4))
    #   }
    
    photos = list.files("//stash/bikeapp/website/Web/Internal/notePhotos")
    photos = photos[grepl("note_image_",photos)]
    ids = gsub("note_image_","",photos)
    ids = as.numeric(gsub(".jpg","",ids))
    photoList = as.data.frame(matrix(nrow = length(ids),ncol = 2))
    colnames(photoList) =c("note_id","path")
    photoList$note_id = ids
    photoList$path = photos
    photoList = photoList[order(photoList$note_id),]
    photoPath = "//stash/bikeapp/website/Web/Internal/notePhotos"
    photoWeb = "http://orcycle2.cecs.pdx.edu/Web/Internal/notePhotos"
    
    issueSummary$photo_url=NA
    issueSummary$link = NA
    issueSummary$landscape= FALSE
    for (i in 1:nrow(issueSummary)){
      nid = issueSummary$note_id[i]
      if (nid %in% photoList$note_id){
        pic <- readJPEG(paste0(photoPath,"/note_image_",nid,".jpg"))
        image = make.image(pic)
        height = image[["dim"]][1]
        width = image[["dim"]][2]
        if(width>height){
          issueSummary$landscape[i] = TRUE
        }
        issueSummary$photo_url[i]=photoList$path[photoList$note_id==nid]
        issueSummary$link[i] = paste0(photoWeb,"/note_image_",nid,".jpg")
      }
    }
    crashSummary$photo_url=NA
    crashSummary$link = NA
    crashSummary$landscape= FALSE
    for (i in 1:nrow(crashSummary)){
      nid = crashSummary$note_id[i]
      if (nid %in% photoList$note_id){
        pic <- readJPEG(paste0(photoPath,"/note_image_",nid,".jpg"))
        image = make.image(pic)
        height = image[["dim"]][1]
        width = image[["dim"]][2]
        if(width>height){
          crashSummary$landscape[i] = TRUE
        }
        crashSummary$photo_url[i]=photoList$path[photoList$note_id==nid]
        crashSummary$link[i] = paste0(photoWeb,"/note_image_",nid,".jpg")
      }
    }
    
    points = as.data.frame(cbind(notes$id,notes$longitude,notes$latitude))
    colnames(points)=c("note_id","X","Y")
    crashPoints = subset(points,points$note_id %in% crashSummary$note_id)
    issuePoints = subset(points,points$note_id %in% issueSummary$note_id)
    coordinates(crashPoints)=~X+Y
    crashPoints@proj4string = CRS("+init=epsg:4326")
    crashShape = SpatialPointsDataFrame(crashPoints,crashSummary)
    coordinates(issuePoints)=~X+Y
    issuePoints@proj4string = CRS("+init=epsg:4326")
    issueShape = SpatialPointsDataFrame(issuePoints,issueSummary)
    
    icons = as.data.frame(list.files("//stash/bikeapp/website/Web/Internal/reportIcons"))
    colnames(icons)="file"
    iconWeb = "http://orcycle2.cecs.pdx.edu/Web/Internal/reportIcons/"
    icons$link = paste0(iconWeb,icons$file)
    legendLink = icons$link[grepl("Legend",icons$link)]
    
    link = icons$link[grepl("BlueCrash",icons$link)]
    sevIcon_1=makeIcon(iconUrl = link,iconWidth = 38,iconHeight = 33)
    link = icons$link[grepl("GreenCrash",icons$link)]
    sevIcon_2=makeIcon(iconUrl = link,iconWidth = 38,iconHeight = 33)
    link = icons$link[grepl("YellowCrash",icons$link)]
    sevIcon_3=makeIcon(iconUrl = link,iconWidth = 38,iconHeight = 33)
    link = icons$link[grepl("OrangeCrash",icons$link)]
    sevIcon_4=makeIcon(iconUrl = link,iconWidth = 38,iconHeight = 33)
    link = icons$link[grepl("RedCrash",icons$link)]
    sevIcon_5=makeIcon(iconUrl = link,iconWidth = 38,iconHeight = 33)
    sevPopHTML = vector()
    for (i in 1:nrow(crashSummary)){
      if(!is.na(crashSummary$link[i])){
        if (crashSummary$landscape[i]){
          sevPopHTML[i]= paste0("<p><strong>Date: </strong>",crashShape$reportDate[i],"</p>",
                                "<p><strong>Severity: </strong>",crashShape$severity[i],"</p>",
                                "<p><strong>Conflicts: </strong>",crashShape$cflictText[i],"</p>",
                                "<p><strong>Reasons: </strong>",crashShape$rsnText[i],"</p>",
                                "<p><strong>Actions: </strong>",crashShape$actnText[i],"</p>",
                                "<p><strong>Details: </strong>",crashShape$notes[i],"</p>",
                                paste0("<div align='center'><img src='",crashShape$link[i],"' alt='ReportPhoto' height='160' width='240'></div>"))
        }else{
          sevPopHTML[i]=paste0("<p><strong>Date: </strong>",crashShape$reportDate[i],"</p>",
                               "<p><strong>Severity: </strong>",crashShape$severity[i],"</p>",
                               "<p><strong>Conflicts: </strong>",crashShape$cflictText[i],"</p>",
                               "<p><strong>Reasons: </strong>",crashShape$rsnText[i],"</p>",
                               "<p><strong>Actions: </strong>",crashShape$actnText[i],"</p>",
                               "<p><strong>Details: </strong>",crashShape$notes[i],"</p>",
                               paste0("<div align='center'><img src='",crashShape$link[i],"' alt='ReportPhoto' height='240' width='160'></div>"))
        }
      }else{
        sevPopHTML[i]= paste0("<p><strong>Date: </strong>",crashShape$reportDate[i],"</p>",
                              "<p><strong>Severity: </strong>",crashShape$severity[i],"</p>",
                              "<p><strong>Conflicts: </strong>",crashShape$cflictText[i],"</p>",
                              "<p><strong>Reasons: </strong>",crashShape$rsnText[i],"</p>",
                              "<p><strong>Actions: </strong>",crashShape$actnText[i],"</p>",
                              "<p><strong>Details: </strong>",crashShape$notes[i],"</p>")
      }
      
    }
    sevPops =  sevPopHTML
    
    link = icons$link[grepl("glyph-blue",icons$link)]
    urgIcon_1=makeIcon(iconUrl = link,iconWidth = 46,iconHeight = 27)
    link = icons$link[grepl("glyph-green",icons$link)]
    urgIcon_2=makeIcon(iconUrl = link,iconWidth = 46,iconHeight = 27)
    link = icons$link[grepl("glyph-yellow",icons$link)]
    urgIcon_3=makeIcon(iconUrl = link,iconWidth = 46,iconHeight = 27)
    link = icons$link[grepl("glyph-orange",icons$link)]
    urgIcon_4=makeIcon(iconUrl = link,iconWidth = 46,iconHeight = 27)
    link = icons$link[grepl("glyph-red",icons$link)]
    urgIcon_5=makeIcon(iconUrl = link,iconWidth = 46,iconHeight = 27)
    urgPopHTML = vector()
    for (i in 1:nrow(issueSummary)){
      if(!is.na(issueSummary$link[i])){
        if (issueSummary$landscape[i]){
          urgPopHTML[i]= paste0("<p><strong>Date: </strong>",issueShape$reportDate[i],"</p>",
                                "<p><strong>Urgency: </strong>",issueShape$urgency[i],"</p>",
                                "<p><strong>Issue Type: </strong>",issueShape$issText[i],"</p>",
                                "<p><strong>Details: </strong>",issueShape$notes[i],"</p>",
                                paste0("<div align='center'><img src='",issueShape$link[i],"' alt='ReportPhoto' height='160' width='240'></div>"))
        }else{
          urgPopHTML[i]=paste0("<p><strong>Date: </strong>",issueShape$reportDate[i],"</p>",
                               "<p><strong>Urgency: </strong>",issueShape$urgency[i],"</p>",
                               "<p><strong>Issue Type: </strong>",issueShape$issText[i],"</p>",
                               "<p><strong>Details: </strong>",issueShape$notes[i],"</p>",
                               paste0("<div align='center'><img src='",issueShape$link[i],"' alt='ReportPhoto' height='240' width='160'></div>"))
        }
      }else{
        urgPopHTML[i]= paste0("<p><strong>Date: </strong>",issueShape$reportDate[i],"</p>",
                              "<p><strong>Urgency: </strong>",issueShape$urgency[i],"</p>",
                              "<p><strong>Issue Type: </strong>",issueShape$issText[i],"</p>",
                              "<p><strong>Details: </strong>",issueShape$notes[i],"</p>")
      }
    }
    urgPops = urgPopHTML
    
    leg_html = paste0("<div class='my-legend'><div class='legend-title'>Report Icons</div><div class='legend-scale'><img src='",legendLink,"' alt='Legend' style='width:260px;height:220px'></div><div class='legend-source'>Data: <a href='http://www.pdx.edu/transportation-lab/orcycle'>ORcycle</a></div></div>")
    
    m = leaflet() %>% addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}",attribution = leg_html) %>% setView(-122.654422, 45.5424364, zoom = 10)
    
    m = m %>% addMarkers(data = crashShape[crashShape$sevNum==1,],popup = sevPops[crashShape$sevNum==1],icon = sevIcon_1) %>%
      addMarkers(data = crashShape[crashShape$sevNum==2,],popup = sevPops[crashShape$sevNum==2],icon = sevIcon_2)%>% 
      addMarkers(data = crashShape[crashShape$sevNum==3,],popup = sevPops[crashShape$sevNum==3],icon = sevIcon_3)%>% 
      addMarkers(data = crashShape[crashShape$sevNum==4,],popup = sevPops[crashShape$sevNum==4],icon = sevIcon_4)%>% 
      addMarkers(data = crashShape[crashShape$sevNum==5,],popup = sevPops[crashShape$sevNum==5],icon = sevIcon_5)%>%
      addMarkers(data = issueShape[issueShape$urgNum==1,],popup = urgPops[issueShape$urgNum==1],icon = urgIcon_1)%>%
      addMarkers(data = issueShape[issueShape$urgNum==2,],popup = urgPops[issueShape$urgNum==2],icon = urgIcon_2)%>%
      addMarkers(data = issueShape[issueShape$urgNum==3,],popup = urgPops[issueShape$urgNum==3],icon = urgIcon_3)%>%
      addMarkers(data = issueShape[issueShape$urgNum==4,],popup = urgPops[issueShape$urgNum==4],icon = urgIcon_4)%>%
      addMarkers(data = issueShape[issueShape$urgNum==5,],popup = urgPops[issueShape$urgNum==5],icon = urgIcon_5)
    
    return(m)
    
    
  }else{
    setwd("/Users/bblanc/OneDrive/_BikeAppProject/ORcycle_Analysis_Tool_Suite")
    
    crashSummary$notes = subset(notes,notes$id %in% crashSummary$note_id)$details
    issueSummary$notes = subset(notes,notes$id %in% issueSummary$note_id)$details
    crashSummary$date = subset(notes,notes$id %in% crashSummary$note_id)$details
    issueSummary$date = subset(notes,notes$id %in% issueSummary$note_id)$details
    for (i in 1:nrow(crashSummary)){
      nid =crashSummary$note_id[i]
      if(nchar(notes$reportDate[notes$id==nid])==0){
        crashSummary$reportDate[i]=substr(notes$recorded[notes$id==nid],1,10)
      }else{
        crashSummary$reportDate[i]=notes$reportDate[notes$id==nid]
      }
    }
    for (i in 1:nrow(issueSummary)){
      nid =issueSummary$note_id[i]
      if(nchar(notes$reportDate[notes$id==nid])==0){
        issueSummary$reportDate[i]=substr(notes$recorded[notes$id==nid],1,10)
      }else{
        issueSummary$reportDate[i]=notes$reportDate[notes$id==nid]
      }
    }
    crashSummary$sevNum = as.numeric(crashSummary$severity)
    issueSummary$urgNum = as.numeric(issueSummary$urgency)
    
    cols = colnames(crashSummary)
    conflictCols = grepl("conflict",cols)
    conflict = cbind(crashSummary$note_id,crashSummary[,conflictCols])
    colnames(conflict)[1]="note_id"
    answerText =c("No data",answers[answers$id %in% qaMap[qaMap$question_id==29,"answer_id"],"text"])
    for (i in 1:nrow(conflict)){
      row = conflict[i,]
      ans = row[,2:length(row)]
      if (ans[,1]){
        text = "No data"
      }else{
        text = ""
        text = answerText[2]
        for (j in 3:ncol(ans)){
          if (ans[,j]){
            text = paste(text,answerText[j],sep=", ")
          }
        }
      }
      crashSummary$cflictText[i]=text
    }
    
    actionCols = grepl("action",cols)
    action = cbind(crashSummary$note_id,crashSummary[,actionCols])
    colnames(action)[1]="note_id"
    answerText =c("No data",answers[answers$id %in% qaMap[qaMap$question_id==32,"answer_id"],"text"])
    for (i in 1:nrow(action)){
      row = action[i,]
      ans = row[,2:length(row)]
      if (ans[,1]){
        text = "No data"
      }else{
        text = ""
        text = answerText[2]
        for (j in 3:ncol(ans)){
          if (ans[,j]){
            text = paste(text,answerText[j],sep=", ")
          }
        }
      }
      crashSummary$actnText[i]=text
    }
    
    reasonCols = grepl("reason",cols)
    reason = cbind(crashSummary$note_id,crashSummary[,reasonCols])
    colnames(reason)[1]="note_id"
    answerText =c("No data",answers[answers$id %in% qaMap[qaMap$question_id==33,"answer_id"],"text"])
    for (i in 1:nrow(reason)){
      row = reason[i,]
      ans = row[,2:length(row)]
      if (ans[,1]){
        text = "No data"
      }else{
        text = ""
        text = answerText[2]
        for (j in 3:ncol(ans)){
          if (ans[,j]){
            text = paste(text,answerText[j],sep=", ")
          }
        }
      }
      crashSummary$rsnText[i]=text
    }
    
    cols = colnames(issueSummary)
    issueCols = grepl("issue",cols)
    issue = cbind(issueSummary$note_id,issueSummary[,issueCols])
    colnames(issue)[1]="note_id"
    answerText =c("No data",answers[answers$id %in% qaMap[qaMap$question_id==30,"answer_id"],"text"])
    for (i in 1:nrow(issue)){
      row = issue[i,]
      ans = row[,2:length(row)]
      if (ans[,1]){
        text = "No data"
      }else{
        text = ""
        text = answerText[2]
        for (j in 3:ncol(ans)){
          if (ans[,j]){
            text = paste(text,answerText[j],sep=", ")
          }
        }
      }
      issueSummary$issText[i]=text
    }
    issueSummary$photo_url=NA
    issueSummary$landscape= FALSE
    
    photoList = drop_dir("publicHost/reportPhotos")
    for (i in 1:nrow(photoList)){
      share = drop_share(photoList$path[i],short_url = FALSE)
      photoList$link[i] = share$url
      photoList$link[i]=gsub(x = photoList$link[i],pattern="https://www.dropbox.com",replacement="https://dl.dropboxusercontent.com")
      photoList$note_id[i] = as.numeric(substr(photoList$path[i],nchar("//publicHost/reportPhotos/note_image_"),nchar(photoList$path[i])-4))
    }
    for (i in 1:nrow(issueSummary)){
      nid = issueSummary$note_id[i]
      if (nid %in% photoList$note_id){
        pic <- readJPEG(paste0(photoPath,"/note_image_",nid,".jpg"))
        image = make.image(pic)
        height = image[["dim"]][1]
        width = image[["dim"]][2]
        if(width>height){
          issueSummary$landscape[i] = TRUE
        }
        issueSummary$photo_url[i]=photoList$link[photoList$note_id==nid]
      }
    }
    crashSummary$photo_url=NA
    crashSummary$landscape= FALSE
    for (i in 1:nrow(crashSummary)){
      nid = crashSummary$note_id[i]
      if (nid %in% photoList$note_id){
        pic <- readJPEG(paste0(photoPath,"/note_image_",nid,".jpg"))
        image = make.image(pic)
        height = image[["dim"]][1]
        width = image[["dim"]][2]
        if(width>height){
          crashSummary$landscape[i] = TRUE
        }
        crashSummary$photo_url[i]=photoList$link[photoList$note_id==nid]
      }
    }
    
    points = as.data.frame(cbind(notes$id,notes$longitude,notes$latitude))
    colnames(points)=c("note_id","X","Y")
    crashPoints = subset(points,points$note_id %in% crashSummary$note_id)
    issuePoints = subset(points,points$note_id %in% issueSummary$note_id)
    coordinates(crashPoints)=~X+Y
    crashPoints@proj4string = CRS("+init=epsg:4326")
    crashShape = SpatialPointsDataFrame(crashPoints,crashSummary)
    coordinates(issuePoints)=~X+Y
    issuePoints@proj4string = CRS("+init=epsg:4326")
    issueShape = SpatialPointsDataFrame(issuePoints,issueSummary)
    
    file_list = drop_dir("publicHost")
    icons = subset(file_list,file_list$mime_type=="image/png")
    for (i in 1:nrow(icons)){
      share = drop_share(icons$path[i],short_url = FALSE)
      icons$link[i] = share$url
      icons$link[i]=gsub(x=icons$link[i],pattern="https://www.dropbox.com",replacement="https://dl.dropboxusercontent.com")
    }
    legendLink = icons$link[grepl("Legend",icons$link)]
    
    
    link = icons$link[grepl("BlueCrash",icons$link)]
    sevIcon_1=makeIcon(iconUrl = link,iconWidth = 38,iconHeight = 33)
    link = icons$link[grepl("GreenCrash",icons$link)]
    sevIcon_2=makeIcon(iconUrl = link,iconWidth = 38,iconHeight = 33)
    link = icons$link[grepl("YellowCrash",icons$link)]
    sevIcon_3=makeIcon(iconUrl = link,iconWidth = 38,iconHeight = 33)
    link = icons$link[grepl("OrangeCrash",icons$link)]
    sevIcon_4=makeIcon(iconUrl = link,iconWidth = 38,iconHeight = 33)
    link = icons$link[grepl("RedCrash",icons$link)]
    sevIcon_5=makeIcon(iconUrl = link,iconWidth = 38,iconHeight = 33)
    sevPopHTML = vector()
    for (i in 1:nrow(crashSummary)){
      if(!is.na(crashSummary$photo_url[i])){
        if (crashSummary$landscape[i]){
          sevPopHTML[i]= paste0("<p><strong>Date: </strong>",crashShape$reportDate[i],"</p>",
                                "<p><strong>Severity: </strong>",crashShape$severity[i],"</p>",
                                "<p><strong>Conflicts: </strong>",crashShape$cflictText[i],"</p>",
                                "<p><strong>Reasons: </strong>",crashShape$rsnText[i],"</p>",
                                "<p><strong>Actions: </strong>",crashShape$actnText[i],"</p>",
                                "<p><strong>Details: </strong>",crashShape$notes[i],"</p>",
                                paste0("<div align='center'><img src='",crashShape$photo_url[i],"' alt='ReportPhoto' height='160' width='240'></div>"))
        }else{
          sevPopHTML[i]=paste0("<p><strong>Date: </strong>",crashShape$reportDate[i],"</p>",
                               "<p><strong>Severity: </strong>",crashShape$severity[i],"</p>",
                               "<p><strong>Conflicts: </strong>",crashShape$cflictText[i],"</p>",
                               "<p><strong>Reasons: </strong>",crashShape$rsnText[i],"</p>",
                               "<p><strong>Actions: </strong>",crashShape$actnText[i],"</p>",
                               "<p><strong>Details: </strong>",crashShape$notes[i],"</p>",
                               paste0("<div align='center'><img src='",crashShape$photo_url[i],"' alt='ReportPhoto' height='240' width='160'></div>"))
        }
      }else{
        sevPopHTML[i]= paste0("<p><strong>Date: </strong>",crashShape$reportDate[i],"</p>",
                              "<p><strong>Severity: </strong>",crashShape$severity[i],"</p>",
                              "<p><strong>Conflicts: </strong>",crashShape$cflictText[i],"</p>",
                              "<p><strong>Reasons: </strong>",crashShape$rsnText[i],"</p>",
                              "<p><strong>Actions: </strong>",crashShape$actnText[i],"</p>",
                              "<p><strong>Details: </strong>",crashShape$notes[i],"</p>")
      }
      
    }
    sevPops =  sevPopHTML
    
    link = icons$link[grepl("glyph-blue",icons$link)]
    urgIcon_1=makeIcon(iconUrl = link,iconWidth = 46,iconHeight = 27)
    link = icons$link[grepl("glyph-green",icons$link)]
    urgIcon_2=makeIcon(iconUrl = link,iconWidth = 46,iconHeight = 27)
    link = icons$link[grepl("glyph-yellow",icons$link)]
    urgIcon_3=makeIcon(iconUrl = link,iconWidth = 46,iconHeight = 27)
    link = icons$link[grepl("glyph-orange",icons$link)]
    urgIcon_4=makeIcon(iconUrl = link,iconWidth = 46,iconHeight = 27)
    link = icons$link[grepl("glyph-red",icons$link)]
    urgIcon_5=makeIcon(iconUrl = link,iconWidth = 46,iconHeight = 27)
    urgPopHTML = vector()
    for (i in 1:nrow(issueSummary)){
      if(!is.na(issueSummary$photo_url[i])){
        if (issueSummary$landscape[i]){
          urgPopHTML[i]= paste0("<p><strong>Date: </strong>",issueShape$reportDate[i],"</p>",
                                "<p><strong>Urgency: </strong>",issueShape$urgency[i],"</p>",
                                "<p><strong>Issue Type: </strong>",issueShape$issText[i],"</p>",
                                "<p><strong>Details: </strong>",issueShape$notes[i],"</p>",
                                paste0("<div align='center'><img src='",issueShape$photo_url[i],"' alt='ReportPhoto' height='160' width='240'></div>"))
        }else{
          urgPopHTML[i]=paste0("<p><strong>Date: </strong>",issueShape$reportDate[i],"</p>",
                               "<p><strong>Urgency: </strong>",issueShape$urgency[i],"</p>",
                               "<p><strong>Issue Type: </strong>",issueShape$issText[i],"</p>",
                               "<p><strong>Details: </strong>",issueShape$notes[i],"</p>",
                               paste0("<div align='center'><img src='",issueShape$photo_url[i],"' alt='ReportPhoto' height='240' width='160'></div>"))
        }
      }else{
        urgPopHTML[i]= paste0("<p><strong>Date: </strong>",issueShape$reportDate[i],"</p>",
                              "<p><strong>Urgency: </strong>",issueShape$urgency[i],"</p>",
                              "<p><strong>Issue Type: </strong>",issueShape$issText[i],"</p>",
                              "<p><strong>Details: </strong>",issueShape$notes[i],"</p>")
      }
    }
    urgPops = urgPopHTML
    
    leg_html = paste0("<div class='my-legend'><div class='legend-title'>Report Icons</div><div class='legend-scale'><img src='",legendLink,"' alt='Legend' style='width:260px;height:220px'></div><div class='legend-source'>Data: <a href='http://www.pdx.edu/transportation-lab/orcycle'>ORcycle</a></div></div>")
    
    m = leaflet() %>% addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}",attribution = leg_html) %>% setView(-122.654422, 45.5424364, zoom = 10)
    
    m = m %>% addMarkers(data = crashShape[crashShape$sevNum==1,],popup = sevPops[crashShape$sevNum==1],icon = sevIcon_1) %>%
      addMarkers(data = crashShape[crashShape$sevNum==2,],popup = sevPops[crashShape$sevNum==2],icon = sevIcon_2)%>% 
      addMarkers(data = crashShape[crashShape$sevNum==3,],popup = sevPops[crashShape$sevNum==3],icon = sevIcon_3)%>% 
      addMarkers(data = crashShape[crashShape$sevNum==4,],popup = sevPops[crashShape$sevNum==4],icon = sevIcon_4)%>% 
      addMarkers(data = crashShape[crashShape$sevNum==5,],popup = sevPops[crashShape$sevNum==5],icon = sevIcon_5)%>%
      addMarkers(data = issueShape[issueShape$urgNum==1,],popup = urgPops[issueShape$urgNum==1],icon = urgIcon_1)%>%
      addMarkers(data = issueShape[issueShape$urgNum==2,],popup = urgPops[issueShape$urgNum==2],icon = urgIcon_2)%>%
      addMarkers(data = issueShape[issueShape$urgNum==3,],popup = urgPops[issueShape$urgNum==3],icon = urgIcon_3)%>%
      addMarkers(data = issueShape[issueShape$urgNum==4,],popup = urgPops[issueShape$urgNum==4],icon = urgIcon_4)%>%
      addMarkers(data = issueShape[issueShape$urgNum==5,],popup = urgPops[issueShape$urgNum==5],icon = urgIcon_5)
    
    return(m)
  }
}
