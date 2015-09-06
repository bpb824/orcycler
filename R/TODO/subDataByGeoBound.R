####Function to subset ORcycle data by geographic boundary shapefile
###Outputs an csv tables to results/databaseFiles/foldername

library(DBI)
library(RMySQL)
library(rgdal)
library(rgeos)
library(sp)
library(plyr)
library(Hmisc)
library(shapefiles)


subDataByGeoBound= function(boundShape,folderName,startDate,endDate,privacy = FALSE,include_qa=TRUE){
  
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
  userResponses = dbReadTable(con,"userResponse")
  trips = dbReadTable(con, "trip")
  tripResponses = dbReadTable(con, "tripResponse")
  notes = dbReadTable(con, "note")
  noteResponses = dbReadTable(con, "noteResponse")
  coords = dbReadTable(con, "coord")
  questions = dbReadTable(con, "questions")
  answers = dbReadTable(con,"answers")
  qaMap = dbReadTable(con,"question_answer_relate")
  
  trips$date = substr(trips$stop,1,10)
  trips = subset(trips,!is.na(trips$date))
  startDate = as.POSIXct(strptime(startDate,format = "%Y-%m-%d"))
  endDate = as.POSIXct(strptime(endDate,format = "%Y-%m-%d"))
  trips$date = as.POSIXct(strptime(trips$date,format = "%Y-%m-%d"))
  trips = subset(trips,trips$date>=startDate & trips$date<=endDate)
  notes$date = substr(notes$recorded,1,10)
  notes$date = as.POSIXct(strptime(notes$date,format = "%Y-%m-%d"))
  notes = subset(notes,notes$date>=startDate & notes$date<=endDate)
  
  tripUsers = unique(trips$user_id)
  noteUsers = unique(notes$user_id)
  allUsers = sort(unique(c(tripUsers,noteUsers)))
  users = subset(users,users$id %in% allUsers)
  
  userResponses = subset(userResponses,userResponses$user_id %in% users$id)
  tripResponses = subset(tripResponses,tripResponses$trip_id %in% trips$id)
  noteResponses = subset(noteResponses,noteResponses$note_id %in% notes$id)
  coords = subset(coords,coords$trip_id %in% trips$id)

  
  ###Get spatial file for both coords and notes 
  
  ##coords
  points = data.frame(matrix(ncol=4,nrow = nrow(coords)))
  colnames(points) = c("id","trip_id","X","Y")
  points$id = rownames(coords)
  points$trip_id = coords$trip_id
  points$X = coords$longitude
  points$Y = coords$latitude
  coordinates(points)=~X+Y
  points@proj4string = CRS("+init=epsg:4326")
  
  ##notes
  notePoints = data.frame(matrix(ncol=3,nrow = nrow(notes)))
  colnames(notePoints) = c("id","X","Y")
  notePoints$id = notes$id
  notePoints$X = notes$longitude
  notePoints$Y = notes$latitude
  coordinates(notePoints)=~X+Y
  notePoints@proj4string = CRS("+init=epsg:4326")
  
  ##transform boundary shape to WGS 84
  boundShape = spTransform(boundShape,CRS("+init=epsg:4326"))
  
  #retrieve intersection of note points and geo boundary
  intersect = gIntersection(notePoints,boundShape, byid = c(TRUE,FALSE))
  if(!is.null(intersect)){
    rows = as.numeric(rownames(intersect@coords))
    subNotes = notes[rows,]
    subNoteResponses = noteResponses[noteResponses$note_id %in% subNotes$id,]
    
  }else{
    subNotes=NA
    subNoteResponses=NA
  }
  
  #retrieve intersection of trip coords and boundary
  intersect = over(as(points,"SpatialPoints"),as(boundShape,"SpatialPolygons"))
  if(!is.null(intersect)){
    inside = !is.na(intersect)
    interiorCoords = coords[inside,]
    interiorTrips = sort(unique(interiorCoords$trip_id))
    subTrips = trips[trips$id %in%  interiorTrips,]
    subCoords = coords[coords$trip_id %in% interiorTrips,]
    subTripResponses = tripResponses[tripResponses$trip_id %in% subTrips$id,]
  }else{
    subTrips=NA
    subCoords=NA
    subTripResponses=NA
  }
  
  
  #Get subset users
  if(!is.na(subTrips)){
    tripUsers = unique(subTrips$user_id)
  }else{
    tripUsers=vector()
  }
  if(!is.na(subNotes)){
    noteUsers = unique(subNotes$user_id)
  }else{
    noteUsers=vector()
  }
 
  
  allUsers = sort(unique(c(tripUsers,noteUsers)))
  
  subUsers = users[users$id %in% allUsers,]
  subUserResponses = userResponses[userResponses$user_id %in% allUsers,]
  
  ###Privacy clause removes user_id's from trip and coord tables. 
  if(privacy){
    drop = colnames(trips)[c(2,8:19)]
    subTrips = subTrips[,colnames(subTrips)[!(colnames(subTrips) %in% drop)]]
    subCoords = subCoords[,colnames(subCoords)[colnames(subCoords) != "user_id"]]
  }
  
  dir = paste0("results/databaseFiles/",folderName)
  dir.create(dir)
  write.csv(subUsers,paste0(dir,"/user.csv"),row.names = FALSE)
  write.csv(subTrips,paste0(dir,"/trip.csv"),row.names = FALSE)
  write.csv(subNotes,paste0(dir,"/note.csv"),row.names = FALSE)
  write.csv(subUserResponses,paste0(dir,"/userResponse.csv"),row.names = FALSE)
  write.csv(subNoteResponses,paste0(dir,"/noteResponse.csv"),row.names = FALSE)
  write.csv(subTripResponses,paste0(dir,"/tripResponse.csv"),row.names = FALSE)
  write.csv(subCoords,paste0(dir,"/coord.csv"),row.names = FALSE)
  if(include_qa){
    write.csv(questions,paste0(dir,"/questions.csv"),row.names = FALSE)
    write.csv(answers,paste0(dir,"/answers.csv"),row.names = FALSE)
    write.csv(qaMap,paste0(dir,"/qaMap.csv"),row.names = FALSE)
  }
}