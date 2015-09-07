#' Subset data by geographic boundary
#'
#' @param con Database connection object
#' @param boundShape Geographc subsetting boundary (SpatialPolygons object)
#' @param folderName Name of folder to output
#' @param startDate Start date of subset in YYYY-MM-DD format
#' @param endDate End date of subset in YYYY-MM-DD format
#' @param privacy Boolean indicating whether to include user IDs (not included if TRUE)
#' @param include_qa Boolean indicating if question answer dictionaries will be included in output folder
#'
#' @return None
#' @export

subDataByGeoBound= function(con,boundShape,folderName,startDate,endDate,privacy = FALSE,include_qa=TRUE){

  ###Read in tables needed for user analyses
  users = DBI::dbReadTable(con,"user")
  userResponses = DBI::dbReadTable(con,"userResponse")
  trips = DBI::dbReadTable(con, "trip")
  tripResponses = DBI::dbReadTable(con, "tripResponse")
  notes = DBI::dbReadTable(con, "note")
  noteResponses = DBI::dbReadTable(con, "noteResponse")
  coords = DBI::dbReadTable(con, "coord")
  questions = DBI::dbReadTable(con, "questions")
  answers = DBI::dbReadTable(con,"answers")
  qaMap = DBI::dbReadTable(con,"question_answer_relate")

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
  sp::coordinates(points)=~X+Y
  points@proj4string = sp::CRS("+init=epsg:4326")

  ##notes
  notePoints = data.frame(matrix(ncol=3,nrow = nrow(notes)))
  colnames(notePoints) = c("id","X","Y")
  notePoints$id = notes$id
  notePoints$X = notes$longitude
  notePoints$Y = notes$latitude
  sp::coordinates(notePoints)=~X+Y
  notePoints@proj4string = sp::CRS("+init=epsg:4326")

  ##transform boundary shape to WGS 84
  boundShape = sp::spTransform(boundShape,CRS("+init=epsg:4326"))

  #retrieve intersection of note points and geo boundary
  intersect = rgeos::gIntersection(notePoints,boundShape, byid = c(TRUE,FALSE))
  if(!is.null(intersect)){
    rows = as.numeric(rownames(intersect@coords))
    subNotes = notes[rows,]
    subNoteResponses = noteResponses[noteResponses$note_id %in% subNotes$id,]

  }else{
    subNotes=NA
    subNoteResponses=NA
  }

  #retrieve intersection of trip coords and boundary
  intersect = sp::over(as(points,"SpatialPoints"),as(boundShape,"SpatialPolygons"))
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
