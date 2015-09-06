downloadReportPhotos = function(remotePath,localPath){
  files = list.files(remotePath)
  notePhotos = files[grepl("note_image",files)]
  existingPhotos = list.files(localPath)
  for (i in 1:length(notePhotos)){
    photo = notePhotos[i]
    fromPath = paste0(remotePath,"/",photo)
    toPath = paste0(localPath,"/",photo)
    if (sum(grepl(photo,existingPhotos))==0){
      file.copy(from=fromPath,to=toPath)
    }
  }
}