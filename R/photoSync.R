#' Synchronize Photos in Local Repository with Dropbox Folder
#'
#' This will synchronize the photos between a local repository folder in the `orcycler` environment and the dropbox folder where report photos will be hosted.
#'
#' @param localPath Absolute or Relative Path to local repository
#' @param dropPath Path to dropbox photo repository relative to dropbox root folder
#'
#' @return None
#' @export
syncPhotos = function(localPath,dropPath){
  existingRemote = rdrop2::drop_dir(dropPath)
  remoteFiles = substr(existingRemote$path,nchar(paste0("//",dropPath)),nchar(existingRemote$path))

  localFiles = list.files(localPath)
  compare = !(localFiles %in% remoteFiles)
  if (sum(compare>0)){
    newFiles = localFiles[which(compare)]
    for (i in 1:length(newFiles)){
      new = newFiles[i]
      fromPath = paste0(localPath,"/",new)
      toPath = paste0(localPath)
      rdrop2::drop_upload(fromPath,toPath)
    }
  }

}

#' Download Report Photos
#'
#' Sync report photos from remote stash folder to local photo repository in the `orcycler` environment.
#'
#' @param remotePath The remote path from which to download photos. This requires a server connection.
#' @param localPath The local path to sync the remote photo repository with.
#'
#' @return None
#' @export
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
