#' Attach Data for Reports Extracted from Google Maps
#'
#' @param reportSummary Report summary data frame to join google map info to.
#' @param sheetPath Absolute or relative path to google map data (.csv file)
#'
#' @return Report summary data frame with google map data attached
#' @export
attach_GmapReportData = function(reportSummary,sheetPath){
  googleData = read.csv(sheetPath,stringsAsFactors = FALSE)
  for (i in 1:nrow(reportSummary)){
    nid = reportSummary$note_id[i]
    if (nid %in% googleData$note_id){
      nearIntersect = googleData$nearIntersect[googleData$note_id==nid]
      if(length(nearIntersect)==0|is.na(nearIntersect)){
        reportSummary$nearIntersect[i]=NA
      }else{
        reportSummary$nearIntersect[i] = as.logical(nearIntersect)
      }
      numLegs = googleData$numLegs[googleData$note_id==nid]
      if (numLegs =="#N/A" | nchar(numLegs)==0|is.na(numLegs)){
        reportSummary$numLegs[i]= NA
      }else{
        reportSummary$numLegs[i]= as.numeric(numLegs)
      }
      intType = googleData$intType[googleData$note_id ==nid]
      if (intType == "#N/A" | nchar(intType)==0|is.na(intType)){
        reportSummary$intType[i]=NA
      }else{
        reportSummary$intType[i] = as.numeric(intType)
      }
      numLanesAuto = googleData$numLanesAuto[googleData$note_id==nid]
      if(numLanesAuto == "#N/A" |length(numLanesAuto)==0|is.na(numLanesAuto)){
        reportSummary$numLanesAuto[i] = NA
      }else{
        reportSummary$numLanesAuto[i] = as.numeric(numLanesAuto)
      }
      bikeBox = googleData$bikeBox[googleData$note_id==nid]
      if(bikeBox == "#N/A" |length(bikeBox)==0|is.na(bikeBox)){
        reportSummary$bikeBox[i]=NA
      }else{
        reportSummary$bikeBox[i] = as.logical(bikeBox)
      }
      twoWay = googleData$twoWay[googleData$note_id==nid]
      if(twoWay == "#N/A" |length(twoWay)==0){
        reportSummary$twoWay[i]=NA
      }else{
        reportSummary$twoWay[i]=as.logical(twoWay)
      }
      adjParking = googleData$adjParking[googleData$note_id==nid]
      if(adjParking == "#N/A" |length(adjParking)==0){
        reportSummary$adjParking[i]=NA
      }else{
        if(adjParking == "Yes"){
          reportSummary$adjParking[i]=TRUE
        }else if(adjParking=="No"){
          reportSummary$adjParking[i]=FALSE
        }
      }
      centerLine = googleData$centerLine[googleData$note_id==nid]
      if(centerLine == "#N/A" |length(centerLine)==0){
        reportSummary$centerLine[i]=NA
      }else{
        reportSummary$centerLine[i]=as.logical(centerLine)
      }
    }else{
      reportSummary[i,c("nearIntersect","numLegs","intType","numLanesAuto","bikeBox","twoWay","adjParking","centerLine")]=NA
    }
  }
  return(reportSummary)
}
