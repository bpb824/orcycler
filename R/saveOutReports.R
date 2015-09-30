#' Save Report Summary to Excel
#'
#' @param crashSummary Crash report summary table
#' @param issueSummary Issue report summary table
#' @param notes Note table from SQL database
#' @param saveFolder Relative or Absolute path to output folder
#'
#' @return None
#' @export
saveReportsToExcel=function(crashSummary,issueSummary,notes,saveFolder){
  crashSheet = data.frame(matrix(nrow=nrow(crashSummary),ncol = ncol(crashSummary)+3))
  colnames(crashSheet)=c("latitude","longitude","googleLink",colnames(crashSummary))
  crashSheet$latitude = notes$latitude[notes$id %in% crashSummary$note_id]
  crashSheet$longitude = notes$longitude[notes$id %in% crashSummary$note_id]
  crashSheet$googleLink = paste0("http://maps.google.com/maps?q=",crashSheet$latitude,",",crashSheet$longitude,"&ll=",crashSheet$latitude,",",crashSheet$longitude,"&z=16")
  crashSheet[,colnames(crashSummary)]=crashSummary

  issueSheet = data.frame(matrix(nrow=nrow(issueSummary),ncol = ncol(issueSummary)+3))
  colnames(issueSheet)=c("latitude","longitude","googleLink",colnames(issueSummary))
  issueSheet$latitude = notes$latitude[notes$id %in% issueSummary$note_id]
  issueSheet$longitude = notes$longitude[notes$id %in% issueSummary$note_id]
  issueSheet$googleLink = paste0("http://maps.google.com/maps?q=",issueSheet$latitude,",",issueSheet$longitude,"&ll=",issueSheet$latitude,",",issueSheet$longitude,"&z=16")
  issueSheet[,colnames(issueSummary)]=issueSummary


  ###Function code taken from http://www.r-bloggers.com/quickly-export-multiple-r-objects-to-an-excel-workbook/
  save.xlsx <- function (file, ...)
  {
    require(xlsx, quietly = TRUE)
    objects <- list(...)
    fargs <- as.list(match.call(expand.dots = TRUE))
    objnames <- as.character(fargs)[-c(1, 2)]
    nobjects <- length(objects)
    for (i in 1:nobjects) {
      if (i == 1)
        write.xlsx(objects[[i]], file, sheetName = objnames[i],row.names = FALSE)
      else write.xlsx(objects[[i]], file, sheetName = objnames[i],
                      append = TRUE)
    }
    print(paste("Workbook", file, "has", nobjects, "worksheets."))
  }

  save.xlsx(paste0(saveFolder,"/reportSheet.xlsx"),crashSheet,issueSheet)

}
