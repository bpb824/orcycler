#' Impute Responses
#'
#' Wrapper for \code{\link[missForest]{missForest}} function used to impute missing responses in user, trip, and report data. 
#'
#' @param data Summary table to impute missing responses in. 
#' @param impCols Columns in summary table to use in imputation, including both columns with/without missing responses. Values used for imputation must be numeric or factor. 
#'
#' @return Summary table with imputed responses. 
#'
#' @export

imputeResponses = function(data,impCols){
  require(missForest)
  miss = data[,impCols]
  all = 1:ncol(data)
  nonMiss = all[!(all %in% impCols)]
  dataImp = cbind(data[,nonMiss],as.data.frame(missForest(miss)$ximp))
  return(dataImp)
}