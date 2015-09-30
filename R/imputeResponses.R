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
  imp = data[,impCols]
  if(class(impCols)=="character"){
    all = colnames(data)
  }else{
    all = 1:ncol(data)
  }
  nonImp = data[,!(all %in% impCols)]
  result = missForest::missForest(imp)
  dataImp = cbind(nonImp,result$ximp)
  return(dataImp)
}
