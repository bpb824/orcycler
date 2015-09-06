#' Wrap long strings
#'
#' Wraps long strings onto multiple lines for plotting
#'
#' @param stringVector Vector of strings to split onto multiple lines
#' @param width Line width to trim strings to (in number of characters)
#'
#' @return Returns strings split onto multiple lines if they exceed the width parameter. 
#'
#' @export
#' 
#' @author RickyB 
#' @references \url{http://stackoverflow.com/questions/7367138/text-wrap-for-plot-titles}
#' 
wrap_sentence <- function(stringVector, width) {
  output = vector()
  for (j in 1:length(stringVector)){
    words <- unlist(strsplit(stringVector[j], " "))
    fullsentence <- ""
    checklen <- ""
    for(i in 1:length(words)) {
      checklen <- paste(checklen, words[i])
      if(nchar(checklen)>(width+1)) {
        fullsentence <- paste0(fullsentence, "\n")
        checklen <- ""
      }
      fullsentence <- paste(fullsentence, words[i])
    }
    fullsentence <- sub("^\\s", "", fullsentence)
    fullsentence <- gsub("\n ", "\n", fullsentence)
    output[j]=fullsentence
  }
  return(output)
}