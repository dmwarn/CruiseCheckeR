#' Make roxygen document for data included with R packages.
#'
#' @param dataFrame
#' The name of the data frame for which you wish to create roxygen output
#' @param title
#' The title for the data frame
#'
#' @return
#' Text in roxygen format for use in documenting data included with R packages.
#' @export
#'
makeRoxDataDoc <- function (dataFrame, title = substitute(dataFrame)) {
  output = c(paste("#'", title), "#' @format data.frame", gsub("^","#'",capture.output(str(dataFrame))), dQuote(title))
  cat(output, sep="\n")
}





