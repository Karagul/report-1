#' Generate an in-text report of correlation results
#'
#' @param corcoef object containing correlation coefficient
#' @param format How should the output be formatted? Available options are:
#' \code{"plain"}, \code{"latex"}, \code{"rmarkdown"}
#' @param inline Should non-plain results be formatted for inline or for copy-pasting?
#' @return correlation coefficient reported to three decimal places (no leading zero)
#' @examples
#' x1 <- rnorm(200, 10, 2)
#' x2 <- rnorm(200, 15, 1.5)
#' r_coef <- cor(x1, x2)
#' report_r(r_coef)
#' @export


report_r <- function(corcoef = NULL, format = 'rmarkdown', inline = T) {
  if (format != 'plain') {
    rcoef <- sub("^(-?)0.", "\\1.", sprintf("%.3f", corcoef))
    r_report <- paste("$\\textit{r}$ =", rcoef)
  }
  if (format == 'plain') {
    rcoef <- sub("^(-?)0.", "\\1.", sprintf("%.3f", corcoef))
    r_report <- paste("r =", rcoef)
  }
  # return conditions
  if (inline == F){
    return(cat(r_report))
  }
  if (format == 'rmarkdown'){
    return(noquote(r_report))
  }
  if (format != 'rmarkdown'){
    return(cat(r_report))
  }
}
