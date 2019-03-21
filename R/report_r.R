#' Generate an in-text report of correlation results
#'
#' @param corcoef object containing correlation coefficient
#' @param markdown should the output be formatted for Markdown (e.g., *r* vs r)
#' or plain text [T/F]?
#' @return correlation coefficient reported to three decimal places (no leading zero)
#' @examples
#' x1 <- rnorm(200, 10, 2)
#' x2 <- rnorm(200, 15, 1.5)
#' r_coef <- cor(x1, x2)
#' report_r(r_coef)
#' @export


report_r <- function(corcoef = NULL, markdown = T) {
  if (markdown == T) {
    rcoef <- sub("^(-?)0.", "\\1.", sprintf("%.3f", corcoef))
    r_report <- paste("*r* =", rcoef)
  }
  if (markdown == F) {
    rcoef <- sub("^(-?)0.", "\\1.", sprintf("%.3f", corcoef))
    r_report <- paste("r =", rcoef)
  }
  return(noquote(r_report))
}
