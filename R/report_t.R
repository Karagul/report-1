#' Generate an in-text report of t-test results
#'
#' @param tresults object containing t-test results from t.test()
#' @param format How should the output be formatted? Available options are:
#' \code{"plain"}, \code{"latex"}, \code{"rmarkdown"}
#' @param inline Should non-plain results be formatted for inline (\code{TRUE})
#' or for copy-pasting (\code{FALSE})?
#' (default is \code{TRUE})
#' @return t-value reported to two decimal places, degrees of freedom, and p-value reported
#' to three decimal places
#' @examples
#' t1 <- rnorm(110, 50, 10)
#' t2 <- rnorm(110, 55, 12)
#' tresult <- t.test(t1, t2)
#' report_t(tresult)
#' @export



report_t <- function(tresults = NULL,
                     format = 'rmarkdown', inline = T) {
  # save objects needed to report
  tval <- tresults[[1]]
  degfree <- tresults[[2]]
  pvalue <- tresults[[3]]

  # format results for markdown
  if (format != 'plain') {
    # get t stat, df, convert to two decimal places, format report
    tval <- sub("^(-?)0.", "\\1.", sprintf("%.2f", tval))
    t_report <-
      paste("$\\textit{t}$", "(", round(degfree, 0), ")", " = ", tval, sep = "")
    # get and format p-value
    if (round(pvalue, 3) > 0) {
      pval <- sub("^(-?)0.", "\\1.", sprintf("%.3f", pvalue))
      p_report <- paste("$\\textit{p}$ =", pval)
    }
    if (round(pvalue, 3) == 0) {
      p_report <- paste("$\\textit{p}$ < .001")
    }
    # report the t statistic, df, and p-value together
    full_report <- paste(t_report, p_report, sep = ", ")
  }

  # format results for plain text
  if (format == 'plain') {
    # get t stat, df, convert to two decimal places, format report
    tval <- sub("^(-?)0.", "\\1.", sprintf("%.2f", tval))
    t_report <-
      paste("t", "(", round(degfree, 0), ")", " = ", tval, sep = "")
    # get and format p-value
    if (round(pvalue, 3) > 0) {
      pval <- sub("^(-?)0.", "\\1.", sprintf("%.3f", pvalue))
      p_report <- paste("p =", pval)
    }
    if (round(pvalue, 3) == 0) {
      p_report <- paste("p < .001")
    }
    # report the t statistic, df, and p-value together
    full_report <- paste(t_report, p_report, sep = ", ")
  }
  # return conditions
  if (inline == F){
    return(cat(full_report))
  }
  if (format == 'rmarkdown'){
    return(noquote(full_report))
  }
  if (format != 'rmarkdown'){
    return(cat(full_report))
  }
}
