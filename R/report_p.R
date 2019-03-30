#' Automate the reporting of p-values
#'
#' @param pvalue object containing pvalue to report
#' @param format How should the output be formatted? Available options are:
#' \code{"plain"}, \code{"latex"}, \code{"rmarkdown"}
#' @param inline Should non-plain results be formatted for inline or for copy-pasting?
#' @return p-value rounded to three decimal places if greater than .001. If less than,
#' .001 defaults to p < .001.
#' @examples
#' report_p(0.001567)
#' @export


report_p <- function(pvalue = NULL, format = 'rmarkdown', inline = T) {
  if (format != 'plain') {
    if (round(pvalue, 3) > 0) {
      pval <- sub("^(-?)0.", "\\1.", sprintf("%.3f", pvalue))
      p_report <- paste("$\\textit{p}$", " = ", pval, sep = "")
    }
    if (round(pvalue, 3) == 0) {
      p_report <- paste("$\\textit{p}$", " < .001")
    }
  }
  if (format == 'plain') {
    if (round(pvalue, 3) > 0) {
      pval <- sub("^(-?)0.", "\\1.", sprintf("%.3f", pvalue))
      p_report <- paste("p =", pval)
    }
    if (round(pvalue, 3) == 0) {
      p_report <- paste("p < .001")
    }
  }

  # return conditions
  if (inline == F){
    return(cat(p_report))
  }
  if (format == 'rmarkdown'){
    return(noquote(p_report))
    }
  if (format != 'rmarkdown'){
    return(cat(p_report))
  }
}


