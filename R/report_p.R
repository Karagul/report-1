#' Automate the reporting of p-values
#'
#' @param pvalue object containing pvalue to report
#' @param markdown should the output be formatted for Markdown (e.g., *p* ) or
#' plain text [T/F]?
#' @return p-value rounded to three decimal places if greater than .001. If less than,
#' .001 defaults to p < .001.
#' @examples
#' report_p(0.001567)
#' @export


report_p <- function(pvalue = NULL, markdown = T) {
  if (markdown == T) {
    if (round(pvalue, 3) > 0) {
      pval <- sub("^(-?)0.", "\\1.", sprintf("%.3f", pvalue))
      latex_p <- switch("p", "p" = "$\textit{p}$")
      p_report <- paste(latex_p, " = ", pval, sep = "")
    }
    if (round(pvalue, 3) == 0) {
      p_report <- paste(latex_p, "< .001")
    }
  }
  if (markdown == F) {
    if (round(pvalue, 3) > 0) {
      pval <- sub("^(-?)0.", "\\1.", sprintf("%.3f", pvalue))
      p_report <- paste("p =", pval)
    }
    if (round(pvalue, 3) == 0) {
      p_report <- paste("p < .001")
    }
  }
  return(noquote(p_report))
}


