#' Generate an in-text report of means and standard deviations
#'
#' @param var variable to report descriptives for
#' @param markdown should the output be formatted for Markdown (e.g., *M* vs M)
#' or plain text [T/F]?
#' @return Mean and SD reported to two decimal places
#' @examples
#' age <- rnorm(110, 21, 3.4)
#' report_msd(age)
#' @export


report_msd <- function(var = NULL, markdown = T) {
  if (markdown == T) {
    MSD_report <- paste("$\\textit{M}$ = ",
                        sprintf('%.2f', mean(var, na.rm = T)),
                        ", ",
                        "$\\textit{SD}$ = ",
                        sprintf('%.2f', sd(var, na.rm = T)),
                        sep = "")
  }
  if (markdown == F) {
    MSD_report <- paste("M = ",
                        sprintf('%.2f', mean(var, na.rm = T)),
                        ", ",
                        "SD = ",
                        sprintf('%.2f', sd(var, na.rm = T)),
                        sep = "")
  }
  return(noquote(MSD_report))
}
