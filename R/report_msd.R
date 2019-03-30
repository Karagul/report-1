#' Generate an in-text report of means and standard deviations
#'
#' @param variable variable to report descriptives for
#' @param format How should the output be formatted? Available options are:
#' \code{"plain"}, \code{"latex"}, \code{"rmarkdown"}
#' @param inline Should non-plain results be formatted for inline or for copy-pasting?
#' @return Mean and SD reported to two decimal places
#' @examples
#' age <- rnorm(110, 21, 3.4)
#' report_msd(age)
#' @export


report_msd <- function(variable = NULL, format = 'rmarkdown', inline = T) {
  if (format != 'plain') {
    MSD_report <- paste("$\\textit{M}$ = ",
                        sprintf('%.2f', mean(variable, na.rm = T)),
                        ", ",
                        "$\\textit{SD}$ = ",
                        sprintf('%.2f', sd(variable, na.rm = T)),
                        sep = "")
  }
  if (format == 'plain') {
    MSD_report <- paste("M = ",
                        sprintf('%.2f', mean(variable, na.rm = T)),
                        ", ",
                        "SD = ",
                        sprintf('%.2f', sd(variable, na.rm = T)),
                        sep = "")
  }

  # return conditions
  if (inline == F){
    return(cat(MSD_report))
  }
  if (format == 'rmarkdown'){
    return(noquote(MSD_report))
  }
  if (format != 'rmarkdown'){
    return(cat(MSD_report))
  }
}
