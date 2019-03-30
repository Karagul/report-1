#' Generate an in-text report of a Likelihood Ratio Test
#'
#' @param result Results from likelihood ratio test to report
#' @param format How should the output be formatted? Available options are:
#' \code{"plain"}, \code{"latex"}, \code{"rmarkdown"}
#' @param inline Should non-plain results be formatted for inline (\code{TRUE})
#' or for copy-pasting (\code{FALSE})?
#' (default is \code{TRUE})
#' @return Formatted results containing chi-square statistic, degrees of freedom,
#' and p-value.
#' @examples
#' \dontrun{
#' politeness <- read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
#' library(lme4)
#' max.model <- lmer(frequency ~ attitude + (1 | subject) + (1 | scenario), data=politeness)
#' reduced.model <- lmer(frequency ~ attitude + (1 | subject), data=politeness)
#' results <- anova(reduced.model, max.model, refit = F)[2, ]
#' report_LRT(result = results, format = 'rmarkdown')
#' }
#' @export

report_LRT <- function(result = NULL, format = 'rmarkdown', inline = T) {

  chi <- result$Chisq # get chi-square statistic
  df <- result$`Chi Df` # get degrees of freedom for chi-square
  pvalue <- result$`Pr(>Chisq)` # get p-value for chi-square

  if (format != 'plain') {
    if (round(pvalue, 3) > 0) {
      LRT_report <- paste("$\\chi^2$", "(",df,")", " = ",
                          sub("^(-?)0.", "\\1.", sprintf("%.2f", chi)),
                          ", ",
                          "$\\textit{p}$ = ",
                          sub("^(-?)0.", "\\1.", sprintf("%.3f", pvalue)),
                          sep = "")
    }
    if (round(pvalue, 3) == 0) {
      LRT_report <- paste("$\\chi^2$", "(",df,")", " = ",
                        sub("^(-?)0.", "\\1.", sprintf("%.2f", chi)),
                        ", ",
                        "$\\textit{p}$ < .001", sep = "")
    }
  }
  if (format == 'plain') {
    if (round(pvalue, 3) > 0) {
      LRT_report <- paste("X^2", "(",df,")", " = ",
                        sub("^(-?)0.", "\\1.", sprintf("%.2f", chi)),
                        ", ",
                        "p = ",
                        sub("^(-?)0.", "\\1.", sprintf("%.3f", pvalue)),
                        sep = "")
    }
    if (round(pvalue, 3) == 0) {
      LRT_report <- paste("X^2", "(",df,")", " = ",
                          sub("^(-?)0.", "\\1.", sprintf("%.2f", chi)),
                          ", ",
                          "p < .001", sep = "")
    }
  }
  # return conditions
  if (inline == F){
    return(cat(LRT_report))
  }
  if (format == 'rmarkdown'){
    return(noquote(LRT_report))
  }
  if (format != 'rmarkdown'){
    return(cat(LRT_report))
  }
}

