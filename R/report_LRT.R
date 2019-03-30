#' Generate an in-text report of a Likelihood Ratio Test
#'
#' @param result Results from likelihood ratio test to report
#' @param markdown Markdown should the output be formatted for Markdown/Latex
#' or plain text [T/F]?
#' @return Formatted results containing chi-square statistic, degrees of freedom, and p-value.
#' @examples
#' \dontrun{
#' politeness <- read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
#' library(lme4)
#' max.model <- lmer(frequency ~ attitude + (1 | subject) + (1 | scenario), data=politeness)
#' reduced.model <- lmer(frequency ~ attitude + (1 | subject), data=politeness)
#' results <- anova(reduced.model, max.model, refit = F)[2, ]
#' report_LRT(result = results, markdown = T)
#' }
#' @export

report_LRT <- function(result = NULL, markdown = T) {

  chi <- result$Chisq # get chi-square statistic
  df <- result$`Chi Df` # get degrees of freedom for chi-square
  pvalue <- result$`Pr(>Chisq)` # get p-value for chi-square

  if (markdown == T) {
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
  if (markdown == F) {
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
  return(noquote(LRT_report))
}

