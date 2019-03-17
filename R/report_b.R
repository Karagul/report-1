#' Generate an in-text report of regression coeffiecients
#'
#' @param coefficient object containing regression coefficient to report
#' @param markdown should the output be formatted for Markdown (e.g., *p*) or plain text [T/F]?
#' @param standardize is the coefficient undstandardized [F] or standardized [T]
#' @return unstandardized or standardized regression coefficient
#' rounded to three decimal places (e.g., b = .123, beta = .123).
#' @examples
#' x <- c(1:25)
#' y = c(1, 1, 2, 3, 4, 5, 6, 2, 4, 6, 7, 2, 7, 3, 1, 1, 1, 3, 4, 6, 7, 7, 1, 4, 1)
#' model <- lm(y ~ x)
#' coef <- summary(model)$coefficients[2, 1]
#' report_b(coef, standardize = FALSE, markdown = FALSE)
#' @export



report_b <- function(coefficient = NULL, markdown = T, standardize = F){
  if(markdown == T){
    if(standardize == F){
      bval <- sub("^(-?)0.", "\\1.", sprintf("%.3f", coefficient))
      b_report <- paste("*b* =", bval)
    }
    if(standardize == T){
      bval <- sub("^(-?)0.", "\\1.", sprintf("%.3f", coefficient))
      b_report <- paste("\beta", " = ", bval, sep = "")
    }
  }
  if(markdown == F){
    if(standardize == F){
      bval <- sub("^(-?)0.", "\\1.", sprintf("%.3f", coefficient))
      b_report <- paste("b =", bval)
    }
    if(standardize == T){
      bval <- sub("^(-?)0.", "\\1.", sprintf("%.3f", coefficient))
      b_report <- paste("beta", " = ", bval, sep = "")
    }
  }
  return(noquote(b_report))
}


