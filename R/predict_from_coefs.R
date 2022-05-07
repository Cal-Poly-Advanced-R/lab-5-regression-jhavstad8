#' Computes predicted values given coefficients
#'
#' This function takes a data frame of coefficients in the form outputted by
#' functions like \code{multiple_linear_regression} or \code{ridge_regression}.
#'
#' It calculates the predicted values of a response variable from these coefficients.
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param coefs A data frame of coefficient estimates
#'
#' @return A data frame of true and predicted values
#'
#' @import dplyr
#'
#' @export
predict_from_coefs <- function(dat, response, coefs){
  dat2 <- dat %>% select(-{{response}})
  dat3 <- as.matrix(dat2)
  coefs2 <- coefs[-1]
  coefs3 <- as.matrix(coefs2)
  pred_response <- dat3 %*% t(coefs3) + coefs[1,1]
  colnames(pred_response) <- c("Predicted Responses")
  return(pred_response)
}
