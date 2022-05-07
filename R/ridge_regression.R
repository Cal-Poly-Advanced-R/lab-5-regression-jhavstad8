#' Implements ridge regression with many predictors
#'
#' This function computes coefficients for ridge regression
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export
ridge_regression <- function(dat, response, lambda) {

ydat <- dat %>% pull({{response}})
ymatr <- as.matrix(ydat, ncol=1)
xdat <- dat %>% select(-{{response}})
xmatr <- as.matrix(xdat, ncol = ncol(dat))
xmatr <- cbind(rep(1,nrow(xmatr)),xmatr)
xcol <- ncol(t(xmatr)%*%xmatr)
xrow <- nrow(t(xmatr)%*%xmatr)
imatr <- diag(nrow=xrow, ncol=xcol)
names <- names(xdat)
result <- solve(t(xmatr) %*% xmatr + lambda*imatr) %*% t(xmatr) %*% ymatr
results <- data.frame(matrix(result, nrow = length(lambda)))
results <- cbind(results,lambda)
dfnames <- c("Intercept", names, "lambda")
colnames(results) <- dfnames

  ### This should be a data frame, with columns named
  ### "Intercept" and the same variable names as dat, and also a column
  ### called "lambda".
  return(results)

}

#' Determines the best penalty term from a set of options
#'
#' This function uses a randomly chosen test and training set
#'
#' No interaction terms are included.
#'
#'
#' @param train_dat A data frame to construct the model from
#' @param test_dat A data frame to test the model on
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return A data frame of penalty terms and resulting errors
#'
#' @import dplyr
#'
#' @export
find_best_lambda <- function(train_dat, test_dat, response, lambdas) {
  real <- train_dat %>% select(-{{response}})
  pred <- train_dat %>% pull({{response}})
  ymatr <- as.matrix(pred,ncol=1)
  xmatr <- as.matrix(real, ncol = ncol(train_dat))
  xmatr <- cbind(rep(1,nrow(xmatr)), xmatr)
  Amatr <- ridge_regression(train_dat,response,lambdas)
  Ematr <- ymatr - xmatr %*% Amatr
  sse <- crossprod(Ematr)
  lambda_errors <- matrix(c(lambdas,sse),ncol=2)
  lambda_errors <- data.frame(lambda_errors)
  ## I am not sure how to do this
  ### lambda_errors should be a data frame with two columns: "lambda" and "error"
  ### For each lambda, you should record the resulting Sum of Squared error
  ### (i.e., the predicted value minus the real value squared) from prediction
  ### on the test dataset.

  colnames(lambda_errors) <- c("lambda", "error")
  return(lambda_errors)
}
