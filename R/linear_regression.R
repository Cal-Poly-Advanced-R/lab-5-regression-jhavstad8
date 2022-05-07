#' Implements simple linear regression by hand
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param explanatory The name of the explanatory variable in the data frame (unquoted)
#' @param method The method used to compute the coefficients (NULL, "qr", "gradientdescent")
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export
simple_linear_regression <- function(dat, response, explanatory, method = NULL){

  x <- dat %>% pull({{explanatory}})
  y <- dat %>% pull({{response}})

  explan_name <- dat %>%
    select({{explanatory}}) %>%
    names()

  x_bar <- mean(x)
  y_bar <- mean(y)

  ### Edit code after here

  sd_x <- sd(x)
  sd_y <- sd(y)

  beta_0 <- y_bar - (cov(x,y)/var(x))*x_bar
  beta_1 <- cov(x,y)/var(x)

  ### Stop editing

  results <- tibble::tibble(
    Intercept = beta_0,
    Slope = beta_1
  )

  names(results)[2] <- explan_name

  return(results)

}


#' Implements linear regression with many predictors by hand
#'
#' This function computes coefficients for multiple regression by hand.
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param method The method used to compute the coefficients (NULL, "qr", "gradientdescent")
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#' @export
multiple_linear_regression <- function(dat, response, method = NULL) {
  num_rows <- length(dat[,1])
  ones_col <- rep(1,num_rows)
  xmatr <- matrix(ones_col, ncol = 1)

  y <- dat %>% pull({{response}})
  ymatr <- matrix(y, ncol = 1)

  dat2 <- dat %>% select(-{{response}})
  xmatr2 <- cbind(xmatr,dat2)
  xmatr3 <- as.matrix(xmatr2)
  xdf <- data.frame(xmatr3)
  names <- names(xdf[-1])
  vars <- c("Intercept", names)
  A <- solve(crossprod(xmatr3)) %*% t(xmatr3) %*% ymatr
  results <- data.frame(matrix(A,nrow=1))
  ### This should be a data frame, with columns named
                ### "Intercept" and the same variable names as dat.
  colnames(results) <- vars

  return(results)

}
