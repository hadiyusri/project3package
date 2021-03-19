#' The linear regression model
#'
#' This function is used to fit linear model and carry out linear regression.
#'
#' @param formula an object class as a symbolic description of the
#'  model to be fitted.
#' @param data input data frame.
#' @keywords regression, linear model, inference, prediction.
#'
#' @return a table similar to coefficient table of lm()
#'
#' @examples
#' formula <- mpg ~ hp + wt
#' data <- mtcars
#' my_lm(formula, data)
#'
#'
#' @export

my_lm <- function(formula, data) {
  # Extract model
  mat_X <- model.matrix(formula, data)
  mod_Y <- model.response(model.frame(formula, data))

  # linear regression coefficients
  beta_hat <- (solve(t(mat_X) %*% mat_X)) %*% t(mat_X) %*% mod_Y

  # variance(sigma squared)
  degree_f <- nrow(mat_X) - (length(attr(terms(formula), "term.labels")) + 1)
  variance <- sum((mod_Y - mat_X %*% beta_hat)^2 / degree_f)

  # standard error
  std_error <- sqrt(diag(variance * solve(t(mat_X) %*% mat_X)))


  # T value
  t_val <- beta_hat / std_error
  # Pr(>|t|)
  Pr_t <- pt(abs(t_val), degree_f, lower.tail = FALSE) * 2

  # create return data frame
  data_frame <- data.frame(beta_hat,
                           std_error,
                           t_val,
                           Pr_t)
  names(data_frame)[1:4] <- c("Estimate",
                              "Std. Error",
                              "t value",
                              "Pr(>|t|)")
  return(data_frame)
}
