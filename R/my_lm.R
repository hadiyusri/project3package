#' the linear regression model summary
#'
#' This function calculates the summary of the linear regression model analyzed
#' from the lifeExp data in the gapminder package. In the data, gdpPercap and
#' continent are the explanatory variables. We will also plot the actual vs.
#' fitted values from the function and do an interpretation about the plots.
#'
#' @param formula the regression formula used based on the data given in a package
#' @param data tinput data frame
#' @keywords regression, linear model
#'
#' @return a table similar to coefficient table of lm()
#'
#' @examples
#' my_lm(x, "less", 60)
#'
#'
#' @export
library(gapminder)
data("my_gapminder")
library(ggplot2)
#test1 <- my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder)
my_lm <- function(formula, data) {
  mat_X <- model.matrix(formula, data)
  my_frame <- model.frame(formula, data)
  # mod_fits <- fitted(my_frame)
  # my_df <- data.frame(formula, fitted = mod_fits)
  mod_Y <- model.response(my_frame)

  # linear regression coefficients
  beta_hat <- solve(t(mat_X) %*% mat_X) %*% t(mat_X) %*% mod_Y

  # variance(sigma squared)
  degree_f <- nrow(data) - length(mat_X)
  variance <- (mod_Y - mat_X %*% beta_hat)**2 / degree_f

  # standard error
  # j <- variance %*% solve(t(mat_X) %*% mat_X)[,3]
  std_error <- sqrt(diag(variance %*% solve(t(mat_X) %*% mat_X)[5, ]))
  #std_error <- sqrt(diag(variance))

  # T value
  t_val <- beta_hat / std_error
  # Pr(>|t|)
  Pr_t <- pt(abs(t_val), degree_f, lower.tail = FALSE) * 2
  data_frame <- data.frame( "Estimate"   = beta_hat,
                            "Std. Error" = std_error,
                            "t value"    = t_val[4 ,])
  print(cbind("Estimate" = beta_hat,"Std. Error" = std_error,
              "t value" = t_val,"Pr(>|t|)" = Pr_t))
}
