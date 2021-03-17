#' the t-test calculator
#'
#' This function calculates values associated to the t-test such as the
#' test statistics, p-value, degree of freedom, and the conclusion on
#' whether we reject or fail to reject the null hypothesis tested from
#' lifeExp data in the gapminder package.
#'
#' @param x the numeric vector of lifeExp pulled from the gapminder data
#' @param alternative the one-sided or two-sided test
#' @param mu the mean value tested given from the null hypothesis (mu = 60)
#' @keywords hypothesis
#'
#' @return A list indicating test statistics, p-value, degree of freedom,
#' and the conclusion on whether we reject or fail to reject the null hypothesis
#' tested from lifeExp data in the gapminder package.
#'
#' @examples
#' my_t.test(x, "less", 60)
#' my_t.test(x, "greater", 60)
#' my_t.test(x, "two.sided", 60)
#'
#' @export
library(tidyverse)
library(gapminder)
x <- my_gapminder %>% pull(lifeExp)
my_t.test <- function(x, alternative, mu) {

  # calculate standard error
  std_error <- sd(x) / sqrt(length(x))
  # calculate t-statistic value
  t_stat <- (mean(x) - mu) / std_error
  # store degree of freedom
  deg_freedom <- length(x) - 1

  # if else statement depend on alternative argument
  if (alternative == "less") {
    p_val <- pt(t_stat, deg_freedom, lower.tail = TRUE)
  } else if (alternative == "greater") {
    p_val <- 1 - pt(t_stat, deg_freedom, lower.tail = FALSE)
  } else if (alternative == "two.sided") {
    p_val <- pt(abs(t_stat), deg_freedom, lower.tail = FALSE) * 2
  } else {
    stop("Error; parameter 'alternative should be either two.sided,
         less, or greater")
  }

  if(p_val < 0.05) {
    message <- "reject the null"
  } else {
    message <- "fail to reject the null"
  }

  # create a returned list
  return_list <- list("test_stat"   = t_stat,
                      "df"          = deg_freedom,
                      "alternative" = alternative,
                      "p_val"       = p_val,
                      "conclusion"  = message)
  return(return_list)
}
