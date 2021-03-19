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
#' @keywords hypothesis, inference
#'
#' @return A list indicating test statistics, p-value, degree of freedom,
#' and the conclusion on whether we reject or fail to reject the null hypothesis
#' tested from lifeExp data in the gapminder package.
#'
#' @examples
#' set.seed(302)
#' p <- 0.4
#' flip_coin <- rbinom(100, size = 1, prob = p)
#' my_t.test(as.numeric(flip_coin), alternative = "greater", mu = p)
#'
#' @export

my_t.test <- function(x, alternative, mu) {

  # calculate standard error
  std_error <- sd(x) / sqrt(length(x))
  # calculate t-statistic value
  t_stat <- (mean(x) - mu ) / std_error
  # store degree of freedom
  deg_freedom <- length(x) - 1

  # if else statement depend on alternative argument
  if (alternative == "less") {
    p_val <- pt(t_stat, deg_freedom, lower.tail = TRUE)
  } else if (alternative == "greater") {
    p_val <- pt(t_stat, deg_freedom, lower.tail = FALSE)
  } else if (alternative == "two.sided") {
    p_val <- pt(abs(t_stat), deg_freedom, lower.tail = FALSE) * 2
  } else {
    stop("Error; parameter 'alternative should be either two.sided,
         less, or greater")
  }
  # create a returned list
  return_list <- list("test_stat"   = t_stat,
                      "df"          = deg_freedom,
                      "alternative" = alternative,
                      "p_val"       = p_val)
  return(return_list)
}
