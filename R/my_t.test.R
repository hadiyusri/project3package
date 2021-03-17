library(gapminder)
# The my_t.test function
# Parameter: x = numeric vector of data, alternative = character string,
#           mu = number indicating the null hypothesis value of the mean
# Return: a list with elements test_stat, df, alternative, p_val
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
