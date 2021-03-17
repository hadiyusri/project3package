# The my_lm function
# Parameter: formula = a `formula` class object,
#           data = input data frame
# Return:  a table similar to coefficient table of lm()
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
  std_error <- sqrt(diag(variance %*% solve(t(mat_X) %*% mat_X)[,3]))
  #std_error <- sqrt(diag(variance))

  # T value
  t_val <- beta_hat / std_error
  # Pr(>|t|)
  Pr_t <- pt(abs(t_val), degree_f, lower.tail = FALSE) * 2
  data_frame <- data.frame( "Estimate"   = beta_hat,
                            "Std. Error" = std_error,
                            "t value"    = t_val[,1],
                            "Pr(>|t|)"   = Pr_t)
  print(cbind("Estimate" = beta_hat,"Std. Error" = std_error,
              "t value" = t_val,"Pr(>|t|)" = Pr_t))
}
