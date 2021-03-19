#' k-nearest neighbors cross validation method
#'
#' This function predict output class \code{species} using covariates \code{bill_length_mm},
#' \code{bill_depth_mm}, \code{flipper_length_mm}, and \code{body_mass_g}. We will
#' be using a 5-fold cross validation to test whether it is working or not.
#'
#' @param train input data frame
#' @param cl true class value of your training data
#' @param k_nn integer representing the number of neighbors
#' @param k_cv integer representing the number of folds
#' @keywords prediction, cross validation
#'
#' @return a list that contain objects;
#' *class: a vector of the predicted class $\hat{Y}_{i}$ for all observations
#' *cv_err: a numeric with the cross-validation misclassification error
#'
#' @examples
#' my_knn_cv(data_p[, -1], data_p[, 1], 5, 5)
#'
#' @export
library(palmerpenguins)
library(class)
library(tidyverse)
data(package = "palmerpenguins")
my_knn_cv <- function(train, cl, k_nn, k_cv) {

  # Randomly assign observation to folds with equal probability
  fold <- k_cv
  inds <- sample(rep(1:fold, length = nrow(train)))
  n_data <- data.frame("x" = train, "y" = cl, "split" = inds)

  # Variably classy and prob_attr to keep track the predicted class and probability
  classy <- c()
  prob_attr <- c()

  # for loops to iterate every each fold
  for (i in 1:fold) {

    # In each iteration, the data splitted into 5 parts where we take the fold
    # data as test data and others as training
    x_train <- n_data %>% filter(split != i) %>% select(-"species", -"split")
    x_test <- n_data %>% filter(split == i) %>% select(-"species", -"split")
    y_train <- n_data %>% filter(split != i) %>% select("species")

    # record the prediction and miscalculation
    knn_pred <- as.factor(knn(x_train, x_test, cl = y_train$species, k = k_nn, prob = TRUE))
    prob_attr <- append(prob_attr, attr(knn_pred,"prob"))
    classy <- append(class, as.factor(knn_pred))
  }
  # Calculate the misclassification error
  prob_attr <- 1 - prob_attr
  # Calculate the average misclassification rate
  cv_err <- mean(prob_attr)
  # Full data knn()
  class <- knn(train, train, cl$species, k_nn)
  # Store and return the output
  my_output <- list("class" = class,
                    "cv_err" = cv_err)
  return(my_output)
}

# Prove the function my_knn_cv(work)
# Use Penguin data from palmerpenguins library
data_p <- palmerpenguins::penguins %>%
  drop_na() %>%
  select(-island, -sex, -year)

# Store and calculate knn
p_knn1 <- my_knn_cv(data_p[, -1], data_p[, 1], 1, 5)
p_knn5 <- my_knn_cv(data_p[, -1], data_p[, 1], 5, 5)

# Store as numeric the real knn
knn1 <- as.numeric(knn(data_p[, -1], data_p[, -1], data_p[, 1]$species, 1))
knn5 <- as.numeric(knn(data_p[, -1], data_p[, -1], data_p[, 1]$species, 5))

# Calculate and store the true class
true_class <- data_p[, 1]
trueClass <- as.numeric(unlist(true_class))

# Calculate the training error
train_err1 <- mean((trueClass - knn1)^2)
train_err5 <- mean((trueClass - knn5)^2)

# Table for training error
table_train_err <- cbind("train error 1" = train_err1, "train error 5" = train_err5)

# Table for cross validation error
table_err <- cbind("cv_err knn1" = p_knn1$cv_err,"cv_err knn5" =  p_knn5$cv_err)
comp_knn1 <- 0
comp_knn5 <- 0

p_knn1_class <- as.character(p_knn1$class)
p_knn5_class <- as.character(p_knn5$class)

# for loop to calculate the difference between the true class and class predicted
for (a in 1:333) {
  if (true_class[a, 1] != p_knn1_class[a]) {
    comp_knn1 <- comp_knn1 + 1
  }
  if (true_class[a, 1] != p_knn5_class[a]) {
    comp_knn5 <- comp_knn5 + 1
  }
}
# Table for difference between true and predicted class
table_class <- cbind("difference knn1" = comp_knn1, "difference knn5" = comp_knn5)

print(table_train_err)
print(table_class)
print(table_err)
