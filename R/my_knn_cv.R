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
  count <- nrow(train)
  inds <- sample(rep(1:fold, length = count))
  n_data <- train %>% mutate("split" = inds)

  # Variable classy and prob_attr to keep track the predicted class and probability
  classy <- vector(length = length(train))
  prob_attr <- 0

  # for loops to iterate every each fold
  for (i in 1:fold) {

    # In each iteration, the data splitted into 5 parts where we take the fold
    # data as test data and others as training
    x_train <- n_data %>% filter(split != i)
    x_test <- n_data %>% filter(split == i)
    y_train <- cl[inds != i]

    # record the prediction and miscalculation
    classy[fold == i] <- knn(x_train, x_test, y_train, k_nn)
    check <- suppressWarnings(as.numeric(cl[inds == i]))
    if (check != class[inds]) {
      prob_attr <- prob_attr + length(check)
    }
  }
  # Calculate the average misclassification rate
  cv_err <- prob_attr / count
  # Store and return the output
  my_output <- list("class" = classy,
                    "cv_err" = cv_err)
  return(my_output)
}
