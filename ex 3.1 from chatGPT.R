# Load required libraries
#library(MASS)
library(tidyverse)
library(caret)

# Function to generate synthetic data
generate_data <- function(n, mu, Sigma) {
  data <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)
  data_frame <- data.frame(data)
  setNames(data_frame, c("y", "x_1", "x_2"))
}

# Function to partition data into training and test sets
partition_data <- function(data, response_var, train_prop = 0.5) {
  test_index <- createDataPartition(data[[response_var]], times = 1, p = train_prop, list = FALSE)
  train_set <- data |>
    slice(-test_index)
  test_set <- data |>
    slice(test_index)
  list(train = train_set, test = test_set)
}

# Function to fit models given a list of formulas
fit_models <- function(train_data, formulas) {
  lapply(formulas, function(formula) lm(formula, data = train_data))
}

# Function to calculate RMSE for a model's predictions
calculate_rmse <- function(fit, test_data, response_var) {
  predictions <- predict(fit, newdata = test_data)
  sqrt(mean((predictions - test_data[[response_var]])^2))
}

# Main script
set.seed(1)
n <- 100
mu <- c(0, 0, 0)
Sigma <- matrix(c(1.0, 0.75, 0.75, 
                  0.75, 1.0, 0.25, 
                  0.75, 0.25, 1.0), 
                nrow = 3, ncol = 3)

# Generate and partition the data
dat <- generate_data(n, mu, Sigma)
splits <- partition_data(dat, "y", train_prop = 0.5)

# Fit models
formulas <- c(y ~ x_1, y ~ x_2, y ~ x_1 + x_2)
models <- fit_models(splits$train, formulas)

# Calculate and print RMSE for each model
for (i in seq_along(models)) {
  rmse <- calculate_rmse(models[[i]], splits$test, "y")
  cat("RMSE for model with formula", as.character(formulas[i]), ":", rmse, "\\n")
}
