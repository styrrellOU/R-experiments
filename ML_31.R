library(tidyverse)
library(caret)

set.seed(1)
n <- c(100, 500, 1000, 5000, 10000)
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
#this creates a two by two matrix whose values are simply 9, 4.5, 4.5, 9


dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) |>
  data.frame() |> setNames(c("x", "y"))

set.seed(1)

rmse <- replicate (100, { 
  y <- dat$y
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat |> slice(-test_index) 
  test_set <- dat |> slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  #y_hat <- fit$coef[1] + fit$coef[2]*test_set$x
  y_hat <- predict(fit, test_set)
  # A linear model is fitted using the training set with y as the dependent variable and x as the independent variable.
  
  sqrt(mean((y_hat - test_set$y)^2))}
)


checkRMSEbyNbr <- function (n) {
  #Q2 part 1
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) |>
    data.frame() |> setNames(c("x", "y"))
  #Q2 part 2
  rmse <- replicate (100, { 
    y <- dat$y
    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat |> slice(-test_index) 
    test_set <- dat |> slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    #y_hat <- fit$coef[1] + fit$coef[2]*test_set$x
    y_hat <- predict(fit, test_set)
    # A linear model is fitted using the training set with y as the dependent variable and x as the independent variable.
    
    sqrt(mean((y_hat - test_set$y)^2))}
  )
  #Q2 part 3
  c (n, mean(rmse), sd(rmse))
  
}

q2 <- sapply(n, checkRMSEbyNbr)



# mvrnorm produces simulations of a multivariate normal distribution: 
# n = 100 
# µ = c(69,69) gives the means of the variables. If it doesn't match
# the covariance matrix (sigma) --> error
# then we pipe it to set the column names
# covariance matrix specifies how the two variables vary in combination, so 
# this is showing what exactly? Basically just that y's variation with x (and vice-versa)
# is half as strong ? 

#"Overall, this code implements a typical workflow in applied statistics or machine learning: splitting data, training a model, making predictions, and evaluating error.
#(Sensitivity level: CU) GPT@EC AI generated content – please use with caution."

set.seed(1)

rmse <- replicate (100, { 
  y <- dat$y
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat |> slice(-test_index) 
  test_set <- dat |> slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  #y_hat <- fit$coef[1] + fit$coef[2]*test_set$x
  y_hat <- predict(fit, test_set)
  # A linear model is fitted using the training set with y as the dependent variable and x as the independent variable.
  
  sqrt(mean((y_hat - test_set$y)^2))}
  )

############################################# Q6 #######################################

set.seed(1)
n <- 100
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) |>
  data.frame() |> setNames(c("y", "x_1", "x_2"))

set.seed(1, sample.kind="Rounding")
y <- dat$y
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- dat |> slice(-test_index) 
test_set <- dat |> slice(test_index)
opts <- c("x_1", "x_2", "x_1+x_2")
fit1 <- lm(y ~ x_1, data = train_set)
fit2 <- lm(y ~ x_2, data = train_set)
fit3 <- lm(y ~ x_1+x_2, data = train_set)

#y_hat <- fit$coef[1] + fit$coef[2]*test_set$x
y_hat1 <- predict(fit1, test_set)
y_hat2 <- predict(fit2, test_set)
y_hat3 <- predict(fit3, test_set)

# A linear model is fitted using the training set with y as the dependent variable and x as the independent variable.

sqrt(mean((y_hat1 - test_set$y)^2))
sqrt(mean((y_hat2 - test_set$y)^2))
sqrt(mean((y_hat3 - test_set$y)^2))


