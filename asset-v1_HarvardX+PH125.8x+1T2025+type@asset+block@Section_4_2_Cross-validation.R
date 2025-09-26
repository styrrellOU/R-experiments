# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Section 4: Cross-validation and k-Nearest Neighbors

## Section 4.2: Cross-validation

### Video: Choosing k

library(caret)
library(dslabs)
library(gridExtra)
library(tidyverse)

data("mnist_27")

ks <- seq(3, 251, 2)

library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(y_hat, mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(y_hat, mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
})

accuracy %>% mutate(k = ks) %>%
  gather(set, accuracy, -k) %>%
  mutate(set = factor(set, levels = c("train", "test"))) %>%
  ggplot(aes(k, accuracy, colour = set)) + 
  geom_line() +
  geom_point()

ks[which.max(accuracy$test)]
max(accuracy$test)

### Video: Mathematical description of cross-validation

### Video: k-fold cross-validation

# define the population distribution of income
set.seed(1995)
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, colour = I("black"))

# calculate the population median
m <- median(income)
m

# estimate the population median
N <- 100
X <- sample(income, N)
M<- median(X)
M

# use a Monte Carlo simulation to learn the distribution of M
library(gridExtra)
B <- 10^4
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, colour = I("black"))
p2 <- qplot(sample = scale(M), xlab = "theoretical", ylab = "sample") + geom_abline()
grid.arrange(p1, p2, ncol = 2)

# compare the 95% CI based on the CLT to the actual one
median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)
quantile(M, c(0.025, 0.975))

# bootstrap and approximate the distribution
B <- 10^4
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})

# look at the confidence interval from the bootstrap
quantile(M_star, c(0.025, 0.975))

### comprehension check

library(dslabs)
library(caret)

data(mnist_27)

set.seed(1995)
indices <- createResample(mnist_27$train$y, 10, list = FALSE)
## apparently sum(indexes[[1]] would work for list=TRUE?
y <- rnorm(100, 0, 1)
quantile(y, 0.75)

set.seed(1)
q3 <- replicate (10000, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})

## Q4

set.seed(1)
y <- rnorm(100, 0, 1)
set.seed(1)
q4_indices <- createResample(y, 10000)
q4test <- sapply(q4_indices, function(ind) {
  yx <- y[ind]
  quantile(yx, 0.75)
} )

mean(q4test)
sd(q4test)





