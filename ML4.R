
library(tidyverse)
library(caret)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]


### hangs
set.seed(1)
fit <- train(x_subset, y)
fit$results


# seems to work
set.seed(1)
fit <- train(x_subset, y, method = "glm")
fit$results

# error
set.seed(1)
fit <- train(y, x_subset, method = "glm")
fit$results

## test not found
set.seed(1)
fit <- test(x_subset, y, method = "glm")
fit$results

## q2

pvals <- rep(0, ncol(x))
for (i in 1:ncol(x)) {
  pvals[i] <- t.test(x[,i][y==0], x[,i][y==1], var.equal=TRUE)$p.value
}

sum(pvals <= 0.01)
