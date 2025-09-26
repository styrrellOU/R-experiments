library(tidyverse)
library(caret)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

set.seed(1)
fit <- train(x_subset, y, method = "glm")
fit$results

pvals <- rep(0, ncol(x))
for (i in 1:ncol(x)) {
  pvals[i] <- t.test(x[,i][y==0], x[,i][y==1], var.equal=TRUE)$p.value
}

#Q3

ind <- which(pvals < 0.01)

x_subset <- x[, ind]

#Q4

set.seed(1)
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

set.seed(1)
fit <- train(x_subset, y, method = "knn")
ggplot(fit)