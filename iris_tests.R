# iris tests
library(caret)
library(tidyverse)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(76)
test_index <- createDataPartition(y, times=1, p=0.5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

features <- colnames(iris)
train[[features[1]]] ## the [[]] gets to the column, the [] to the element of features

features <- features |> head(-1) # remove the species

cutoff <- seq(min(iris$Sepal.Length), max(iris$Sepal.Length), 0.1)
results <- tibble() ## ncol = length(features), nrow = 1)

for (i in features[]) {
  cutoff <- seq(min(iris[[i]]), max(iris[[i]]), 0.1)
  accuracy <- map_dbl(cutoff, function(x){
    temp_y_hat <- ifelse(x > train[[i]], "virginica", "versicolor") |> 
      factor(levels = levels(train$Species))
    mean(temp_y_hat == train$Species)
   })
  results[1, i] <- mean(accuracy) #cutoff[which.max(accuracy)]
  
}



cutoff <- seq(min(iris$Sepal.Length), max(iris$Sepal.Length), 0.1)
accuracy <- map_dbl(cutoff, function(x){
  temp_y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") |> 
    factor(levels = levels(train$Species))
  mean(temp_y_hat == train$Species)
})


foo <- function(x){
  rangedValues <- seq(range(x)[1], range(x)[2], by=0.1)
  sapply(rangedValues, function(i){
    y_hat <- ifelse(x>i, 'virginica', 'versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5], 2, foo)
sapply(predictions, max)	
