options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

class (brca)

colmeans <- colMeans(brca$x)
colsd <- brca$x |> apply(2, sd)
which.min(colsd)

# Use sweep() two times to scale each column: subtract the column means of brca$x, then divide by the column standard deviations of brca$x.
# 
# After scaling, what is the standard deviation of the first column?

# y <- sweep(y, MARGIN = 1, FUN = '-', rowMeans(y,  na.rm=TRUE)) #summary stats which needs to swept away, rowMeans removed 
# y <- sweep(y, 2, colMeans(y, na.rm=TRUE)) #summary stats which needs to swept away, colMeans removed

# sweep(y, 1, rowMeans(y, na.rm=TRUE)):
  
#   sweep() function: This function is used to perform element-wise operations on arrays/matrices. It's particularly useful for applying a function across margins (rows or columns).
# y: This is the matrix or data frame being operated on.
# 1: This indicates that the operation is being applied across rows. If it were 2, the operation would be across columns.
# rowMeans(y, na.rm=TRUE):
# rowMeans() calculates the mean of each row in the matrix y.
# na.rm=TRUE specifies that any NA values should be removed before calculating the mean. This helps avoid problems if y contains missing data.
# when all else fails read the instructions : COLUMN

new_x <- sweep (brca$x, 2, colMeans(brca$x, na.rm=TRUE))  
new_x <- sweep (new_x, 2, FUN = '/', colsd)
median(new_x[,1])
sd(new_x[,1])

pca_out <- prcomp(new_x)
summary(pca_out)

data.frame(pca_out$x[,1:2], y = brca$y) |> 
  ggplot(aes(PC1, PC2, colour=y)) + 
  geom_point(cex=3, pch=21) +
  coord_fixed(ratio = 1)


data.frame(pca_out$x[,1:10], y = brca$y) |> 
  pivot_longer (cols = 1:10, names_to = "Component", values_to = "Values") |> 
  ggplot(aes(Component, Values, colour = y)) +
  geom_boxplot()
  
  #pivot_longer (cols = everything(), names_to = "Variable", values_to = "Value")

  ggplot(aes(PC1, PC2, colour=y)) + 
  geom_boxplot() +
  coord_fixed(ratio = 1)
  
  #Q6- ...
  
  set.seed(1) 
  test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
  test_x <- new_x[test_index,]
  test_y <- brca$y[test_index]
  train_x <- new_x[-test_index,]
  train_y <- brca$y[-test_index]
 
  # Question 7: Logistic regression model
  # 0.0/1.0 point (graded)
  # Set the seed to 1, then fit a logistic regression model on the training set with caret::train() using all predictors. Ignore warnings about the algorithm not converging. Make predictions on the test set.
  # 
  # What is the accuracy of the logistic regression model on the test set?   
  
  library(caret)
  set.seed(1)
  
  training_data <- data.frame(train_x, y = train_y)
  test_data <- data.frame(test_x, y = test_y)
  
  train_control <- trainControl(method = "cv", number = 10)
  
  
  train_glm <- train(y ~ ., method = "glm", data = training_data)
  #train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
  
  y_hat_glm <- predict(train_glm, test_data, type = "raw")
  #y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")
  
  confusionMatrix(y_hat_glm, test_data$y)$overall[["Accuracy"]]
  
  set.seed(5)
  train_loess <- train(y ~ ., method = "gamLoess", data = training_data)
  y_hat_loess <- predict(train_loess, test_data) #, type = "raw")
  confusionMatrix(y_hat_loess, test_data$y)$overall[["Accuracy"]]
  ## comes out with wrong answer
  
  # Set the seed to 7, then train a k-nearest neighbors model on the training set using the caret package. Try odd values of  from 3 to 21. Use the final model to generate predictions on the test set.
  # 
  # What is the final value of  used in the model?
  
  set.seed(7)
  train_knn <- train(y ~ ., method = "knn", 
                     data = training_data,
                     tuneGrid = data.frame(k = seq(3, 21, 2)))
  ggplot(train_knn, highlight = TRUE)
  y_hat_knn <- predict(train_knn, test_data) #, type = "raw")
  confusionMatrix(y_hat_knn, test_data$y)$overall[["Accuracy"]]
  train_knn$bestTune
  
  
  
  
  
  
  
