library(caret)
library(tidyverse)

# Load and prepare data
data(iris)
iris <- iris[-which(iris$Species == 'setosa'),]
y <- iris$Species
set.seed(76)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train <- iris[-test_index,]
test <- iris[test_index,]

# Identify feature columns
features <- colnames(iris)
features <- features[-length(features)]  # Remove 'Species'

# Initialize a tibble to store results
results <- tibble(Feature = character(), MaxAccuracy = numeric(), OptimalCutoff = numeric())

for (i in features) {
  # Generate cutoff sequence
  cutoff <- seq(min(train[[i]], na.rm = TRUE), max(train[[i]], na.rm = TRUE), 0.1)
  
  # Calculate accuracy for each cutoff
  accuracy <- map_dbl(cutoff, function(x) {
    temp_y_hat <- ifelse(train[[i]] > x, "virginica", "versicolor") |> 
      factor(levels = levels(train$Species))
    mean(temp_y_hat == train$Species)
  })
  
  # Find maximum accuracy and corresponding cutoff
  max_accuracy <- max(accuracy)
  optimal_cutoff <- cutoff[which.max(accuracy)]
  
  # Store results
  results <- results %>% add_row(Feature = i, MaxAccuracy = max_accuracy, OptimalCutoff = optimal_cutoff)
}

# Print the results
print(results)

# Determine the best feature
best_feature <- results |> filter(MaxAccuracy == max(MaxAccuracy))
print(best_feature)

q11 <- ifelse(test[["Petal.Width"]] > 1.7 & test[["Petal.Length"]] > 4.8, "virginica", "versicolor") |> 
  factor(levels = levels(test$Species))

mean(q11 == test$Species)
