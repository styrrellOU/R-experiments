install.packages("caret")
library(caret)
library(dslabs)
data(heights)

read_mnist()
x <- read_mnist()


# get the metrics
cm <- confusionMatrix(data = y_hat, reference = test_set$sex)

# access specific metrics
cm$overall["Accuracy"]

cm$byClass[c("Sensitivity","Specificity", "Prevalence")]