library(caret)
library(dplyr)

data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

class(y)
factor(y)
table(y)

#Split train and test
set.seed(2)    # if using R 3.6 or later, use set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

mean (iris$Sepal.Length)
mean (iris$Sepal.Width)
mean (iris$Petal.Length)
mean (iris$Petal.Width)

#Which feature produces the highest accuracy?
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues, function(i){
    y_hat <- ifelse(x > i,'virginica','versicolor')
    mean(y_hat == train$Species)
  })
}

predictions <- apply(train[,-5], 2, foo)
sapply(predictions,max)

#Which feature best optimizes our overall accuracy?
predictions <- apply(test[,-5], 2, foo)
sapply(predictions,max)	

#For the feature selected in Q8, use the smart cutoff value from the training data to calculate overall accuracy in the test data. What is the overall accuracy?
table(train$Species)
cutoff <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by = 0.1)

accuracy <- map_dbl(cutoff, function (x){
  y_hat <- ifelse(train$Petal.Length > x, 'virginica','versicolor')
  mean (y_hat == train$Species)
})

new_best_cutoff <- cutoff[which.max(accuracy)]
y_hat <- ifelse(test$Petal.Length > new_best_cutoff, 'virginica','versicolor')
mean (y_hat == test$Species)

cutoff <- seq(range(iris$Petal.Length)[1],range(iris$Petal.Length)[2],by=0.1)[which.max(predictions[["Petal.Length"]])]

accuracy <- iris %>%
  mutate(y_hat = ifelse(Petal.Length >= cutoff, 'virginica','versicolor')) %>%
  summarize(mean(y_hat == Species))
accuracy
