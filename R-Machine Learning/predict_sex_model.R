#install.packages('caret')
library(caret)
library(dslabs)
data(heights)

#See summary statistics of the data
heights %>% group_by(sex) %>% summarise(mean(height),sd(height))

x <- heights$height
y <- heights$sex

set.seed(2)
test_index <- createDataPartition(y, times=1, p=0.5, list = FALSE)

train_set <- heights[-test_index,]
test_set <- heights[test_index,]

# Model 1 - Guess the outcome
y_hat <- sample(c('Male','Female'), length(test_index), replace=TRUE)
y_hat <- y_hat %>% factor(levels = levels(test_set$sex))

#See the accuracy when we just guessed the outcome
mean (y_hat == test_set$sex)

# Model 2 - Assume some cutoff and classify as males/females
mean(heights$height)
sd(heights$height)
cutoff <- 64
y_hat <- ifelse(train_set$height > cutoff, 'Male','Female') %>% factor (levels=levels(test_set$sex))
y_hat
mean (y_hat == test_set$sex) ##Accuracy goes to 71%

# Model 3 - Assume some higher cutoff and classify as males/females
mean(heights$height)
sd(heights$height)
cutoff <- 66
y_hat <- ifelse(train_set$height > cutoff, 'Male','Female') %>% factor (levels=levels(test_set$sex))
y_hat
mean (y_hat == test_set$sex) ##Accuracy drops to 62%


# Model 4 - Try different cut offs
cutoff <- seq(60,70)

accuracy <- map_dbl(cutoff, function (x){
  y_hat <- ifelse(train_set$height > x, 'Male','Female') %>% factor (levels=levels(test_set$sex))
  mean (y_hat == test_set$sex)
})

plot(cutoff, accuracy) #best accuracy is around 61

#Final model - using best cutoff
best_cutoff <- 61
y_hat <- ifelse(train_set$height > best_cutoff, 'Male','Female') %>% factor (levels=levels(test_set$sex))
y_hat
mean (y_hat == test_set$sex) ##Accuracy drops to 62%

#Confusion Matrix
table(predicted = y_hat, actual = test_set$sex) #reveals problem with model

#find mean of males
mean (heights$sex=='Male') #77% are males
mean (heights$sex=='Female') #22% are females

# Similar to concept of Precision and Recall we have concept of Sensitivity & Specificity
# Sensitivity -> predict y_hat as 1 when y is 1
# Specificity -> predict y_hat as 0 when y is 0

confusionMatrix(data=y_hat, reference = test_set$sex)
F_meas(data=y_hat, reference = factor(test_set$sex))

# Model 5 - Try different cut offs and measure F1
cutoff <- seq(60,75)

F_1 <- map_dbl(cutoff, function (x){
  y_hat <- ifelse(train_set$height > x, 'Male','Female') %>% factor (levels=levels(test_set$sex))
  F_meas(data=y_hat, reference = factor(test_set$sex))
})

plot(cutoff, F_1) #best f1_score is around 69

new_best_cutoff <- cutoff[which.max(F_1)]
y_hat <- ifelse(train_set$height > new_best_cutoff, 'Male','Female') %>% factor (levels=levels(test_set$sex))
F_meas(data=y_hat, reference = factor(test_set$sex))

confusionMatrix(data=y_hat, reference = test_set$sex)
###############MNIST Dataset
mnist <- read_mnist()
class (mnist)
length(mnist)

class (mnist[1]$train$labels)

images <- mnist[1]$train$images
class (images)
dim(images)

labels <- mnist[1]$train$labels
length(unique(labels))
