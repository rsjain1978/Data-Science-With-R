###-----------------------------MNIST Digits Classification Using GLM
data('mnist_27')


head(mnist_27)
View (mnist_27)
class (mnist_27)

train <- mnist_27[[1]]
test <- mnist_27[[2]]

p <-  train %>% ggplot() + geom_point(aes(x=x_1, y=x_2,color=y),size=3)
p

#fit a Logistic regression model
glm_fit <- glm(y~x_1+x_2, data=train,family='binomial')

#predict logits
pred_digits_logits <- predict(glm_fit, test, type='response')

pred_digits <- ifelse(pred_digits_logits>0.5, 7,2)

#print confusion matrix
confusionMatrix(as.factor(pred_digits), as.factor(test$y))

###-------------------------------MNIST Digits Classification Using KNN

knn_fit <- train %>% knn3(y~., data=., k=5)
pred_digits_knn <-predict(knn_fit, test, type='class')

#print confusion matrix
confusionMatrix(as.factor(pred_digits_knn), as.factor(test$y))$overall['Accuracy']

###-------------------------------MNIST Digita Classification Using multiple values of K in KNN
ks <- seq(3,251, 2)

accuracies <- sapply(ks, function(k){
  knn_fit <- train %>% knn3(y~., data=., k=k)
  
  test_pred_digits_knn <-predict(knn_fit, test, type='class')
  train_pred_digits_knn <-predict(knn_fit, train, type='class')
  
  #print confusion matrix
  acc_test <- confusionMatrix(as.factor(test_pred_digits_knn), as.factor(test$y))$overall['Accuracy']
  acc_train <- confusionMatrix(as.factor(train_pred_digits_knn), as.factor(train$y))$overall['Accuracy']
  accs <- c(acc_test, acc_train)
  accs
  
})

length(ks)

class(accuracies)
dim(accuracies)
accuracies[1,]

df <- data.frame(x=ks, test_acc=accuracies[1,], train_acc=accuracies[2,])
head(df)
p <-  ggplot(df, aes(x=x)) + geom_line(aes(y=train_acc), col='red') + geom_line(aes(y=test_acc), col='green')
p

###-------------------------------Predict Heights Using LR
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
library(dslabs)

data(heights)
test_index <- createDataPartition(heights$sex,times=1,p=0.5,list=FALSE)
test <- heights[test_index,]
train <- heights[-test_index,]

train$sex <- ifelse(train$sex=='Male',0,1)
test$sex <- ifelse(test$sex=='Male',0,1)

#fit a Logistic regression model
glm_fit <- glm(sex~height, data=train,family='binomial')

#predict logits
pred_sex_logits <- predict.glm(glm_fit, test, type='response')

#plot logits
p <-  ggplot() + 
  geom_point(aes(x=test$height, y=pred_sex_logits),size=3)
p

#convert logits to sex value - 0 or 1
pred_sex <- ifelse(pred_sex_logits>0.5,1,0)


#print confusion matrix
confusionMatrix(as.factor(pred_sex), as.factor(test$sex))$overall['Accuracy']

###-------------------------------Predict Heights Using KNN
ks <- seq(1,101,3)

f1_scores<- sapply(ks, function(k){
  test_index <- createDataPartition(heights$sex,times=1,p=0.5,list=FALSE)
  test <- heights[test_index,]
  train <- heights[-test_index,]
  
  knnfit <- train %>% knn3(sex~height, data=., k=k)
  pred_y <- predict(knnfit, test, type='class')
  
  F_meas(pred_y, test$sex)
  
})
f1_scores
df <- data.frame(k=ks, f1=f1_scores)
print (df)
p <-  ggplot() + geom_point(aes(x=ks, y=f1_scores),size=3)
p
