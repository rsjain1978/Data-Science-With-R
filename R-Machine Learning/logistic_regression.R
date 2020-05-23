set.seed(1)
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
confusionMatrix(as.factor(pred_sex), as.factor(test$sex))$overall["Accuracy"]

#plot actual vs predicted sex values
p <-  ggplot() + 
  geom_point(aes(x=test$height, y=pred_sex),size=3) + 
  geom_point(aes(x=test$height, y=test$sex),size=1, color='red')
p


###-----------------------------MNIST Digits Classification
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
pred_digits_logits <- predict.glm(glm_fit, test, type='response')

pred_digits <- ifelse(pred_digits_logits>0.5, 7,2)

#print confusion matrix
confusionMatrix(as.factor(pred_digits), as.factor(test$y))

p <-  train %>% ggplot() + geom_point(aes(x=x_1, y=x_2,color=y),size=3) + geom_line(aes(pred_digits_logits),size=1)
p

#-----------------------------------------------
library(purrr)
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later

make_data <- function(n = 1000, 
                        p = 0.5, 
                        mu_0 = 0, 
                        mu_1 = 1, 
                        sigma_0 = 1,  
                        sigma_1 = 1){
    
    y <- rbinom(n, 1, p)
    print (y)
    f_0 <- rnorm(n, mu_0, sigma_0)
    f_1 <- rnorm(n, mu_1, sigma_1)
    x <- ifelse(y == 1, f_1, f_0)
    
    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
    
    dat <- list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
                test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
    dat
}

mu <- seq(0,3,len=25)
accuracy<- dataset <- sapply(mu, function(d){
  data <- make_data(mu_1=d)
  train <- data[[1]]
  test <- data[[2]]
  glmfit <- train %>% glm(y~x, data=., family='binomial')
  pred_y_logits <- predict.glm(glmfit, test)
  pred_y <- ifelse(pred_y_logits>0.5, 1, 0)
  mean(pred_y == test$y)
})
accuracy
p <-  ggplot() + geom_point(aes(x=mu, y=accuracy),size=3)
p
