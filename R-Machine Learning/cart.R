# Load data
library(tidyverse)
library(dslabs)
library(caret)

data('mnist_27')

head(mnist_27)
class (mnist_27)

train <- mnist_27[[1]]
test <- mnist_27[[2]]

##------------------Decison Trees
# fit a classification tree and plot it
train_rpart <- train(y ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = mnist_27$train)
plot(train_rpart)

# compute accuracy
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

#plot conditional probality plot
plot_cond_prob(predict(train_rpart, mnist_27$true_p, type = "prob")[,2])

##------------------Random Forest
library(randomForest)
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# use cross validation to choose parameter
train_rf_2 <- train(y ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

plot_cond_prob(predict(train_rf, mnist_27$true_p, type = "prob")[,2])

##--------------
library(rpart)
library(dplyr)
library(ggplot2)

#Create a simple dataset where the outcome grows 0.75 units on average for every increase in a predictor, using this code:
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

fit <- rpart(y ~ ., data = dat) 

plot(fit)
text(fit)

#Below is most of the code to make a scatter plot of y versus x along with the predicted values based on the fit.
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)

#Now run Random Forests instead of a regression tree using randomForest() from the randomForest package, and remake the scatterplot with the prediction line. Part of the code is provided for you below.
library(randomForest)
fit <- randomForest(y ~ x, data = dat)

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

#Use the plot() function to see if the Random Forest from Q4 has converged or if we need more trees.
plot(fit)

#It seems that the default values for the Random Forest result in an estimate that is too flexible (unsmooth). Re-run the Random Forest but this time with a node size of 50 and a maximum of 25 nodes. Remake the plot.
library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodes = 50, max = 25)

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

plot(fit)
