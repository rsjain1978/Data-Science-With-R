library(caret)
library(dplyr)

set.seed(1, sample.kind="Rounding") # set.seed(1, sample.kind="Rounding") if using R 3.6 or later

num <- c(100, 500, 1000, 5000, 10000)
ret <- sapply(num,function(n) {

  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  rmse<- replicate(100,{
    test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
    test <- dat[test_index,]
    train <- dat[-test_index,]
    fit <- lm(y ~ x, data = train)
    y_hat <- predict(fit, newdata = test)
    sqrt(mean((y_hat-test$y)^2))
  })
  
  c(avg = mean(rmse), sd = sd(rmse))
})

ret

#----------------------------------------------------
set.seed(1)
num <- c(100)

ret <- sapply(num, function(num) {
  
  Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  rmse<- replicate(100,{
    test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
    test <- dat %>% slice(test_index)
    train <- dat %>% slice(-test_index)
    fit <- lm(y ~ x, data = train)
    y_hat <- predict(fit, newdata = test)
    sqrt(mean((y_hat-test$y)^2))
  })
  
  c(avg = mean(rmse), sd = sd(rmse))
})

print (ret)

#-----------------------
set.seed(1)
sample.kind="Rounding"
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)

test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
test <- dat %>% slice(test_index)
train <- dat %>% slice(-test_index)

fit <- lm(y ~ x_1, data = train)
y_hat <- predict(fit, newdata = test)
sqrt(mean((y_hat-test$y)^2))


fit <- lm(y ~ x_2 data = train)
y_hat <- predict(fit, newdata = test)
sqrt(mean((y_hat-test$y)^2))


fit <- lm(y ~ x_1+x_2, data= train)
y_hat <- predict(fit, newdata = test)
sqrt(mean((y_hat-test$y)^2))
