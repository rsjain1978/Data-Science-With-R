library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

head(titanic_train)
str(titanic_train)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

head(titanic_clean)
nrow(titanic_clean)
ncol(titanic_clean)

#Split titanic_clean into test and training sets - after running the setup code, it should have 891 rows and 9 variables.
#Set the seed to 42, then use the caret package to create a 20% data partition based on the Survived column. Assign the 20% partition to test_set and the remaining 80% partition to train_set.
#How many observations are in the training set?
set.seed(42, sample.kind="Rounding") #if you are using R 3.6 or later

test_index <- createDataPartition(titanic_clean$Survived,times=1,p=0.2,list=FALSE)
test <- titanic_clean[test_index,]
train <- titanic_clean[-test_index,]
nrow(test)
nrow(train)

mean(train$Survived==1)

#What proportion of training set females survived?
num_males <- sum(train$Sex=='male')
num_females <- sum(train$Sex=='female')
train %>% summarise(sum(Sex=='male' & Survived==1)/num_males )
train %>% summarise(sum(Sex=='female' & Survived==1)/num_females)

#Train a logistic regression model with the caret glm method using age as the only predictor.
#What is the accuracy on the test set using age as the only predictor?
set.seed(1, sample.kind="Rounding") #if you are using R 3.6 or later
train_glm <- train(Survived~Age, method='glm', data=train)
pred_survived <- predict(train_glm, test)
confusionMatrix(pred_survived, test$Survived)$overall["Accuracy"]

head(train)
train_glm <- train(Survived~Sex+Pclass+Fare+Age, method='glm', data=train)
pred_survived <- predict(train_glm, test)
confusionMatrix(pred_survived, test$Survived)$overall["Accuracy"]

train_glm <- train(Survived~., method='glm', data=train)
pred_survived <- predict(train_glm, test)
confusionMatrix(pred_survived, test$Survived)$overall["Accuracy"]

#Set the seed to 6. Train a kNN model on the training set using the caret train function. Try tuning with k = seq(3, 51, 2).
set.seed(6, sample.kind="Rounding") #if you are using R 3.6 or later
train_knn <- train(Survived~., method='knn', data=train, tuneGrid = data.frame(k = seq(3,51,2)))
ggplot(train_knn, highlight = TRUE)
train_knn$bestTune
