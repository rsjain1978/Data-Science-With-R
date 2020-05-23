library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type
x

#The type column of dat indicates whether students took classes in person ("inclass") or online ("online"). What proportion of the inclass group is female? What proportion of the online group is female?
inclass_students<-dat %>% filter(type=='inclass') %>% count()
inclass_fml_students<-dat %>% filter(type=='inclass' & sex=='Female') %>% count()
inclass_fml_students/inclass_students

online_students<-dat %>% filter(type=='online') %>% count()
online_fml_students<-dat %>% filter(type=='online' & sex=='Female') %>% count()
online_fml_students/online_students

#In the course videos, height cutoffs were used to predict sex. Instead of using height, use the type variable. Use what you learned about Q1 to make an informed guess about sex based on the most prevalent sex for each type. Report the accuracy of your prediction of sex based on type. You do not need to split the data into training and test sets.
y_hat <- ifelse(x == "online", "Male", "Female") %>% factor(levels = levels(y))
mean (y_hat==dat$sex)

y_hat <- ifelse(x == "inclass", "Female", "Male") %>% factor(levels = levels(y))
mean (y_hat==dat$sex)

#Write a line of code using the table() function to show the confusion matrix between y_hat and y. Use the exact format function(a, b) for your answer and do not name the columns and rows.
table(y_hat, y) #reveals problem with model

#What is the sensitivity, specificity and prevalence of this prediction? You can use the sensitivity() function from the caret package. Enter your answer as a percentage or decimal (eg "50%" or "0.50") to at least the hundredths place.
confusionMatrix(data=y_hat, reference = y)
