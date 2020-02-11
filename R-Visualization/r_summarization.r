library ('dplyr')
library (tidyverse)

data ("heights")

summ <- heights %>% filter (sex=='Male') %>% summarize (average = mean(height),
                                                std = sd (height))

print (summ)
print (summ$average)
print (summ$std)

#group by
gb <- heights %>% group_by(sex)

data('murders')
murders %>% group_by (region) %>% summarise(median(total))

#sorting/ordering by field
murders %>% arrange(total) %>%head()

#sorting and specifying order
murders %>% arrange(desc(total)) %>%head()

#sorting on multiple fields
murders %>% arrange (region, total) %>% head()

#use top_n to select top n rows
murders %>% arrange(desc(total)) %>%top_n(10)


#install.packages('NHANES')
library(NHANES)
data(NHANES)

# This dataset has NA values so below two calls with fail
mean(na_example)
sd(na_example)

mean (na_example, na.rm = TRUE)
sd (na_example, na.rm = TRUE)

#We will be exploring blood pressure in this dataset.
names (NHANES)

#selecting details for females in 20-29 age category
tab <- NHANES %>% filter (AgeDecade ==' 20-29' & Gender =='female')

#Mean and SD of BP handling NA
ref <- NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") %>% summarize (mean(BPSysAve, na.rm=TRUE),sd(BPSysAve, na.rm=TRUE))

NHANES %>% filter (Gender='male') %>% group_by(AgeDecade) %>% summarize (average = mean(BPSysAve, na.rm=TRUE), standard_deviation=sd(BPSysAve, na.rm=TRUE))

#compare systolic blood pressure across values of the Race1 variable for males between the ages of 40-49.
NHANES %>% arrange(desc(Race1)) %>% filter (AgeDecade==' 40-49') %>% group_by (Race1) %>% summarize (average = mean(BPSysAve,na.rm=TRUE), standard_deviation=sd(BPSysAve,na.rm=TRUE))