library(tidyverse)

#change working directory and list files
setwd('C:\\MachineLearning\\repos\\harvardx\\Data-Science-With-R\\R_DataWrangling\\data')
cwd <- getwd()
list.files(cwd)

#read csv file
data <- read_csv('life-expectancy-and-fertility-two-countries-example.csv')

#select 7 columns and their values
select(data,1:7)

#gather data
tidy_data <- data %>% gather (key, value, -country)

head (tidy_data)

#split the data into year and fertility/life_expectancy value
clean_data <- tidy_data %>% separate(key, c('year','variable_name'), extra='merge')

head (clean_data)

#spread the data so that fertility and life expectancy values
final_data <- clean_data %>% spread(variable_name, value)

head (final_data)

#trying same with unite function
tidy_data <- data %>% gather (key, value, -country)
head (tidy_data)

clean_data <- tidy_data %>% separate(key, c('year','first_variable_name','second_variable_name'), fill='right')
head (clean_data)

united_data <- clean_data %>% unite(variable_name, first_variable_name, second_variable_name, sep='_') 
head (united_data)

final_data <- united_data %>% spread (variable_name, value)
head (final_data)

final_data <- final_data %>% rename(fertility=fertility_NA)
head (final_data)



############ Tidying up Times related data#################
times <- read_csv('times.csv')
times

tidy_data <- times %>% gather(year, time, '2015':'2017')
tidy_data

tidy_data %>% spread(year,time)


############ Tidying up diseases related data#################
data <- read_csv('diseases.csv')
data

#tidy_data <- data %>% gather (key=disease, value=count, -state,-year,-population)
tidy_data <- data %>% gather (key=disease, value=count, HepatitisA:Rubella)
tidy_data

############ Tidying up population related data#################
data <- read_csv('population.csv')
data

data %>% spread(key = var, value = people)

############ Tidying up population related data#################
data <- read_csv('times1.csv')
data

data1 <- data %>% gather(key='key', value='value',-age_group)
data1
data2 <- data1 %>% separate(col=key,c('year','variable_name'), sep ='_')
data2
data3 <- data2 %>% spread(key=variable_name, value=value)
data3


###############Tidy up basketball data#################
data <- read_csv('bb.csv')
data

data %>% separate(col='key', into=c('player','variable_name')) %>% spread(key=variable_name, value=value)

###############examing co2 dataset #############
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

#################################
library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
dat

dat %>% spread(key=gender, value=admitted)

tmp <- gather(admissions, key, value, admitted:applicants)
tmp

tmp1 <- tmp %>% unite(column_name, c(key,gender))
tmp1

tmp1 %>% spread(key=column_name, value=value)
