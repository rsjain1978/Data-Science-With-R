#get working directory
getwd()

#set working directory
setwd('C:\\MachineLearning\\repos\\harvardx\\Data-Science-With-R\\R_DataWrangling\\data')

#get working directory
current_directory <- getwd()

list.files(current_directory)

#path to the list of data files installed with dslabs
path <- system.file('extdata', package='dslabs')

#list files at specified path
list.files(path)

filename = file.path(current_directory,'olive.csv')

#check if certain file exists
file.exists(filename)

#read 5 lines from csv
read.csv('olive.csv',nrows = 5)

#read complete csv file
data <- read.csv('olive.csv',nrows = 5)

#print data
data

class(data)

#head over data
head (data)

# read using tidyverse
library(tidyverse)

data <- read_csv('olive.csv')
data
head(data)
class(data)


race_times <- read_csv("olive.csv", col_names = TRUE)
race_times
race_times <- read_delim("olive.csv", delim = ',')

#############Read excel#############
sheet_2016 <- read_excel('2010_bigfive_regents.xls', sheet=2)
sheet_2016
##########################Downloading Files from Internet##############################


#create a temp file name
tmp_file <- tempfile()

#some url
url <- 'http://google.com'

#download file
download.file(url, tmp_file)

dat <- read_csv(tmp_file)

#delete temp file
file.remove(tmp_file)

#######
url <- 'http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data'
data <- read_csv(url, col_names = FALSE)
nrow(data)
str(data)
dim(data)
