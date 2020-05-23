#Handling double quotes in string
#s <- 10" # throws error

#representing 10 inches
s <- '10"'
cat (s) #print value of string

#representing 10 feet
s <- "10'"
cat (s)

#representing 10 feet 10 inches, use backslash to escape.
s <- "10' 10\""
cat (s)

# load stringr package
library (stringr)

#assume some string value scrapped from web, say 10,300
s <- '10,300'
class(s)

snum <- as.integer(s) #try coercion here and it would fail because of comma
snum

###################Scrap Murders Data #############################
#load libraries
library(tidyverse)
library (rvest)

#specify url to be scrapped
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"

#read the html from url
h <- read_html(url)

#extract all tables from read html
tabs <- h %>% html_nodes('table')

#extract table of interest
tab <- tabs[[2]]

#convert table to dataframe
tab <- tab %>% html_table
class(tab)

#see column names
str(tab)

#rename columns
murders_raw <- tab %>% setNames(c('state','population','total','murders','gun_murders','gun_ownership','total_rate','murder_rate','gun_murder_rate'))

str(murders_raw)
###################Scraping for Murders Data ends#############################

commas <- function(x)
{
  any(str_detect(x,','))
}

# check if population column has commas
commas(murders_raw$population)

# check all columns for commas
murders_raw %>% summarise_all(commas)

# replace commas from population column
test1 <- str_replace_all(murders_raw$population, ',','')
class(test1) #replacing commas does not change column type

test1 <- as.numeric(test1)
class (test1)

#all of above steps could be done with single in-built function called parse_number
test2 <- parse_number(murders_raw$population)

test2s
test1
murders_raw$population

identical(test1, test2)
s
#apply parse_numbers to all specified columns
head (murders_raw)
cleaned_murders <- murders_raw %>% mutate_at(2:3, parse_number)
head (cleaned_murders)
class (cleaned_murders$population)


cleaned_murders_1 <- murders_raw %>% mutate_at(2:3, as.numeric)s
head (cleaned_murders_1)
class (cleaned_murders_1$population)
