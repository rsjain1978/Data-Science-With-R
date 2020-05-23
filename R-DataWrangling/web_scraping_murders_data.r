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
tab <- tab %>% setNames(c('state','population','total','murders','gun_murders','gun_ownership','total_rate','murder_rate','gun_murder_rate'))

str(tab)
