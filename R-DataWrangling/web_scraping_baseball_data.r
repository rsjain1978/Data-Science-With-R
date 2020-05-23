#load libraries
library(tidyverse)
library (rvest)

#specify url to be scrapped
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"

#read the html from url
h <- read_html(url)

#read all tables
nodes <- html_nodes(h, "table")

#convert first, 2nd,3rd and 4th tables to dataframe and see their content
head (html_table(nodes[[1]]))
head (html_table(nodes[[2]]))
head (html_table(nodes[[3]]))
head (html_table(nodes[[4]]))

html_text(nodes[[20]])
head (html_table(nodes[[20]]))
head (html_table(nodes[[19]]))
head (html_table(nodes[[18]]))

#create dataframes from two tables
tab_1 <- html_table(nodes[[10]])
tab_2 <- html_table(nodes[[19]])

str(tab_1)
str(tab_2)

#drop specific column from one dataframe
keeps <- c("X2", "X3", "X3")
tab_1 <- tab_1[keeps]

#rename columns
tab_1 <- tab_1 %>% setNames(c('team','payroll','average'))
tab_2 <- tab_2 %>% setNames(c('team','payroll','average'))

#drop first row
tab_1 <- tab_1[-c(1),]
tab_2 <- tab_2[-c(1),]

combined_table <- tab_1 %>% full_join(tab_2, by='team')
nrow(combined_table)
