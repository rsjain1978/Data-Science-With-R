#load libraries
library(tidyverse)
library (rvest)

#specify url to be scrapped
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"

#read the html from url
h <- read_html(url)

#read all tables
tab <- html_nodes(h, "table")

head(html_table(tab[[1]], fill = TRUE))
head(html_table(tab[[2]], fill = TRUE))
head(html_table(tab[[3]], fill = TRUE))
str(html_table(tab[[4]], fill = TRUE))
str(html_table(tab[[5]], fill = TRUE))

