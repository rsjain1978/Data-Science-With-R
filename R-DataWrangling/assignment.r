##Sample dataframe
dayv <- c('Monday','Tuesday')
staffv <- c('Mandy, Chris and Laura','Steve, Ruth and Frank')
dat <- data.frame(day=dayv, staff=staffv)

str_split(dat$staff, pattern = ', | and')
str_split(dat$staff, pattern = ',\\s|\\sand\\s')

head (dat)

#convert to tidy format dataframe
dat %>% mutate(staff = str_split(dat$staff, pattern = ',\\s|\\sand\\s')) %>% unnest()

#Using the gapminder data, you want to recode countries longer than 12 letters in the region "Middle Africa" to their abbreviations in a new column, "country_short". Which code would accomplish this?
dat1 <- gapminder %>% filter(region == "Middle Africa") %>% 
  mutate(country_short = recode(country, 
                                "Central African Republic" = "CAR", 
                                "Congo, Dem. Rep." = "DRC",
                                "Equatorial Guinea" = "Eq. Guinea"))
head (dat1)
dat1$country_short


#Import raw Brexit referendum polling data from Wikipedia:
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)
nrow(polls)
str(polls)
polls <- polls%>% setNames(c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes"))
str(polls)

#remove those rows where remain column does not have percentage
polls <- polls %>% filter(str_detect(polls$remain, '%'))
nrow(polls)

parse_number(polls$Remain)/100
parse_number(polls$remain)/100

#The undecided column has some "N/A" values. These "N/A"s are only present when the remain and leave columns total 100%, so they should actually be zeros.
#Use a function from stringr to convert "N/A" in the undecided column to 0. The format of your command should be function_name(polls$undecided, "arg1", "arg2").

unique(polls$undecided)
polls$Undecided <- str_replace(polls$Undecided,"N/A","0") #original value does not changes


#parse dates
temp <- str_extract_all(polls$dates, "\\d+\\s[a-zA-Z]+")
temp <- str_extract_all(polls$dates, "[0-9]+\\s[a-zA-Z]+")
temp <- str_extract_all(polls$dates, "\\d{1,2}\\s[a-zA-Z]+")
temp <- str_extract_all(polls$dates, "\\d+\\s[a-zA-Z]{3,5}")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
end_date
