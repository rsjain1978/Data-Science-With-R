library(dslabs)
data('polls_us_election_2016')

polls_us_election_2016$startdate %>% head
polls_us_election_2016$startdate %>% head() #dates look like strings
class(polls_us_election_2016$startdate) #class type is Date

as.numeric(polls_us_election_2016$startdate) %>% head #since for Date epoch is Jan 1 1970, every day after that gets incremented by 1

############Use Lubridate library to get Days, Months, Year and Day from any DATE##############
library(lubridate)
sample_dates <- sample(polls_us_election_2016$startdate,10) %>% sort
sample_dates

day(sample_dates)
month(sample_dates)
month(sample_dates, label = TRUE)
year(sample_dates)

###########Convert String to Date###############
d <- c("2016-07-15","2016-08-31","2016-09-07","2016-09-30","2016-10-17","2016-10-20","2016-10-24")
class (d)
d_hash <- ymd(d)
class (d_hash)
month(d_hash[1], label = TRUE)
d_hash[2]

# different parsers extract year, month and day in different orders
x <- "09/01/02"
ymd(x)
mdy(x)
ydm(x)
myd(x)
dmy(x)
dym(x)

# getting time zone
now()
now('GMT')

# get all time zones
OlsonNames()

# getting hours, minutes and times
now()
now() %>% hour()
now() %>% minute()
now() %>% second()

# converting string to times
t <- c("0:57:13")
hms(t)

# converting string with date and time 
dt <- c('12/01/01 12:01:30')
ymd_hms(dt)


###################Assignment#######################
library(dslabs)
library(lubridate)
options(digits = 3)    # 3 significant digits

data(brexit_polls)
sum (month(brexit_polls$startdate) == 4)
sum (month(brexit_polls$startdate))

table(weekdays(brexit_polls$enddate)) #table gives count, something like value_counts() in python

data(movielens)
movielens$timestamp %>% head

table(year(as_datetime(movielens$timestamp))) %>% sort
table(hour(as_datetime(movielens$timestamp))) %>% sort
