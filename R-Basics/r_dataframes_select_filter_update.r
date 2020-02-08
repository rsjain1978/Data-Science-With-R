library('dplyr')

#mutate - add column
#select - 
#filter - 

library('dslabs')
data('murders')

# Mutate to add new columns to dataframe
str(murders)
murders <- mutate(murders, rate=total/population*100000)
str(murders)
head (murders)

# filter based on columnar condition
filter (murders, rate<= 0.6)

# select
new_table <- select (murders, state, region, rate)
head (new_table)

# Create a new data frame called murders_nw with only the states from the northeast and the west
murders_nw <- filter (murders, region %in% c('Northeast','West'))

# using these together
select (filter (murders, rate < 0.6), state, abb)

# add the rate column
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))

# Create a table, call it my_states, that satisfies both the conditions 
my_states <- filter (murders, region %in% c('Northeast','West') & rate<1)
# Use select to show only the state name, the murder rate and the rank
select (my_states, state, rate, rank)


# Create new data frame called my_states (with specifications in the instructions)
my_states <- murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate)) %>% filter (region %in% c('Northeast','West') & rate<1) %>% select(rate, rank)

#piping functions
murders %>% select (state, abb, rate) %>% filter (rate >3 & rate <3.2)

#create data frames
grades <- data.frame (names=c('rahul','surbhi'),
                      age=c(42,40),
                      stringAsFactors=FALSE)
head(grades)

class (grades$age)
class (grades$names)
