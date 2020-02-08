#load specific library
library("dslabs")

#load murders dataframe
data("murders")

#shape/structure of dataframe
str(murders)

#names of columns
names(murders)

#specific column
murders$state

#length of vector
state <- murders$state
length(state)

#datatype of object
class(state)
class(10)

#top 5 rows in dataframe
head(murders)

#character vectors
name <- "Rahul"
print(name)

#logical vectors
z <- 1==0
print (z)
print (class(z))

#factors vectors for categorical data
print (class(murders$region))
print (levels(murders$region))

#number of factor vectors
print (nlevels(murders$region))

# Create data frame with city names and temperature 
temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
city_temps <- data.frame(name = city, temperature = temp)
