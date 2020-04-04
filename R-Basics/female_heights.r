#load specific library
library("dslabs")

#load heights data
data ('heights')

# print first five records
head (heights)

# filter out female height records
heights_of_females <- filter (heights, sex=='Female')

# print first five records for female heights
head (heights_of_females)

# average height
avg <- mean (heights$height)

# count of male & female above average height
above_avg <- sum (heights$height > avg)
print (above_avg)

# index of records where height is greater than average
above_avg_heights_index <- which (heights$height > avg)

