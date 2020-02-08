# scatter plots
library('dslabs')
data('murders')

# scatter plot
pop_in_mn <- murders$population/1000000
murders_ct <- murders$total
plot(pop_in_mn, murders_ct)

#histogram
hist(murders_ct)

# Create a boxplot of state populations by region for the murders dataset
boxplot(population~region, data=murders)

library(dslabs)
data(heights)
options(digits = 3) 

head(heights)
#describe dataframe
str(heights)

#find mean
mean(heights$height)

#find entries bigger than mean
ind <- heights$height>mean(heights$height)
print (sum(ind))

ind <- heights$height>mean(heights$height) & heights$sex =='Female'
print (sum(ind))

ind <- heights$sex =='Female'
print (mean(ind))

min (heights$height)
which.min(heights$height)
heights$sex[1032]

max (heights$height)

x <- as.integer(min(heights$height)):as.integer(max(heights$height))
print (x)

tmp <- filter (heights, height %in% x)
nrow (tmp)

print (nrow(heights)-nrow(tmp))

mean(heights2$height)*2.54
head (heights)
heights2 <- filter (heights, sex=='Female')
nrow(heights2)


library(dslabs)
data(olive)
head(olive)
plot(olive$palmitic, olive$palmitoleic)

hist(olive$eicosenoic)

boxplot(palmitic~region, data=olive)
