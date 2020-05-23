library(dslabs)
data(heights)

#extract names from dataframe
names(heights)

#frequency distribution (same as value_counts in python)
x <- heights$height
print (table(x))

print (table(heights$sex))

# find count of heights which have just 1 occurence
tab <- table(x)
sum (tab==1)

library(gtools)
nrow(permutations(6,2))
nrow(combinations(6,2))
