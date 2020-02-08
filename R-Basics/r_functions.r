x <- 1:100

avg <- function (X) {
  sum_ <- sum(X)
  length_ <- length(X)
  sum_/length_
}

avg (x)
mean (x)

#for loop
compute_sum <- function (n){
  x <- 1:n
  sum (x)
}

compute_sum (10)

n <-25
empty_v <- vector (length=n)
for (i in 1:n) {
  empty_v[i] <- compute_sum(i)
}
print (empty_v)
sum (empty_v)

m <- 1:n
plot (m, empty_v)
lines (m, m*(m+1)/1.9)

# Write a function compute_s_n with argument n that for any given n computes the sum of 1 + 2^2 + ...+ n^2
compute_s_n <- function (n){
  x <- 1:n
  x_sqr <- x^2
  sum (x_sqr)
}
# Report the value of the sum when n=10
compute_s_n (10)


# Assignment
library(dslabs)
data(heights)

result <-ifelse (heights$sex=='Female',1,2)
print (result)
sum (result)

result <-ifelse (heights$height>72,heights$height,0)
print (result)
mean(result)

inches_to_ft <- function (x){
  x <- x / 12
}
print (inches_to_ft(144))


nrow (filter (heights, inches_to_ft(height)<5))
