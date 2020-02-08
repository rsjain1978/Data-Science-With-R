library("dslabs")

dslabs::murders
print (murders$region)
names(murders)

?sort

#default sort - ascending
sort(murders$total)

x <- c(1,20,13,40,5)
print (sort(x))
index <- order(x)
print (index)
x[index]

#print 10 state names
print(murders$state[1:10])
print(murders$abb[1:10])

#order states on total number of murders
index <-order(murders$total)
print (index)
print (murders$abb[index])

#using max and which.max
max <- max(murders$total)
cat('max murders count is->',max)

index_max_murders <- which.max(murders$total)
cat ('state with max murders is->',murders$state[index_max_murders])

data("na_example")
print (na_example)
mean (na_example)
ind <-is.na(na_example)
print (ind)
mean(na_example[!ind])


x <- c(2, 43, 27, 96, 18)
print (order(x))
print (rank(x))
print (sort(x))
print (min(x))
which.min(x)
max(x)
which.max((x))


name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
time <- time/60
speed <- distance/time
print (speed)
