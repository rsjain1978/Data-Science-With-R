#conditional expressions
a <- 2
if (a!=0) {
  print (1/a)
}else {
  print ('no reciprocal')
}

library(dslabs)
data(murders)

min_murder_idx = which.min(murders$total)
murder_rate <- murders$total/murders$population*100000
if (murder_rate[min_murder_idx]<0.4){
  print ('State with minimum murder rate is below 0.2')
  print (murders$state[min_murder_idx])
}else{
  print ('State with minimum murder rate is not below 0.2')
}

# ifelse for single line query
a <-1
ans <- ifelse (a>0,'Hurray','Sorry')
print (ans)

a <- -2:2
print (a)
ans <- ifelse (a>0,'Positive','Negative')
print (ans)

data ("na_example")
sum(is.na(na_example))
no_na <- ifelse (is.na(na_example),0,na_example)
sum(is.na(no_na))

#'any' function checks if any value is TRUE
x <- c(1,3,1,'c')
print (is.character(x))

x <- c(TRUE, TRUE, FALSE)
any (x)

x <- c(FALSE, FALSE, FALSE)
any (x)

x <- c(FALSE, FALSE, FALSE)
all (x)

x <- c(TRUE, TRUE, FALSE)
all (x)

x <- c(TRUE, TRUE, TRUE)
all (x)
