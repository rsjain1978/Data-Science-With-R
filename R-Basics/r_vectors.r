# creating character vectors
names <- c('Rahul','Surbhi','Aditya','Sonali')
class (names)
print (names)

# creating numeric vectors
age <-c(1,2,3,4)
class (age)
print (age)

codes <- c(Rahul=1, Surbhi=2, Aditya=3, Sonali=4)
class (codes)
print (codes)

# generate sequences
seq1to10 <- seq(1,5)
print (seq1to10)

seq1to10 <- seq(1,5,2)
print (seq1to10)

# accessing parts of vector
seq1to10 <- seq(1,5)
print (seq1to10[1]) #first element
print (seq1to10[1:3]) #first, second and element
print (seq1to10[-1]) #all but first element
print (seq1to10[-3]) #all but third element
print (seq1to10[-1:-3]) #all but first, second and third element

# datatype conversion
x <- 1:5
print (class(x))
y <- as.character(x)
print (class(y))
