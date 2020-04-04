# generate random variable generator
beads <- rep (c('red','blue'), times=c(1,3))

# generate random variable, if its 'red' assign 1 else 0
x <- ifelse(sample (beads, 1)=='red',1,0)

# print the random variable x
print (x)

#---------------Sampling Methods-----------------#

#------------Code to simulate the sum a casino makes if 1000 people play a game of roulette
color <- rep (c('red','black','green'), times=c(18,18,2))
sample (color,1)

n<- 1000
X<-sample (ifelse(color=='red',-1,1),n, replace=TRUE)
S <- sum(X)
print (S)

X<-sample (c(-1,1),1000, replace=TRUE, prob=c(9/19,10/19))
print (sum(X))
#------Monte Carlo simulation to find the casion earnings by running experiment say 10000 times

B <- 3000000
n <- 1000

casino_savings <- replicate (B, {
  X<-sample (c(-1,1),n, replace=TRUE, prob=c(9/19,10/19))
  sum(X)
})
mean (casino_savings>50 & casino_savings<100)

h <- hist(casino_savings, breaks = 100, plot=FALSE)
h$counts=h$counts/sum(h$counts)
plot(h)

mean (casino_savings)
sd (casino_savings)

# P(X=x), this represent 'What is the Probability of X being x'
# X, represents the random variable
# x, represents the value of that variable, say 'value that comes on a dice'