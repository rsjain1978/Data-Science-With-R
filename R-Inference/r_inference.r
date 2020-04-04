library (tidyverse)
library (dslabs)
ds_theme_set()
take_poll(25)
take_poll(25)
take_poll(25)

# Suppose we want to caculate the Standard Error & Margin of Error for a poll result
# with 20 people polling, here we assume that the propotion of party A getting vote is 0.48
N <- 20
p <- 0.48
se <- sqrt (p*(1-p)/N)
print (se)
print (se*100)

# We will run a Monte Carlo Simulation to see if we get similar results.
p <- 0.45
B <- 100
N <- 20
X_hat = replicate(B, {
  X <- sample(c(0,1), N, replace=TRUE, prob=c(p,1-p))
  mean (X)
})
x_avg = mean (X_hat)
print (x_avg)

x_se = sqrt (p*(1-p)/N)
print (x_se)

hist (X_hat)
qqplot(X_hat)

# Consider following experiment
# From a population we draw a sample of size N, call that random variable X
# X = 1 with p=0.45
# X = 0 with p=1-p, .55
# From that sample we get some estimated average value of X
# We eventually would want to predict 'p' through X

p <- 0.45
N <- 100 

print_interval <- function (){
  X <- sample (c(1,0), N, prob=c(p, 1-p), replace=TRUE)
  mean <- mean (X)
  sd <- sd (X)
  se <- sqrt (p*(1-p)/N)
  
  cat ('mean is ',mean)
  cat ('sd is ', sd)
  cat ('se is ', se)
  cat ('95% confidence interval is (', (mean-2*se),',',(mean+2*se),')')
}

print_interval()
print_interval()


###Sample 2:
d <- 0.039 # spread is known, d=2p-1 hence p=(d+1)/2
p <- (d+1)/2
N <- c(1298)

confidence_intervals_a1 <- sapply(N, function(N){
  X = sample (c(0,1), size=N, prob = c(p,1-p), replace=TRUE)
  X_hat = mean (X)
  X_se = sqrt (X_hat * (1-X_hat)/N)
  2*c(X_hat-2*X_se, X_hat+2*X_se) -1
})
print (confidence_intervals_a1)


