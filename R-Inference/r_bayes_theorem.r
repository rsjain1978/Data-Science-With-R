B <- 1
p <- .00025
N <- 10000

outcomes <- replicate (B, {
  sample_outcomes <- sample (c('Disease','Healthy'), N, prob = c(p,1-p), replace = TRUE)
  sample_outcomes
})

N_D = sum (outcomes == 'Disease')
N_H = sum (outcomes == 'Healthy')

print (N_D)
print (N_H)

# Find the probability of disease is test is positive
accuracy <- 0.99

test <- vector('character',N)
test [outcomes == 'Disease'] <- sample(c('+','-'), N_D, replace=TRUE, prob=c(accuracy, 1-accuracy))
test [outcomes == 'Healthy'] <- sample(c('-','+'), N_H, replace=TRUE, prob=c(accuracy, 1-accuracy))
table (outcomes, test)
