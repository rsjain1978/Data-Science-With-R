#Assume a patient comes into the doctor's office to test whether they have a particular disease.

#The test is positive 85% of the time when tested on a patient with the disease (high sensitivity):  P(test+|disease)=0.85 
#The test is negative 90% of the time when tested on a healthy patient (high specificity):  P(test???|heathy)=0.90 
#The disease is prevalent in about 2% of the community:  P(disease)=0.02 
#Using Bayes' theorem, calculate the probability that you have the disease if the test is positive.

p_disease <- 0.02
p_positive_test_given_disease <- 0.85
p_negative_test_given_healthy <- 0.90
p_positive_test_given_healthy <- 0.10

p_disease_given_test_healthy <- 0

#Baye's Theorem says P(A/B) = P(B/A).P(A)/P(B), here
# A - Disease
# B - Positive Test
# A/B - Disease if Positive test
# B/A - Positive Test if Disease

p_positive_test <- p_positive_test_given_disease*p_disease + p_positive_test_given_healthy*(1-p_disease)
paste ('probability of positive test whether healthy/unhealthy is',p_positive_test)

p_disease_given_test_positive <- p_positive_test_given_disease*p_disease/p_positive_test
paste ('probability of disease given positive test ',p_disease_given_test_positive)

#####----------------------------------------------------------------------------------------------------------------------------

#We have a hypothetical population of 1 million individuals with the following conditional probabilities as described below:
#The test is positive 85% of the time when tested on a patient with the disease (high sensitivity):  P(test+|disease)=0.85 
#The test is negative 90% of the time when tested on a healthy patient (high specificity):  P(test???|heathy)=0.90 
#The disease is prevalent in about 2% of the community:  P(disease)=0.02

set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

#What is the probability that a test is positive?
mean(test)

#What is the probability that an individual has the disease if the test is negative?
mean(disease[test==0])

#What is the probability that you have the disease if the test is positive?
mean(disease[test==1]==1)

mean(disease[test==1]==1)/mean(disease==1)


#-----------------------------------------------------------------------------------------------------------------------------------
library(dslabs)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)

ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
Sigma
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
data.frame() %>% setNames(c("x", "y"))
plot(dat)

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps))) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)
