#---------------Normal Distributions-------------
#create normal distribution
simulated_heights = rnorm(10000, 1,4)

#histogram for normal distribution
data.frame(sh=simulated_heights)%>%ggplot(aes(sh))+geom_histogram(binwidth = 1)

#pdf (probabilty density function) for normal distribution
pdf <-dnorm(simulated_heights, 1,4)
data.frame (simulated_heights, pdf)%>%ggplot(aes(simulated_heights,pdf))+geom_line()

#Run a monte carlo simulation (10000) times to see the probability
#of findnig a person whose height is >7 ft if all heights are normally
#distributed with mean 5 ft and standard deviation of 0.5
B<-10000
all_max_heights <- replicate(B, {
  heights <- rnorm(100,5,.5)
  max(heights)
})
mean (all_max_heights>7)

#---------------Poisson Distributions-------------
#generate a poisson distribution
bookings_distribution = rpois(200,.8)

#visualizing poisson distribution
data.frame(bookings=bookings_distribution)%>%ggplot(aes(bookings))+geom_histogram()

#plotting pdf for poisson distribution
pdf_p <- dpois(bookings_distribution,.8)
data.frame (simulated_heights, pdf_p)%>%ggplot(aes(simulated_heights,pdf_p))+geom_line()


---------------------------
#Assume the distribution of female heights is approximated by a normal distribution with a mean of 64 inches and a standard deviation of 3 inches. If we pick a female at random, what is the probability that she is 6 feet or taller?
female_avg <- 64
female_sd <- 3
1-pnorm(6*12,64,3)

#Assume the distribution of female heights is approximated by a normal distribution with a mean of 64 inches and a standard deviation of 3 inches. If we pick a female at random, what is the probability that she is between 61 and 67 inches?
female_avg <- 64
female_sd <- 3
pnorm (67,64,3) - pnorm (61,64,3)

#Compute the probability that the height of a randomly chosen female is within 1 SD from the average height.
female_avg <- 64
female_sd <- 3
taller <- female_avg + 1*female_sd
shorter <- female_avg - 1*female_sd
pnorm (taller, female_avg, female_sd) - pnorm (shorter, female_avg, female_sd)

#Imagine the distribution of male adults is approximately normal with an average of 69 inches and a standard deviation of 3 inches. How tall is a male in the 99th percentile?
male_avg <- 69
male_sd <- 3
qnorm (0.99, male_avg, male_sd)

#The distribution of IQ scores is approximately normally distributed. The average is 100 and the standard deviation is 15. Suppose you want to know the distribution of the person with the highest IQ in your school district, where 10,000 people are born each year.
#Generate 10,000 IQ scores 1,000 times using a Monte Carlo simulation. Make a histogram of the highest IQ scores

# The variable `B` specifies the number of times we want the simulation to run.
B <- 1000

# Use the `set.seed` function to make sure your answer matches the expected result after random number generation.
set.seed(1)

# Create an object called `highestIQ` that contains the highest IQ score from each random distribution of 10,000 people.
highestIQ <- replicate(B,{
  scores<- rnorm(10000,100,15)
  max (scores)
})

# Make a histogram of the highest IQ scores.
hist(highestIQ)


set.seed(16) 
act_scores <- rnorm(10000, 20.9, 5.7) 
mean(act_scores) 
sd(act_scores)

sum(act_scores<=10)/10000

x <- seq(1,36)
f_x<-dnorm(x, 20.9, 5.7)
plot(x,f_x)

qnorm(.95, 20.9, 5.7)
