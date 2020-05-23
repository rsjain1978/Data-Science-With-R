# create the dataset
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1983)

names(GaltonFamilies)

galton_heights <- GaltonFamilies %>% filter(gender=='male') %>%
                                      group_by(family) %>%
                                      sample_n(1) %>%
                                      ungroup() %>%
                                      select(father, childHeight) %>%
                                      rename(son=childHeight)
galton_heights

#mean and standard deviations
summary_df <- galton_heights %>% summarise(mean(father), sd(father), mean(son), sd(son))
summary_df

# scatterplot of father and son heights (some correlation but no huge)
galton_heights %>% ggplot(aes(father, son)) + geom_point(alpha=0.5)

cor(galton_heights$father, galton_heights$son) #print correlation b/w two

correlation <- galton_heights %>% summarise(r=cor(father, son))
correlation


#####################################################################################

#Take sample from heights data
sample_heights <- sample_n(galton_heights, 25, replace = TRUE) #take sample
sample_heights %>% summarise(mean(father), sd(father), mean(son), sd(son)) #mean and sd of sample
cor(sample_heights$father, sample_heights$son) # print correlation
sample_heights %>% summarise(r=cor(father, son)) %>% pull(r) # print correlation from table

#Run monte carlo simulation for 1000 such experiments. After the end of simulations we will
#get B number of correlation values, plot a histogram of those correlation values and observe
#the distribution
B <- 1000
N <- 50

correlations_dist <- replicate(B,{
  sample <- sample_n(galton_heights, N, replace = TRUE) %>%
  summarise(r=cor(father, son)) %>% pull(r)
})

#plot histogram of observed correlation values - normally distributed
qplot(correlations_dist, geom = "histogram", binwidth = 0.05, color = I("black"))

#mean and standard error of the distribution
mean (correlations_dist) #expected value of the population mean
sd(correlations_dist) #expected value of the population standard distribution

# QQ-plot to evaluate whether N is large enough
data.frame(correlations_dist) %>%
  ggplot(aes(sample = correlations_dist)) +
  stat_qq() +
  geom_abline(intercept = mean(correlations_dist), slope = sqrt((1-mean(correlations_dist)^2)/(N-2)))


###########Correlation in Baseball data
library(Lahman)

#What is the correlation coefficient between number of runs per game and number of at bats per game?
filtered_data <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(num_bats_per_game = AB/G, runs_per_game = R/G)
cor(filtered_data$num_bats_per_game, filtered_data$runs_per_game)

#What is the correlation coefficient between win rate (number of wins per game) and number of errors per game?
filtered_data <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(wins_per_game = W/G, errors_per_game = E/G)
cor(filtered_data$wins_per_game, filtered_data$errors_per_game)

#What is the correlation coefficient between doubles (X2B) per game and triples (X3B) per game?
filtered_data <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(triples_per_game = X3B/G, doubles_per_game = X2B/G)
cor(filtered_data$triples_per_game, filtered_data$doubles_per_game)


################################

head(GaltonFamilies)

galton_heights_mothers <- GaltonFamilies %>% filter(gender=='female') %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(mother, childHeight) %>%
  rename(daughter=childHeight)

mu_m <- mean(galton_heights_mothers$mother)
sigma_m<-sd(galton_heights_mothers$mother)

mu_d <- mean(galton_heights_mothers$daughter)
sigma_d <- sd(galton_heights_mothers$daughter)

rho <- cor(galton_heights_mothers$mother, galton_heights_mothers$daughter)
cor(galton_heights_mothers$daughter, galton_heights_mothers$mother)

rho <- 0.325
#Calculate the slope and intercept of the regression line predicting daughters' heights given mothers' heights. 
#Given an increase in mother's height by 1 inch, how many inches is the daughter's height expected to change?

# Slope of regression line predicting daughters' height from mothers' heights
rho * sigma_d / sigma_m

#Intercept of regression line predicting daughters' height from mothers' heights
mu_d - rho * sigma_d / sigma_m * mu_m

# Change in daughter's height in inches given a 1 inch increase in the mother's height
rho * sigma_d / sigma_m

#What percent of the variability in daughter heights is explained by the mother's height?
rho^2 * 100

#A mother has a height of 60 inches.
#What is the conditional expected value of her daughter's height given the mother's height?
x_ <- (60 - mu_m) / sigma_m
y <- mu_d + rho* sigma_d * x_
y
