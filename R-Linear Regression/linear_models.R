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

galton_heights$son
galton_heights$father

#fit the data using lm() function
fit <- lm(son~father, data=galton_heights)
fit   #print intercept and value of scale (beta0 and beta1)

#print summary of fit
summary(fit)

#extract coeffecients
coefs <- fit %>% .$coef
coefs[1] #intercept
coefs[2] #slope


######################Monte Carlo Simulation ######################
B <- 10000
N <- 50

lse <- replicate(B, {
  sample_n(galton_heights, N, replace=TRUE) %>% 
    lm (son~father, data = .) %>% .$coef
})

lse_df <- data.frame(intercept=lse[1,], slope=lse[2,])
lse_df
head (lse_df)

#plot the disribution of these estimates - they would be normal (CLT)

library(gridExtra)
p1 <- lse_df %>% ggplot(aes(intercept)) + geom_histogram(binwidth = 5, color='black')
p2 <- lse_df %>% ggplot(aes(slope)) + geom_histogram(binwidth = 0.1, color='red')
grid.arrange(p1,p2)

summary(fit)
lse_df %>% summarise(mean_intercept=mean(intercept), mean_slope=mean(slope), se_intercept=sd(intercept), sd_slope=sd(slope))

#Conclusion -> If we take multiple samples for any population of X& Y. If we then calculate
#intercept and slope for each of that sample. Then they will follow normal distribution.
#We can then find out the expected value of mean and standard deviation from the values
#obtained in the distribution.

# plot predictions and confidence intervals
galton_heights %>% ggplot(aes(son, father)) +
  geom_point() +
  geom_smooth(method = "lm")

# predict Y directly
fit <- galton_heights %>% lm(son ~ father, data = .) 
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)
Y_hat

# plot best fit line
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data=.))) %>%
  ggplot(aes(father, Y_hat))+
  geom_line()


####################Assignment
beta1 = seq(0, 1, len=nrow(galton_heights))

results <- data.frame(beta1 = beta1,rss = sapply(beta1, rss, beta0 = 36))

results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

#Load the Lahman library and filter the Teams data frame to the years 1961-2001. 
#Run a linear model in R predicting the number of runs per game based on both the 
#number of bases on balls per game and the number of home runs per game.

library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

head (Teams)

lm_dataset <-Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(hr_per_game = HR/G, 
         runs_per_game = R/G,
         bb_per_game = BB/G) %>% select(hr_per_game, runs_per_game, bb_per_game)


fit <- lm_dataset %>% lm(runs_per_game~bb_per_game+hr_per_game, data = .)
fit


#################Assessment: Least Squares Estimates, part 2

set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

fit <- female_heights %>% lm (mother~daughter, data=.)
fit
coefs <- fit %>% .$coef
coefs

intercept <- coefs[1]
intercept

slope <- coefs[2]
slope

predicted_height <- intercept + slope*female_heights$mother[1]
predicted_height
female_heights$mother[1]

##############################Baseball model

library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_99_01 <- Batting %>% filter(yearID >= 1999 & yearID <=2001 ) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))

sum(bat_99_01$mean_singles >0.2)
sum(bat_99_01$mean_bb >0.2)

#Use inner_join() to combine the bat_02 table with the table of 1999-2001 rate averages you created in the previous question.
joined_table <- bat_99_01 %>% inner_join(bat_02,by = 'playerID')

head (joined_table)

cor(joined_table$mean_singles, joined_table$singles)
cor(joined_table$mean_bb, joined_table$bb)

#Make scatterplots of mean_singles versus singles and mean_bb versus bb.
joined_table %>% ggplot(aes(mean_bb, bb)) + geom_point(alpha=0.5)
joined_table %>% ggplot(aes(mean_singles, singles)) + geom_point(alpha=0.5)

#Fit a linear model to predict 2002 singles given 1999-2001 mean_singles.
fit <- joined_table %>% lm(singles~mean_singles, data=.)
fit
fit <- joined_table %>% lm(bb~mean_bb, data=.)
fit
