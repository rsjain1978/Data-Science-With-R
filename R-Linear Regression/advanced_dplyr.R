library(Lahman)

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

#outcome of group_by & summarise is a dataframe called tibble
#tibble is like modern version of dataframe
dat %>% group_by(HR) %>% class()

#tibble Vs Dataframe

##tibbles are more readable
Teams #print Teams dataframe - long list of values
as_tibble(Teams) #convert dataframe to tibble - better readibility

##tibbles give warnings for wrong columns
Teams$hr #gives null
as_tibble(Teams)$hr #gives warning

library(broom)
fit <- lm(R~BB, data=dat)
fit
tidy(fit) #returns outcome of lm() as dataframe

dat %>% group_by(HR) %>%
         do(tidy(lm(R~BB, data=.),conf.int = TRUE)) %>%
         filter(term=='BB') %>%
         select(HR, term, estimate, conf.low, conf.high) %>%
         ggplot(aes(HR, y=estimate, ymin=conf.low, ymax=conf.high)) +
         geom_errorbar() +
         geom_point()

# inspect with glance
glance(fit)

#############################Heights
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton %>% head

galton %>% group_by(pair) %>% summarise(n())

#Calculate the correlation coefficients for fathers and daughters, fathers and sons, mothers and daughters and mothers and sons.
galton %>% group_by(pair) %>% 
           summarise(cor(childHeight, parentHeight))

#Use lm() and the broom package to fit regression lines for each parent-child pair type. 
#Compute the least squares estimates, standard errors, confidence intervals and p-values 
#for the parentHeight coefficient for each pair.
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = T))  %>%
  filter(term == "parentHeight" & p.value < .05)
