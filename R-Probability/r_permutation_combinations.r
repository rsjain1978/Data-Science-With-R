library('dslabs')

library('dplyr')
#install.packages('gtools')
library('gtools')
choices <- permutations (7,2) #Permutation(A,B) = !A/!A-B
class (choices)
nrow (choices)

#Finding probability of two people in a room of 50 people to have birth day on same day !!
n_times_monte_carlo_simulation = 10000
n_people = 50

# define which function which generates the probabilities
compute_probability <- function (n) {
  results <- replicate(n_times_monte_carlo_simulation, {
    bdays <- sample (1:365, n, replace=TRUE)
    any(duplicated(bdays))
    })
  mean (results)
}

#list of number of people we want to test against
num_of_people <- 1:50

#sapply applies a function to this list & generated probabilities
probs <- sapply (num_of_people, compute_probability)

#put data in a dataframe and generate graph.
df <- data.frame (NumPeople=num_of_people, Probability=probs)
df %>% ggplot(aes(NumPeople, Probability)) + geom_point()

##################Finding Count of Monte Carlo Simulations
n_times_monte_carlo_simulation = 10^seq(1,5, len=100)
print (n_times_monte_carlo_simulation)
n_people = 50

# define which function which generates the probabilities
compute_probability <- function (n) {
  results <- replicate(n, {
    bdays <- sample (1:365, n_people, replace=TRUE)
    any(duplicated(bdays))
  })
  mean (results)
}

#sapply applies a function to this list & generated probabilities
probs <- sapply (n_times_monte_carlo_simulation, compute_probability)
df <- data.frame (NumSimulationRuns=n_times_monte_carlo_simulation, Probability=probs)
p <- df %>% ggplot(aes(NumSimulationRuns, Probability)) + geom_point()


# Permutations
library (gtools)
results <- permutations(8,3)
nrow (results)

#Run a Monte Carlo simulation on this vector representing the countries of the 8 runners in this race:
B <- 10000
set.seed(1)
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")

results <- replicate(B, {
    sample <- sample(runners, 3)
    success <- sum(sample == 'Jamaica')==3
  })

mean (results)

library(tidyverse)
head(esoph)
str (esoph)



tmp<- esoph %>% filter (alcgp=='120+') %>% summarise (ncases,ncontrols)
sum(tmp$ncases)/(sum(tmp$ncontrols)+sum(tmp$ncases))

tmp<- esoph %>% filter (alcgp=='0-39g/day') %>% select (ncases,ncontrols)
sum(tmp$ncases)/(sum(tmp$ncontrols)+sum(tmp$ncases))

top_alc_smk = esoph %>% filter (alcgp=='120+' | tobgp=='30+') %>% select (ncases) %>% sum()
top_alc_smk/sum(esoph$ncases)

top_alc_cases = esoph %>% filter (alcgp=='120+') %>% select (ncases) %>% sum()
top_alc_controls = esoph %>% filter (alcgp=='120+') %>% select (ncontrols) %>% sum()

top_alc_controls/top_alc_cases
