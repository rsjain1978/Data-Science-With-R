library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

head (Teams)
names (Teams)

#Code: Scatterplot of the relationship between HRs and runs (- +ve corelation)
Teams %>% filter(yearID %in% 1961:2001) %>%
          mutate(hr_per_game = HR/G, runs_per_game = R/G) %>% 
          ggplot(aes(hr_per_game, runs_per_game, color='teamID')) + 
          geom_point(alpha=0.5)

#Code: Scatterplot of the relationship between stolen bases and wins (no corelation)
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(hr_per_game = SB/G, wins_per_game = R/G) %>% 
  ggplot(aes(hr_per_game, wins_per_game, color='teamID')) + 
  geom_point(alpha=0.5)

?Teams

#Filter the Teams data frame to include years from 1961 to 2001. Make a scatterplot of runs per game versus at bats (AB) per game.
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(bats_per_game = AB/G, runs_per_game = R/G) %>% 
  ggplot(aes(bats_per_game, runs_per_game, color='teamID')) + 
  geom_point(alpha=0.5)

#Make a scatterplot of win rate (number of wins per game) versus number of fielding errors (E) per game.
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(fielding_errors_per_game = E/G, wins_per_game = W/G) %>% 
  ggplot(aes(fielding_errors_per_game, wins_per_game, color='teamID')) + 
  geom_point(alpha=0.5)

#Make a scatterplot of triples (X3B) per game versus doubles (X2B) per game.
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(triples_per_game = X3B/G, doubles_per_game = X2B/G) %>% 
  ggplot(aes(triples_per_game, doubles_per_game, color='teamID')) + 
  geom_point(alpha=0.5)

##############################CORRELATION########################
##Relationship between more than 1 variable

