# correlation is a measure of how two variables are related
# correlation is not always the right measure
# there are scenarios in which it's a good measure - what are those?

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

#find mean of son's heights
mean (galton_heights$son)

#find dataset where father's height is around 72
tall_fathers <- galton_heights %>% filter(round(father)==72)
mean (tall_fathers$son) #mean for son's height is more now


######################linear regression slope and intercept
# compute a regression line to predict the son's height from the father's height
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x

father_height <- 66
predicted_son_height <- m_1*father_height + b_1
predicted_son_height

# compute a regression line to predict the father's height from the son's height
m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2*mu_y
son_height <- predicted_son_height
predicted_father_height <- m_2*son_height + b_2
predicted_father_height


#Assignment
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- 2
s_y <- 3
r <- 0.5
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x

predicted_son_height <- m_1*60 + b_1 #62.3
predicted_son_height <- m_1*61 + b_1 #63.1

