library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)   # report 3 significant digits

str(stars)

mean(stars$magnitude)
sd(stars$magnitude)

#Make a density plot of the magnitude.
stars %>% ggplot(aes(magnitude)) + geom_density()

#Examine the distribution of star temperature.
stars %>% ggplot(aes(temp)) + geom_density()

#Make a scatter plot of the data with temperature on the x-axis and magnitude on the y-axis and examine the relationship between the variables. Recall that lower magnitude means a more luminous (brighter) star.
stars %>% ggplot(aes(magnitude, temp)) + geom_point()

library(ggrepel)
stars %>% ggplot(aes(temp, magnitude, label=star)) + geom_point() + scale_x_continuous(trans = "log10") + geom_text_repel()

stars %>% filter (temp>5000) %>% ggplot(aes(temp, magnitude, label=star)) + geom_point() + scale_x_continuous(trans = "log10") + geom_text_repel()


stars %>% filter (temp>5000) %>% ggplot(aes(temp, magnitude, color=type)) + geom_point() + scale_x_continuous(trans = "log10")
