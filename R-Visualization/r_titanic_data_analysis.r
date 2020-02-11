options(digits = 3)    # report 3 significant digits
library(tidyverse)
#install.packages('titanic')
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

?titanic_train

#density plots
titanic %>% ggplot(aes(Age, group=Sex)) + geom_density(bw=0.5, position='stack')
titanic %>% ggplot(aes(Age, y=..count..,color=Sex)) + geom_density(bw=0.5, position='stack')
titanic %>% ggplot(aes(Age, y=..count..,color=Sex, )) + geom_density(alpha=0.2, position='stack')

#bar plot to check for survival ratios
titanic %>% ggplot(aes(Sex, fill=Survived)) + geom_bar()
titanic %>% ggplot(aes(Sex, fill=Survived)) + geom_bar(position = position_dodge())

#density plot of age filled by survival status. Change the y-axis to count and set alpha = 0.2.
titanic %>% ggplot(aes(Age, y=..count..,fill=Survived)) + geom_density(alpha=0.2)


#density plot of age filled by survival status. Change the y-axis to count and set alpha = 0.2.
titanic %>% filter(Fare>0) %>% ggplot(aes(Fare, y=..count..,fill=Survived)) + geom_density(alpha=0.2)+scale_x_continuous(trans = "log2")

p1 <- titanic %>% ggplot(aes(Pclass, fill=Survived)) + geom_bar()
p2 <- titanic %>% ggplot(aes(Pclass, fill=Survived)) + geom_bar(position = position_fill())
p3 <- titanic %>% ggplot(aes(Survived, fill=Pclass)) + geom_bar()
library (gridExtra)
grid.arrange(p1,p2,p3, ncol=3)

#Create a grid of density plots for age, filled by survival status, with count on the y-axis, faceted by sex and passenger class.
p1 <- titanic %>% ggplot(aes(Age, y=..count.., fill=Survived)) + geom_density() + facet_grid(Sex~Pclass)
p1
