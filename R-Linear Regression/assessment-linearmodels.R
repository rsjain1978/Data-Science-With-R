library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G) %>%
  mutate(RPG = R/G) %>%
  mutate(HRPG = HR/G)

names (Teams_small)
Teams_small %>% head

#Use runs (R) per game to predict average attendance.
tidy(Teams_small %>% lm(avg_attendance~RPG, data=.))

#Use home runs (HR) per game to predict average attendance
tidy(Teams_small %>% lm(avg_attendance~HRPG, data=.))

#Use number of wins to predict average attendance; do not normalize for number of games.
Teams_small %>% group_by(yearID) %>% lm(avg_attendance~W, data=.) %>% coef

Teams_small_1 <- Teams_small %>% group_by(yearID) %>%
                 mutate (year_attendance=sum(attendance)/length(G))
                #select(year_attendance, attendance, G, yearID)

Teams_small %>% group_by(yearID) %>%
                lm (avg_attendance~yearID, data=.) %>%
                coef

#Are wins and runs per game or wins and home runs per game correlated?
Teams_small_1 <- Teams_small %>% mutate(RPG=R/G, HRPG=HR/G)

cor(Teams_small_1$W, Teams_small_1$RPG)
cor(Teams_small_1$W, Teams_small_1$HRPG)

#Stratify Teams_small by wins: divide number of wins by 10 and then round to the nearest 
#integer. Keep only strata 5 through 10, which have 20 or more data points.

Teams_small_Wins_strata <- Teams_small %>% mutate(W_strata = round(W/10)) %>%
                                           filter(W_strata %in% seq(5,10,1) & n()>=20)

#How many observations are in the 8 win strata?
Teams_small_Wins_strata %>% filter(W_strata == 8) %>% count()

Teams_small_Wins_strata %>% group_by(W_strata) %>% select(W_strata, RPG)

#Calculate the slope of the regression line predicting average attendance given runs per 
#game for each of the win strata.
models <- Teams_small_Wins_strata %>% group_by(W_strata) %>%
                                      do(tidy(lm(avg_attendance~RPG, data = .))) %>%
                                      ungroup() %>%
                                      filter(term=='RPG')

#Calculate the slope of the regression line predicting average attendance given HR per 
#game for each of the win strata.
Teams_small_Wins_strata %>% group_by(W_strata) %>%
                            do(tidy(lm(avg_attendance~HRPG, data=.))) %>%
                            ungroup() %>%
                            filter(term=='HRPG')

#Fit a multivariate regression determining the effects of runs per game, home runs per game, 
#wins, and year on average attendance.
res4 <- Teams_small %>%
  mutate(R = R/G, HR = HR/G) %>%
  do(tidy(lm(avg_attendance ~ R + HR + W + yearID, data=.)))
res4

#Use the multivariate regression model from Question 4. Suppose a team averaged 5 runs per 
#game, 1.2 home runs per game, and won 80 games in a season.

sum(res4$estimate * c(1,5,1.2,80,2002))
sum(res4$estimate * c(1,5,1.2,80,1960))

#Use your model from Question 4 to predict average attendance for teams in 2002 in the 
#original Teams data frame.
#What is the correlation between the predicted attendance and actual attendance?

fit <- Teams_small %>%
              mutate(R = R/G, HR = HR/G) %>%
              lm(avg_attendance ~ R + HR + W + yearID, data=.)


Teams_2002 <- Teams %>% filter(yearID == 2002) %>% 
                        mutate (R = R/G) %>%
                        mutate (HR = HR/G) %>%
                        mutate (avg_attendance = attendance /G)

predicted_attendance = predict(fit, Teams_2002)

cor(predicted_attendance, Teams_2002$avg_attendance)

Teams_2002 <- Teams_2002 %>% mutate(pred_attendance = predicted_attendance)

plot(predicted_attendance,Teams_2002$avg_attendance,xlab="predicted",ylab="actual")
abline(a=0,b=1)

library ('ggrepel')
Teams_2002 %>% 
                ggplot(aes(avg_attendance, pred_attendance, label=teamID, color=teamID))+
                geom_point(size=1) +
                geom_text_repel(nudge_x = .05) +
                geom_abline(intercept = 0, slope = 1) +
                xlab('Actual Avg Attendance')
