library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

names(temp_carbon)
max (temp_carbon$year)

temp_carbon %>% pull(year) %>% max()
temp_carbon %>% .$year %>% max()

temp_carbon %>% filter (!is.na(temp_anomaly)) %>% select(year) %>% max()

p <- temp_carbon %>% ggplot(aes(year, temp_anomaly)) + geom_line() 
p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")


temp_carbon %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line() +
  geom_hline(aes(yintercept = 0), col = "blue") +
  ylab("Temperature anomaly (degrees C)") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue") +
  xlim(c(1880, 2018)) +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018")
