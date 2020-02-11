library('dslabs')
library(dplyr)
data("gapminder")
head(gapminder)


#comparing infant_mortality b/w two countrie
gapminder %>% filter (year== 2015 & country %in% c('Sri Lanka','Turkey')) %>% select (country, infant_mortality)

names (gapminder)

# compare fertility & life expectancy by region
gapminder %>% filter (year== 1962) %>% ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point()

# using faceting
gapminder %>% filter (year== 2012) %>% ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point()

gapminder %>% filter (year %in% c(1962,2012)) %>% ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point() + facet_grid(continent~year)

gapminder %>% filter (year %in% c(1962,2012)) %>% ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point() + facet_grid(.~year)

gapminder %>% 
  filter (year %in% c(1962,1972,1982,1992,2002,2012)) %>% 
  ggplot(aes(fertility, life_expectancy, color=continent)) + 
  geom_point() + 
  facet_wrap(~year)

#time series plot
gapminder %>% filter (country == 'United States') %>%
  ggplot(aes(year, fertility)) +
  geom_line()

# comparing fertility for two countries
gapminder %>% filter (country %in% c('United States','Japan')) %>%
  ggplot(aes(year, fertility, group=country, color=country)) +
  geom_line()

gapminder %>% filter (country %in% c('United States','Japan')) %>%
  ggplot(aes(year, life_expectancy, group=country, color=country)) +
  geom_line()

# adding dollar per day

gapminder <- gapminder %>% mutate (dollars_per_day = gdp/population/365)

past_year <- 1970
gapminder %>% filter (year == past_year & !is.na(gdp)) %>% ggplot (aes(dollars_per_day)) +geom_histogram(binwidth=1)
gapminder %>% filter (year == past_year & !is.na(gdp)) %>% ggplot (aes(log2(dollars_per_day))) +geom_histogram(binwidth=1)

#stratify and boxplot
p <- gapminder %>% filter (year == past_year & !is.na(gdp)) %>% ggplot (aes(region, dollars_per_day)) +geom_boxplot()
p <- p + theme(axis.text.x = element_text(angle=90, hjust=1))
p

#stratify and boxplot & then order
p <- gapminder %>% filter (year == past_year & !is.na(gdp)) %>% ggplot (aes(region, dollars_per_day, fill=continent)) +geom_boxplot()
p <- p + theme(axis.text.x = element_text(angle=90, hjust=1))
p <- p + geom_point()
p

#comparing distributions

west <- c('Western Europe',
          'Northern Europe',
          'Southern Europe',
          'Northern America',
          'Australia and New Zealand')


p <- gapminder %>% filter (year == past_year & !is.na(gdp)) %>% mutate (group = ifelse (region %in% west,'West','Rest')) %>% ggplot (aes(dollars_per_day))
p <- p + geom_histogram(binwidth = 1)
p <- p + scale_x_continuous(trans = "log2")
p <- p+ facet_grid(.~group)
p

past_year <- 1972
current_year <- 2010

p <- gapminder %>% filter (year %in% c(past_year, current_year) & !is.na(gdp)) %>% mutate (group = ifelse (region %in% west,'West','Rest')) %>% ggplot (aes(dollars_per_day))
p <- p + geom_histogram(binwidth = 1, color='black')
p <- p + scale_x_continuous(trans = "log2")
p <- p+ facet_grid(year~group)
p

# density plots
p <- gapminder %>% filter (year %in% c(past_year, current_year) & !is.na(gdp)) %>% mutate (group = ifelse (region %in% west,'West','Rest')) %>% ggplot (aes(dollars_per_day, y=..count.., fill=group))
p <- p + scale_x_continuous(trans = "log2")
p <- p + geom_density(alpha=0.2)
p <- p+ facet_grid(year~.)
p

p <- gapminder %>% filter (year %in% c(past_year) & !is.na(gdp)) %>% mutate (group = ifelse (region %in% west,'West','Rest')) %>% ggplot (aes(dollars_per_day, y=..count.., fill=group))
p <- p + scale_x_continuous(trans = "log2")
p <- p + geom_density(alpha=0.2)
p <- p+ facet_grid(year~.)
p

#catter plot of life expectancy versus fertility for the African continent in 2012.
gapminder %>% filter(continent =='Africa' & year == 2012) %>%
  ggplot(aes(life_expectancy ,fertility )) +
  geom_point()

gapminder %>% filter(continent =='Africa' & year == 2012) %>%
  ggplot(aes(fertility,life_expectancy , color=region)) +
  geom_point()

#table showing the country and region for the African countries (use select) that in 2012 had fertility rates of 3 or less and life expectancies of at least 70.
df <- gapminder %>% filter (year==2012 & continent=='Africa' & fertility <=3 & life_expectancy>=70) %>% select (country, region)

tab <- gapminder %>% filter (year %in% 1960:2010 & country %in% c('United States','Vietnam'))

#geom_line to plot life expectancy vs year for Vietnam and the United States and save the plot as p. The data table is stored in tab.
p <- tab %>% ggplot(aes(year, life_expectancy, color=country)) + geom_line()

#create a time series plot from 1960 to 2010 of life expectancy vs year for Cambodia.
gapminder %>% filter (year %in% 1960:2010 & country == 'Cambodia') %>% ggplot(aes(year, life_expectancy)) + geom_line()


daydollars <- gapminder %>% mutate(dollars_per_day = gdp/population/365) %>% filter(continent == "Africa" & year == 2010 & !is.na(dollars_per_day)) 

#calculate and plot dollars per day for African countries in 2010 using GDP data.
daydollars %>% filter (year == 2010 & continent =='Africa' & !is.na(gdp)) %>% ggplot (aes(dollars_per_day)) +geom_density(alpha=0.2) + scale_x_continuous(trans = "log2")

#combine the plotting tools we have used in the past two exercises to create density plots for multiple years.
daydollars <- gapminder %>% mutate(dollars_per_day = gdp/population/365) %>% filter (continent=='Africa' & year %in% c(1970,2010) & !is.na(gdp)) %>% ggplot (aes(dollars_per_day)) +geom_density(alpha=0.2) + scale_x_continuous(trans = "log2")+ facet_grid(year~.)

#stacked density plot of each region in Africa.
p <- gapminder %>% mutate(dollars_per_day = gdp/population/365) %>% filter (continent=='Africa' & year %in% c(1970,2010) & !is.na(gdp)) %>% ggplot (aes(dollars_per_day,fill=region)) +geom_density(bw=0.5, position='stack') + scale_x_continuous(trans = "log2")+ facet_grid(year~.)


gapminder_Africa_2010 %>% ggplot(aes(dollars_per_day, infant_mortality, color=region, label=country)) + geom_point() + scale_x_continuous(trans = "log2")+geom_text()

gapminder %>% mutate(dollars_per_day = gdp/population/365) %>% filter(continent == "Africa" & year %in% c(1970,2010) & !is.na(dollars_per_day)) %>% ggplot(aes(dollars_per_day, infant_mortality, color=region, label=country)) + geom_point() + scale_x_continuous(trans = "log2")+geom_text() + facet_grid(year~.)

#box plot to show regions ordered by murder rate
murders %>% mutate(rate = total/population*100000) %>% ggplot(aes(x=reorder(region,rate), rate, fill=region))+geom_boxplot()+geom_point ()