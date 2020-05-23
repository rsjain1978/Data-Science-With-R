library(dslabs)
library('gapminder')

head(gapminder)

#draw a plot with year and life expectancy
gapminder %>% filter(region=='Caribbean') %>% ggplot(aes(year, life_expectancy, color=country)) + geom_line()

#find countries with bigger names
gapminder %>% filter(region=='Caribbean') %>% filter(str_length(country)>10) %>% distinct(country)

#replace bigger country names with smaller names

gapminder %>% filter(region=='Caribbean') %>%
        mutate(country = recode(country, 
                                'Antigua and Barbuda'='Antigua',
                                'Dominican Republic'='Dominican',
                                'Puerto Rico'='P Rico',
                                'St. Vincent and the Grenadines'='Grenadines',
                                'Trinidad and Tobago'='T&T')) %>% 
        ggplot(aes(year, life_expectancy, color=country)) + geom_line()
