library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

# load ggthemes library for theme_economist 
library(ggthemes)

the_disease = "Smallpox"
dat <- us_contagious_diseases %>% 
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease ) %>% 
  mutate(rate = count / population * 10000) %>% 
  mutate(state = reorder(state, rate))

dat %>% ggplot(aes(year, state, fill = rate)) + 
  geom_tile(color = "grey50") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
  theme_economist() + 
  theme(panel.grid = element_blank()) + 
  ggtitle(the_disease) + 
  ylab("") + 
  xlab("")
