library ('tidyverse')
library ('dslabs')
data ('murders')

# Pipe data to ggplot
# define layer 1 describing type of graph (_point)
# add layer 2 describing aesthetics
# pass x and y
p <- murders %>% ggplot() + 
  geom_point(aes(x=population/10^6, y=total),size=3) +
  geom_text(aes(x=population/10^6, y=total, label=abb),nudge_x = 2) 
p

rate <- murders %>% summarize(rate = sum(total)/sum(population)*10^6) %>% .$rate
print (rate)

install.packages('ggrepel')
library ('ggrepel')
p <- murders %>% ggplot(aes(population/10^6, total, label=abb, color=region))
p <- p + geom_abline(intercept= log10(rate), lty=2)
p <- p + geom_point(size=1)
p <- p + geom_text_repel(nudge_x = .05)
p <- p + xlab('Population (in mn)')
p <- p + ylab('Murders (total)')
p <- p + ggtitle('US States Murders')
p <- p + scale_x_log10()
p <- p + scale_y_log10()
p <- p + scale_color_discrete(name='Region')

library (ggthemes)
p <- p + theme_economist()
p

# Histogram
data (heights)

p <- heights %>% filter (sex =='Male') %>% ggplot (aes(x=height))
p <- p + geom_histogram(binwidth = 1,fill='blue', col='black')
p <- p + xlab('Height (in inches)')
p <- p + ylab ('Number of male students')
p

?geom_density
# Density plots
p <- heights %>% filter (sex =='Male') %>% ggplot (aes(x=height))
p <- p + geom_density(fill='grey', col='black')
p <- p + xlab('Height (in inches)')
p <- p + ylab ('Number of male students')
p

# using alpha and fill parameters
heights %>% ggplot(aes(height, fill = sex)) + geom_density(alpha=0.2) 

# qq plots
q <- heights %>% filter (sex =='Male') %>% ggplot (aes(sample=height))
q <- q + geom_qq(fill='grey', col='black')
q <- q + xlab('Height (in inches)')
q <- q + ylab ('Number of male students')
q


install.packages('gridExtra')
library (gridExtra)

grid.arrange(p,q, ncol=2)

p <- heights %>% filter (sex =='Male') %>% ggplot (aes(x=height))
p1 <- p + geom_histogram(binwidth = 1,fill='blue', col='black')
p2 <- p + geom_histogram(binwidth = 1,fill='blue', col='black')

grid.arrange(p1,p2, ncol=2)
