data(heights)
heights

colnames(heights)
data.frame(colnames(heights))
rownames(heights)

class (heights)
class (heights$sex)
class (heights$height)
class ('Male')
class (75.00)

nrow(heights)
heights[777,]
heights$sex[777]

#Which row has the minimum height?
which.min(heights$height)

mean(heights$height)
median(heights$height)

count_male <- heights %>% filter(sex=='Male') %>% count()
percentage_male <- count_male / nrow(heights)
percentage_male


heights %>% filter(height>78) %>% count()

heights %>% filter(height>78 & sex=='Female') %>% count()
