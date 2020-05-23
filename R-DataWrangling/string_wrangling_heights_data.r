library(dslabs)
library(tidyverse)
library (rvest)

data("reported_heights")

#print structure of dataframe
str(reported_heights)

#class of heigts column
class (reported_heights$height) #it's character

#checking number of NA
sum (is.na(reported_heights$height))

#do coercion and check for NAs
test <- reported_heights %>% mutate (new_height = as.numeric(height))
sum (is.na(test)) #NAs got introduced after the coercion

#do coercion and filter out records which have NAs
test1 <- reported_heights %>% mutate(new_height = as.numeric(height)) %>% filter(is.na(new_height))
head (test1)

#we see that some heights are in feet and inches, some are in cms. to fix them we first extract

not_inches <- function (x, smallest=50, highest=84) 
{
  inches <- suppressWarnings(as.numeric(x))
  incorrect_heights <- is.na(inches) | inches<smallest | inches>highest
  incorrect_heights
}

wrong_heights <- reported_heights %>% filter(not_inches(height))
dim (wrong_heights)

wrong_heights

###############RegEx#########################

heights_cms <- str_subset(reported_heights$height, 'cm')
length(heights_cms)

sum (str_detect(reported_heights$height, 'cm')) #two entries have trailing cm
sum (str_detect(reported_heights$height, 'inches')) #six entries have trailing inches

sum (str_detect(reported_heights$height, 'cm|inches')) # 8 entries have cm or inches

sum (str_detect(reported_heights$height, '\\d')) #1093 entries have digits in them

install.packages("htmlwidgets") 
str_view(reported_heights$height, '\\d')
str_view_all(reported_heights$height, '\\d')

str_detect(reported_heights$height, 'cm|inches')
height_in_cm_inches <- str_view(reported_heights$height, 'cm|inches')


##############Character level RegEx################
str_view(reported_heights$height, "[69]")   #Those heights which have either 6 or 9
str_subset(reported_heights$height, "[5]")  #Those heights which have either 5

str_subset(reported_heights$height, "[5-9]")  #Those heights having number between 5-9

str_subset(reported_heights$height, "[a-z]")  #Those heights having lower case alphabets
str_subset(reported_heights$height, "[A-Z]")  #Those heights having upper case alphabets

##############Character RegEx using anchors (^-for start position and $-for end position)
str_subset(reported_heights$height, "^[7]$")  #Those heights which have value of just 7
str_subset(reported_heights$height, "^[165]$")  #Those heights which have value of 7 followed by some character

str_subset(reported_heights$height, "^[5-9]'[0-9]{1,2}$")  #Those heights which have inches and feet
str_subset(reported_heights$height, "^[5-9] feet [0-9]{1,2}inches$")  #Those heights which have inches and feet along with heights
str_subset(reported_heights$height, "^[5-9].[0-9]{1,2}$")  #Those heights which have heights in feet.inches


##############Search & Replace with RegEx##################
str_subset(reported_heights$height,"\\d f[a-z]{1,5} \\d")
reported_heights$height <- reported_heights$height %>% str_replace("feet","'") # replace feet by '
reported_heights$height <- reported_heights$height %>% str_replace("ft","'")  # replace ft by '
reported_heights$height <- reported_heights$height %>% str_replace("foot","'") # replace foot by '
str_subset(reported_heights$height,"\\d\'\\d")

identical("Hi","Hi ") #checking equality since space symbol also matters
str_subset(reported_heights$height,"[4-7]'{1}\\s") #matches only those heights which have just one space
str_subset(reported_heights$height,"[4-7]'{1}\\s*")#matches those heights which have one ore more spaces


#################Groups with RegEx##########################
# define regex with and without groups
pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <-  "^([4-7]),(\\d*)$"

# create examples
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)

# demonstrate the effect of groups
str_subset(s, pattern_without_groups)
str_subset(s, pattern_with_groups)

# improve the pattern to recognize more events
pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2")
s

# final pattern
pattern_with_groups <-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"
str_subset(s, pattern_with_groups)

#####################Exercise###################
not_inches <- function(x, smallest = 50, tallest = 84) {
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest 
  ind
}
not_inches(56) #TRUE value means it's not an inch, FALSE value means it's an inch

s <- c("70","5 ft","4'11","",".","Six feet")
str_view(s, "\\d|ft")

animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]"
str_detect(animals, pattern)

animals <- c("moose", "monkey", "meerkat", "mountain lion")
str_detect(animals, "[a-z]*") #should give T T T T
str_detect(animals, "m*") #should give T T T T
pattern <- "moo*"
str_detect(animals, "(mo*)")

schools <- c("U. Kentucky",
             "Univ New Hampshire",
             "Univ. of Massachusetts",
             "University Georgia",
             "U California",
             "California State University")

s_hash <- str_replace(schools, "^Univ\\.?\\s|^U.\\s","University ")
s_hash <- str_replace(s_hash, "^University\\s|^U\\s","University of ")
s_hash

s_hash <- schools %>% 
  str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ") %>% 
  str_replace("^University of |^University ", "University of ")

s_hash


problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")


converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
converted[!index]


yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)

converted <- s %>% 
  str_replace("\\sfeet|foot|ft", "'") %>% 
  str_replace("\\s*inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_subset(converted, pattern)

#######################Seperate with RegEx########################

s <- c("5'11","6'3")#vector
tab <- data.frame(x=s)#converted to dataframe
tab %>% extract(x,c('feet','inches'), regex = "(\\d)'(\\d*)") #using extract to take values out

s <- c("5'11\"","6'3") #more complex vector
tab <- data.frame(x=s)
tab %>% extract(x,c('feet','inches'), regex = "(\\d)'(\\d*)")

s <- c("5'11\"","6'3","5'2inches") #more complex vector
tab <- data.frame(x=s)
tab %>% extract(x,c('feet','inches'), regex = "(\\d{1})'(\\d*)")

s <- c("5'11\"","6'3","5feet 2inches") #more complex vector
tab <- data.frame(x=s)
tab %>% str_replace("feet","'") %>% extract(x,c('feet','inches'), regex = "(\\d?)(\\d*)")

s <- c("5'10", "6'1\"", "5'8inches", "5'7.5")
tab <- data.frame(x = s)
tab %>% extract(x, c('feet','inches','decimal'), regex = "(\\d)'(\\d{1,2})(\\.\\d)?")
