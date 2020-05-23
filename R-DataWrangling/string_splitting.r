# read raw murders data line by line
filename <- system.file("extdata/murders.csv", package = "dslabs")

#read lines
lines <- readLines(filename)
lines[[1]]
lines %>% head()

x<- str_split(lines, pattern = ",")
x %>% head()
x[[1]]
col_names <- x[[1]]
col_names
x <- x[-1]
x %>% head()

# extract first element of each list entry
library(purrr)
map(x,1) %>% head()

dat <- data.frame(parse_guess(map_chr(x,1)),
                  parse_guess(map_chr(x,2)),
                  parse_guess(map_chr(x,3)),
                  parse_guess(map_chr(x,4)),
                  parse_guess(map_chr(x,5))) %>% setNames(col_names)

head(dat)
class(dat$population)
