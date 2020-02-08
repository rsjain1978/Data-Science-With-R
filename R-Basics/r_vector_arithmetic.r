library('dslabs')
data('murders')

cat ('state with highest population ->',murders$state[which.max(murders$population)])

murder_rate <- murders$total/murders$population*100000
print (murder_rate)
idx <- order(murder_rate,decreasing=TRUE)
print (idx)
murders$state[idx]

