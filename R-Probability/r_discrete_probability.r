library('dslabs')

#create an experimental virtual bag of Red and Blue balls, 3 and 2 in number
beads <- rep (c('Red','Blue'), times = c(3,2))

#with replicate we create a manto carlo simulation of selecting one ball from this URN and run
#this simulation 500 times
results <- replicate( 50000, sample(beads,1))

#assign the results and analyse results
tab <- table (results)

#print he proportions of picking either of the two balls and they are inline with expected values
prop.table (tab)
