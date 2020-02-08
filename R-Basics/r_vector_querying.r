library('dslabs')
data('murders')
str(murders)


murder_rate <- (murders$total/murders$population)*100000
print (murder_rate)

safe_states <- murder_rate <0.71
cat ('number of safe states is ', sum(safe_states))
cat ('names of safe states is ',murders$state[safe_states])

west_states <- murders$region == 'West'
cat ('states in west are ', murders$state[west_states])

safe_west_states <- murders$state [safe_states & west_states]
print (safe_west_states)

#Getting index of a vector entry
index <- which (murders$state =='Vermont')
cat ('crime rate for vermont is ', murder_rate[index])

#Getting index of multiple entries
states = c('Vermont','Florida','New York')
index <- match(states, murders$state)
cat ('crime rate for these states is ', murder_rate[index])
print (murders$state[index])

#print random entries
print (murders$state[12])

# Check if entres belong to vector
states = c('Vermont','Florida','New York')
presence <- states %in% murders$state
print (presence)
