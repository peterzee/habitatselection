
## summary of how far individuals move
afield <- array(dim=c(time, pop.size))
afield[1,] <- rep(0, pop.size)

step.distances <- array(dim=c(time, pop.size))

for (j in 1:pop.size){
  for (i in 2:nrow(locations[-which(is.na(locations[,1,j])),,j])){
    afield[i,j] <- distance(rbind(locations[1,,j], locations[i,,j]), method = 'chebyshev')
    step.distances[i,j] <- distance(rbind(locations[i-1,,j], locations[i,,j]), method = 'chebyshev')
    }
}


### searching time histogram
hist(colSums(!is.na(afield)*1))

## plot of how far afield each indiivdual goes from starting position
plot(1:time, afield[,5], type = 'l', xlim = c(0,max(colSums(!is.na(afield)*1))), ylim=c(0,max(afield, na.rm = TRUE)))
for (i in 2:pop.size){
  points(1:time, afield[,i], type = 'l')
}

max(afield, na.rm = TRUE)
max(step.distances, na.rm=TRUE)


range <- 1
moore.size <- ((2*range)+1)^2
