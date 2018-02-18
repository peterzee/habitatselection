x <- array(0, dim = c(10,10))
x[seq(2, 50, by=2)] <- 1
x[seq(52, 100, by=2)] <- 2
x




boxplot(c(distance.tracker), na.rm=TRUE)





distance.tracker[,211]


locations[,,211]

afield <- array(dim=c(time, pop.size))
for (j in 1:pop.size){
  for (i in 2:nrow(locations[-which(is.na(locations[,1,j])),,j])){
    afield[i,j] <- distance(rbind(locations[1,,j], locations[i,,j]))
  }
}


