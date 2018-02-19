###  diversity_metrics



A
egg.landscape[1,2,]


which(index == 2, arr.ind =TRUE)

aDiv <- array(NA, dim = c(nrow(A), ncol(A)))
for (i in 1:length(index)){
  
  id <- which(index == i, arr.ind =TRUE)  
  loc.comm <- egg.landscape[id[1], id[2],]
  aDiv[i] <- sum(loc.comm > 0)
  
  
  }


tapply(c(aDiv), c(A), mean)


