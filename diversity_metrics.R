###  diversity_metrics


aDiv <- array(NA, dim = c(nrow(A), ncol(A)))
for (i in 1:length(index)){
  
  id <- which(index == i, arr.ind =TRUE)  
  loc.comm <- egg.landscape[id[1], id[2],]
  aDiv[i] <- sum(loc.comm > 0)
  
  }


mean.alpha <- tapply(c(aDiv), c(A), mean)
gamma <- sum(apply(egg.landscape, 3, function(x) sum(x) > 0))

beta <- gamma / mean.alpha



