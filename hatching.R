egg.landscape



hatched <- array(dim = dim(egg.landscape))
F <- 0.5
for (i in 1:length(c(egg.landscape))){

  eggs <- egg.landscape[i]

  hatched[i] <- sum(runif(eggs) < F) 
  
}


hatched
