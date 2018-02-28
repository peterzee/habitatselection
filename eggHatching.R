egg.hatched <- function(EGG.LANDSCAPE, P.EMERGE, PENALTY){

    egg.landscape <- EGG.LANDSCAPE
    
    hatched <- array(dim = dim(egg.landscape))
    
    p.emerge <- P.EMERGE
    emerge.penalty <- PENALTY
    
    for (i in 1:length(c(egg.landscape))){
      
      eggs <- egg.landscape[i]
      
      if (a$module.landscape[i] < 2){
        tmpEmerge <- p.emerge
      } else {
        tmpEmerge <- (p.emerge - emerge.penalty)
      }
      
      hatched[i] <- sum(runif(eggs) < tmpEmerge) 
  
    }

  return(hatched)
}

# x <- egg.hatched(sim$egg.landscape, P.EMERGE = 1, PENALTY = 0.9)
# sum(x)
