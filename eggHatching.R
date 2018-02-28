egg.hatched <- function(EGG.LANDSCAPE, FECUNDITY, P.EMERGE, PENALTY, K){
  
    egg.landscape <- EGG.LANDSCAPE
    fec <- FECUNDITY
    p.emerge <- P.EMERGE
    emerge.penalty <- PENALTY

    hatched <- array(dim = dim(egg.landscape))
    for (i in 1:length(c(egg.landscape))){
      
      eggs <- egg.landscape[i] 
      
      if (a$module.landscape[i] < 2){
        tmpEmerge <- p.emerge
      } else {
        tmpEmerge <- (p.emerge - emerge.penalty)
      }
      
      hatched[i] <- sum(runif(eggs) < tmpEmerge) * fec
  
    }

  return(hatched)
}

# x <- egg.hatched(sim$egg.landscape, P.EMERGE = 1, PENALTY = 0.9)
# sum(x)


