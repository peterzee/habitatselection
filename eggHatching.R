egg.hatched <- function(EGG.LANDSCAPE, FECUNDITY, P.EMERGE, PENALTY, CARRY.CAP = 10){
  
    egg.landscape <- EGG.LANDSCAPE
    fec <- FECUNDITY
    p.emerge <- P.EMERGE
    emerge.penalty <- PENALTY
    K <- CARRY.CAP
    
    
    ## 
    total.eggs <- sum(egg.landscape)
    
    hatched <- array(dim = dim(egg.landscape))
    for (i in 1:length(c(egg.landscape))){
      
      eggs <- egg.landscape[i] 
      
          if (a$module.landscape[i] < 2){
            
            tmpEmerge <- p.emerge * ((K - total.eggs) / K)
            
          } else {
            
            tmpEmerge <- (p.emerge - emerge.penalty) * ((K - total.eggs) / K)
          
            }
      hatched[i] <- sum(runif(eggs) < tmpEmerge) * fec
  
    }
    # print(tmpEmerge)
    
  return(hatched)
}

# x <- egg.hatched(sim$egg.landscape, P.EMERGE = 1, PENALTY = 0.9)
# sum(x)


