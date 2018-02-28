source('generateModuleLandscape_function.R')
source('plotLandscape_function.R')
source('shuffleLandscape.R')
source('population_function.R')
source('community_function.R')
source('plotEggs.R')

## Generate a landscape
a <- generateModuleLandscape(MATRIX.SIZE = 4, 
                             PATCH.DIM = 4, 
                             MODULE.DIM = 6,
                             STRUCTURE = TRUE,
                             SHUFFLE = FALSE)

# ## Simulation the population
sim <- pop.habitatselection(POP.SIZE = 250,
                            LANDSCAPE = a$module.landscape,
                            RISK.MAG = 0.8,
                            PERCEPTION = 0.1,
                            MVT = 1,
                            MVT.MOD = -7)

egg.landscape <- sim$egg.landscape


plotLandscape(a$module.landscape)
plotEggs(EGG.LANDSCAPE = sim$egg.landscape, LANDSCAPE = a$module.landscape)

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

x <- egg.hatched(sim$egg.landscape, P.EMERGE = 1, PENALTY = 0.5)
sum(x)
