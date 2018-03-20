source('generateModuleLandscape_function.R')
source('plotLandscape_function.R')
source('shuffleLandscape.R')
source('population_function.R')
source('community_function.R')
source('plotEggs.R')




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


hatched <- array(dim = dim(egg.landscape))

p.emerge <- 1
emerge.penalty <- 0.9

for (i in 1:length(c(egg.landscape))){

  eggs <- egg.landscape[i]

    if (a$module.landscape[i] < 2){
      tmpEmerge <- p.emerge
    } else {
      tmpEmerge <- (p.emerge - emerge.penalty)
      
    }
  
  hatched[i] <- sum(runif(eggs) < tmpEmerge) 
  
}


hatched
sum(hatched)
  