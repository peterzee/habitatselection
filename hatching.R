source('generateModuleLandscape_function.R')
source('plotLandscape_function.R')
source('shuffleLandscape.R')
source('population_function.R')
source('community_function.R')
source('plotEggs.R')




a <- generateModuleLandscape(MATRIX.SIZE = 1, 
                             PATCH.DIM = 3, 
                             MODULE.DIM = 4,
                             STRUCTURE = TRUE,
                             SHUFFLE = TRUE)

# ## Simulation the population
sim <- pop.habitatselection(POP.SIZE = 250,
                            LANDSCAPE = a$module.landscape,
                            RISK.MAG = 0.8,
                            PERCEPTION = 0.1,
                            MVT = 0.5,
                            MVT.MOD = 0.2)

plotLandscape(a$module.landscape)

egg.landscape <- sim$egg.landscape

hatched <- array(dim = dim(egg.landscape))
F <- 0.5

for (i in 1:length(c(egg.landscape))){

  eggs <- egg.landscape[i]

  hatched[i] <- sum(runif(eggs) < F) 
  
}


hatched
  