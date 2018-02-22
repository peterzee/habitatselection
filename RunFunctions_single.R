### run functions
source('generateModuleLandscape_function.R')
source('population_function.R')
source('moore_function.R')
source('module_neighborhood_function.R')
source('plotLandscape_function.R')
source('shuffleLandscape.R')

## Generate the landscape
a <- generateModuleLandscape(MATRIX.SIZE = 2, 
                             PATCH.DIM = 4, 
                             MODULE.DIM = 6, 
                             STRUCTURE = TRUE,
                             SHUFFLE = TRUE)

plotLandscape(a$module.landscape)

## Simulation the population
sim <- pop.habitatselection(POP.SIZE = 100,
                            LANDSCAPE = a$module.landscape,
                            RISK.MAG = 0.9,
                            PERCEPTION = 0.1,
                            MVT = 0.5,
                            MVT.MOD = 0.1)

## Moore neighborhoods
moore.out <- moore.summary(LANDSCAPE = a$module.landscape,
                           EGG.LANDSCAPE = sim$egg.landscape,
                           MOORE.RANGE = 3)

## Module neighborhoods
module.out <- module.neighborhood(LANDSCAPE = sim$A,
                                  EGG.LANDSCAPE = sim$egg.landscape, 
                                  MODULE.DIM = a$inputs$MODULE.DIM,
                                  MOD.EXTRACT = a$module.extract, 
                                  MOD.INDEX = a$mod.index)



