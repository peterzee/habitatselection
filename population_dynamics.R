### Population size through time

source('generateModuleLandscape_function.R')
source('plotLandscape_function.R')
source('shuffleLandscape.R')
source('population_function.R')
source('community_function.R')
source('moore_function.R')
source('module_neighborhood_function.R')
source('plotEggs.R')
source('eggHatching.R')

## Generate the landscape
a <- generateModuleLandscape(MATRIX.SIZE = 1, 
                             PATCH.DIM = 4, 
                             MODULE.DIM = 6, 
                             STRUCTURE = FALSE,
                             SHUFFLE = FALSE)

landscape.index <-array(1:length(a$module.landscape), dim = dim(a$module.landscape))

generations <- 50
initial.popsize <- 1
popsize.time <- rep(NA, generations)
popsize.time[1] <- initial.popsize

for (i in 2:generations){

    tmpPopsize <- popsize.time[i - 1]

      ## Simulation the population
  if (i == 2) {
    sim <- pop.habitatselection(POP.SIZE = tmpPopsize,
                              LANDSCAPE = a$module.landscape,
                              RISK.MAG = 0.8,
                              PERCEPTION = 0.1,
                              MVT = 1,
                              MVT.MOD = -4)
  } else {
    sim <- pop.habitatselection(POP.SIZE = tmpPopsize,
                              LANDSCAPE = a$module.landscape,
                              RISK.MAG = 0.8,
                              PERCEPTION = 0.1,
                              MVT = 1,
                              MVT.MOD = -4,
                              RANDOM.START = FALSE)
  }
  
  hatched.landscape <- egg.hatched(sim$egg.landscape, FECUNDITY = 2, P.EMERGE = 1, PENALTY = 0.1, CARRY.CAP = 10)
  
  popsize.time[i] <-   sum(hatched.landscape)
  
  
  ##egg.starts (get 'locations' for start of next generation)
  egg.starts <- cbind(rep(which(hatched.landscape > 0, arr.ind = TRUE)[,1], hatched.landscape[which(hatched.landscape > 0)]),
                      rep(which(hatched.landscape > 0, arr.ind = TRUE)[,2], hatched.landscape[which(hatched.landscape > 0)]))

  
  ####
  print(i)
  if( popsize.time[i] ==0 ){
    print('extinction!')
    break
    }
}


plot(1:generations, popsize.time, type = 'b', pch = 1,
     ylim = c(0, max(popsize.time, na.rm = TRUE)),
     col = 1)

