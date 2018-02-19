## Running
source('population_function.R')
source('moore_function.R')

## storage for simulation output
OUTPUT.LIST <- list()


replicates <- 10

LANDSCAPE.RATIO <- c(0.1, 0.2, 0.5, 1, 2, 5, 10)
MVT.VEC <- c(0.2, 0.3, 0.5, 0.7, 0.9)
RISK.VEC <- c(0, 0.2, 0.4, 0.6, 0.8)


meta.table <- array(dim = c(length(LANDSCAPE.RATIO) * length(MVT.VEC) * length(RISK.VEC) * replicates, 5))
colnames(meta.table) <- c('id', 'landscape.ratio', 'movement', 'risk', 'replicate')

COUNT <- 0

for ( eye in 1:length(LANDSCAPE.RATIO) ){

      patch.dim <- 10
      x <- array(sample(0:2, patch.dim^2, prob = c(0,1,LANDSCAPE.RATIO[eye]), replace = TRUE), dim = c(patch.dim, patch.dim))
 
      for ( jay in 1:length(MVT.VEC) ){
        for ( kay in 1:length(RISK.VEC) ){
      
          for ( ell in 1:replicates ){
              
              COUNT <- COUNT + 1
      
              ## Habitat selection simulation
              sim.out <- pop.habitatselection(POP.SIZE = 100,
                                          LANDSCAPE = x,
                                          RISK.MAG = RISK.VEC[ kay ],
                                          PERCEPTION = 0.1,
                                          MVT = MVT.VEC[ jay ],
                                          MVT.MOD = 0.1)
              
              ## Moore neighborhoods
              moore.out <- moore.summary(LANDSCAPE = sim.out$A,
                                         EGG.LANDSCAPE = sim.out$egg.landscape,
                                         MOORE.RANGE = 3)
        
        
              
              ## table with parameters      
              meta.table[COUNT,] <- c(COUNT, LANDSCAPE.RATIO[eye], MVT.VEC[jay], RISK.VEC[kay], ell)
              
              ## collect simulation output into a list
              OUTPUT.LIST[[COUNT]] <- list(sim.out = sim.out, 
                                           moore.out = moore.out)

           
              print( COUNT / nrow(meta.table) )
          }
        }
      }
}


#save(meta.table, OUTPUT.LIST, file = 'testRun.RData')

