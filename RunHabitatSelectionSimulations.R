## Running
source('generateModuleLandscape_function.R')
source('population_function.R')
source('moore_function.R')
source('module_neighborhood_function.R')


## storage for simulation output
OUTPUT.LIST <- list()


replicates <- 10

LANDSCAPE.STRUCTURE <- c(FALSE, TRUE)
MVT.VEC <- c(0.2, 0.5, 0.9)
RISK.VEC <- c(0,  0.4, 0.8)


meta.table <- array(dim = c(length(LANDSCAPE.STRUCTURE) * length(MVT.VEC) * length(RISK.VEC) * replicates, 5))
colnames(meta.table) <- c('id', 'landscape.structure', 'movement', 'risk', 'replicate')

COUNT <- 0

for ( eye in 1:length(LANDSCAPE.STRUCTURE) ){

      # patch.dim <- 10
      # x <- array(sample(0:2, patch.dim^2, prob = c(0,1,LANDSCAPE.RATIO[eye]), replace = TRUE), dim = c(patch.dim, patch.dim))

      a <- generateModuleLandscape(MATRIX.SIZE = 2, 
                                   PATCH.DIM = 3, 
                                   MODULE.DIM = 6, 
                                   STRUCTURE = LANDSCAPE.STRUCTURE[ eye ])
      
      index <- array(1:(nrow(a$module.landscape)^2), dim = c(nrow(a$module.landscape), 
                                                             nrow(a$module.landscape)))
      
      for ( jay in 1:length(MVT.VEC) ){
        for ( kay in 1:length(RISK.VEC) ){
      
          for ( ell in 1:replicates ){
              
              COUNT <- COUNT + 1
      
              ## Habitat selection simulation
              sim.out <- pop.habitatselection(POP.SIZE = 250,
                                          LANDSCAPE = a$module.landscape,
                                          RISK.MAG = RISK.VEC[ kay ],
                                          PERCEPTION = 0.1,
                                          MVT = MVT.VEC[ jay ],
                                          MVT.MOD = 0.19)
              
              ## Moore neighborhoods
              moore.out <- moore.summary(LANDSCAPE = a$module.landscape,
                                         EGG.LANDSCAPE = sim.out$egg.landscape,
                                         MOORE.RANGE = 3)
              
              ## Module moore neighborhoods
              module.out <- module.neighborhood(LANDSCAPE = a$module.landscape, 
                                                EGG.LANDSCAPE = sim.out$egg.landscape, 
                                                MODULE.DIM = a$inputs$MODULE.DIM, 
                                                MOD.EXTRACT = a$module.extract, 
                                                MOD.INDEX = a$mod.index)
              
        
        
              
              ## table with parameters      
              meta.table[COUNT,] <- c(COUNT, 
                                      LANDSCAPE.STRUCTURE[ eye ], 
                                      MVT.VEC[ jay ], 
                                      RISK.VEC[ kay ], 
                                      ell)
              
              ## collect simulation output into a list
              OUTPUT.LIST[[COUNT]] <- list(sim.out = sim.out, 
                                           moore.out = moore.out,
                                           module.out = module.out)

           
              print( COUNT / nrow(meta.table) )
          }
        }
      }
}


#save(meta.table, OUTPUT.LIST, file = 'testRun.RData')

