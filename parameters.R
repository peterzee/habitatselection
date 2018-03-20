### Parameters for running simulations

## Lanscape parameters
params_landscape <- list(LANDSCAPE.SIZE = 64,
                         ENV = 2,
                         FRAG = 4, 
                         PROP.MATRIX = 0, 
                         RISK.QUANTILE = 0.5)

## Population parameters
params_population <- list(POP.SIZE = 250,
                          LANDSCAPE = NA,
                          RISK.MAG = 0.9,
                          PERCEPTION = 0.1,
                          MVT = 0.5,
                          MVT.MOD = 0.1,
                          MEM.DEPTH = 1,
                          MEM.WEIGHT = 0, 
                          RANDOM.START = TRUE)

## Moore parameters
params_moore <- list(LANDSCAPE = NA,
                     EGG.LANDSCAPE = NA,
                     MOORE.RANGE = 2)
