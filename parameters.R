### Parameters for running simulations

## Lanscape parameters
params_landscape <- list(LANDSCAPE.SIZE = 32,
                         ENV = -Inf,
                         FRAG = 1, 
                         PROP.MATRIX = 0.2, 
                         RISK.QUANTILE = 0.6)

## Population parameters
params_population <- list(POP.SIZE = 500,
                          LANDSCAPE = NA,
                          RISK.MAG = 0.3,
                          PERCEPTION = 0.5,
                          MVT = 0.5,
                          MVT.MOD = 0.0,
                          MEM.DEPTH = 10,
                          MEM.WEIGHT = 0.1, 
                          RANDOM.START = TRUE)

## Moore parameters
params_moore <- list(LANDSCAPE = NA,
                     EGG.LANDSCAPE = NA,
                     MOORE.RANGE = 4)



