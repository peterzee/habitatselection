### Parameters for running simulations

## Lanscape parameters
params_landscape <- list(LANDSCAPE.SIZE = 64,
                         ENV = 6,
                         FRAG = 1, 
                         PROP.MATRIX = 0, 
                         RISK.QUANTILE = 0.5)

## Population parameters
params_population <- list(POP.SIZE = 250,
                          LANDSCAPE = NA,
                          RISK.MAG = 0.1,
                          PERCEPTION = 0.7,
                          MVT = 0.1,
                          MVT.MOD = 0.0,
                          MEM.DEPTH = 10,
                          MEM.WEIGHT = 0.1, 
                          RANDOM.START = TRUE)

## Moore parameters
params_moore <- list(LANDSCAPE = NA,
                     EGG.LANDSCAPE = NA,
                     MOORE.RANGE = 1)



