## Run everything together (habitat selection)
source('install_packages.R')
source('header.R')

## Parameter values
# source('parameters.R')

## Generate landscape(s)
source('landscapes/wavelet_landscape.R')

wavescape <- generateWavelet_landscape(LANDSCAPE.SIZE = 64,
                                       ENV = 2,
                                       FRAG = 1, 
                                       PROP.MATRIX = 0, 
                                       RISK.QUANTILE = 0.5)

## Run simulations
source('population_function.R')
sim <- pop.habitatselection(POP.SIZE = 100,
                            LANDSCAPE = wavescape,
                            RISK.MAG = 0.9,
                            PERCEPTION = 0.1,
                            MVT = 0.5,
                            MVT.MOD = 0.1,
                            MEM.DEPTH = 0,
                            MEM.WEIGHT = 0, 
                            RANDOM.START = TRUE)


## Analyze output