
source('movement/dispersalKernal.R')
source('movement/traceMovement.R')
source('movement/kernalPlot.R')


wavescape <- generateWavelet_landscape(LANDSCAPE.SIZE = 64,
                                       ENV = 2,
                                       FRAG = 2,
                                       PROP.MATRIX = 0.0,
                                       RISK.QUANTILE = 0.6)
sim <- pop.habitatselection(POP.SIZE = 250,
                            LANDSCAPE = wavescape, #a$module.landscape,
                            RISK.MAG = 0.7,
                            PERCEPTION = 0.1,
                            MVT = 9,
                            MVT.MOD = 0,
                            MEM.DEPTH = 5,
                            MEM.WEIGHT = 0.1)


# ## Plot the disperasl kernal from a focal indidivual
b <- disp.kernal(N = 500, 5, -4, LANDSCAPE = wavescape)

plotLandscape(wavescape)
# kernalPlot(wavescape, add = FALSE)

pickIndividual <- sample(100, 25)
for (i in 1:length(pickIndividual)){
  traceMovement(individual = pickIndividual[ i ], trace.data = sim$locations[,,pickIndividual[ i ]], landscape = wavescape, overlay = TRUE)
}
