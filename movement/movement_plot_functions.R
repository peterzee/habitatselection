
source('dispersalKernal.R')
source('traceMovement.R')
source('kernalPlot.R')

# ## Plot the disperasl kernal from a focal indidivual
b <- disp.kernal(N = 500, 5, -4, LANDSCAPE = wavescape)

plotLandscape(wavescape)
kernalPlot(wavescape, add = FALSE)

pickIndividual <- sample(100, 25)
for (i in 1:length(pickIndividual)){
  traceMovement(individual = pickIndividual[ i ], trace.data = sim$locations[,,pickIndividual[ i ]], landscape = wavescape, overlay = TRUE)
}
