par(mfrow=c(1,1), mar = rep(0.5, 4))
plotLandscape(quilted)

for (k in sample(pop.size, 1)){
  traceMovement(k, locations[,,k], quilted, overlay = TRUE)
}

