library(abind)
# source('generateLandscape_function.R')
# 
# world <- generateLandscape(10, 100, 50)

A <- world$landscape
index <- matrix(1:dim(A)[1]^2, ncol = dim(A)[1])

## Effect of risk (deterrence baseline)
risk.mag <- 0.1

## Signal of risk across the landscape
risk.landscape <- A
risk.landscape[risk.landscape != 2] <- NA
## signal variation drawn N(1, sd) -- less than 1 is more risky; greater than 1 is less risky
risk.variation <- 0.0
risk.landscape[which(risk.landscape == 2)] <- rnorm(sum(risk.landscape==2, na.rm = TRUE), 1, risk.variation)


## Determine the risk signals through time (at each patch for each individual)
risk.ind <- which(!is.na(risk.landscape), arr.ind = TRUE)
risk.signals.time <- array(dim = c(time, pop.size))
for (i in 1:time){
  for (j in 1:pop.size){
    risk.signals.time[i,j] <- risk.landscape[locations[i,1,j], locations[i,2,j]]
  }
}

safe.patch <-  array(dim = c(time, pop.size))
safe.patch[t(encounter) == 1] <- 1

## Perception of signal -- [0,1] -- 
## Higher value means indidual can more accurately perceive risk
p <- 0.9

## Draw random values to test for decisions/choices (if 'yes', lay eggs)
safe.patchCHOICE <- ((array(runif(time * pop.size), dim = c(time, pop.size)) * safe.patch) > p)
risk.patchCHOICE <- ((array(runif(time * pop.size), dim = c(time, pop.size)) * (risk.signals.time - risk.mag)) > p)



decisions <- abind(t(encounter), safe.patchCHOICE, risk.patchCHOICE, along = 3)
decision.summary <- colSums(decisions, na.rm = TRUE) / colSums(!is.na(decisions))
colnames(decision.summary) <- c('encounters', 'safe', 'risk')

colMeans(decision.summary, na.rm = TRUE)



