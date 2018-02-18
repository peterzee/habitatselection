source('generateLandscape_function.R')

world <- generateLandscape(25, 100, 50)

A <- world$landscape
index <- matrix(1:dim(A)[1]^2, ncol = dim(A)[1])

## Effect of risk (deterrence baseline)
risk.mag <- 0.3

## Signal of risk across the landscape
risk.landscape <- A
risk.landscape[risk.landscape != 2] <- NA
## signal variation drawn N(1, sd) -- less than 1 is more risky; greater than 1 is less risky
risk.variation <- 0.0
risk.landscape[which(risk.landscape == 2)] <- rnorm(sum(risk.landscape==2, na.rm = TRUE), 1, risk.variation)


## Determine the risk signals through time (at each patch where an individual arrives through movement)
risk.ind <- which(!is.na(risk.landscape), arr.ind = TRUE)
risk.signals.time <- rep(NA, time)
for (i in 1:time){
  risk.signals.time[i] <- risk.landscape[locations[i,1], locations[i,2]]
}


safe.patch <- rep(NA, time)
safe.patch[encounter == 1] <- 1


## Perception of signal -- [0,1] -- 
## Higher value means indidual can more accurately perceive risk
p <- 0.9

## Draw random values to test for decisions/choices (if 'yes', lay eggs)
safe.patchCHOICE <- ((runif(time) * safe.patch) > p)
risk.patchCHOICE <- ((runif(time) * (risk.signals.time - risk.mag)) > p)



decisions <- cbind(encounter, safe.patchCHOICE, risk.patchCHOICE)
colSums(decisions, na.rm = TRUE) / colSums(!is.na(decisions))
