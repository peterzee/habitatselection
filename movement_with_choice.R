library(philentropy)
source('generateLandscape_function.R')

world <- generateLandscape(50, 1000, 50)

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


## Egg landscape
egg.landscape <- array(0, dim(A))


##movement
start.loc <- sample(world$inputs[[1]]^2, 1)
start.ind <- which(index == start.loc, arr.ind=TRUE)


START <- start.ind
time <- 150
locations <- array(dim = c(time, 2))
locations[1,] <- start.ind
encounter <- rep(NA, time)
  encounter[1] <- A[locations[1,1], locations[1,2]]

risk.signals.time <- rep(NA, time)
safe.patch <- rep(NA, time)


safe.choices.time <- rep(NA, time)
risk.choices.time <- rep(NA, time)

## Perception cutoff
p <- 0.8

mvt.par <- 0.6
egg.check <- 0

distance.tracker <- array(NA,dim=c(time, 2))

## Energy 

energy <- c(1, rep(NA, time-1))


for (i in 2:time){
  
  
  ### perceiving through time 
  risk.signals.time[i-1] <- risk.landscape[locations[i-1,1], locations[i-1,2]]
  safe.patch[i-1] <- (encounter[i-1] == 1)*1
  
  ### Choice through time
  
  
  ##  1. Safe patch location
  if (encounter[i-1] == 1){
    
    ## random draw to determine choice
    safe.patchCHOICE <- ((runif(1) * safe.patch[i-1]) > p)
    
      if (safe.patchCHOICE == TRUE){
        
        ## Lay eggs (add eggs to patch)
        egg.check <- 1
        egg.landscape[locations[i-1,1], locations[i-1,2]] <- egg.landscape[locations[i-1,1], locations[i-1,2]] + 1 
        
        safe.choices.time[i-1] <- 1
      
      } else {
          
          egg.check <- 0
          safe.choices.time[i-1] <- 0
        
          }
    }
  
  ## 2. Risky location
  
  ## default 'mvt.mod' = 0 (and gets reset) // only changes in case of declined risky patch
  mvt.mod <- 0
  
  if (encounter[i-1] == 2){
    
    ## random draw to determine choice (modified by both magnitude, and signal)
    risk.patchCHOICE <- ((runif(1) * (risk.signals.time[i-1] - risk.mag)) > p)
      
      if (risk.patchCHOICE == TRUE){
        
        ## Lay eggs (add eggs to patch)
        egg <- 1
        egg.landscape[locations[i-1,1], locations[i-1,2]] <- egg.landscape[locations[i-1,1], locations[i-1,2]] + 1 
        
        risk.choices.time[i-1] <- 1
      } else {    
          egg <- 0
          risk.choices.time[i-1] <- 0
          mvt.mod <- 0.1
        
          }
  }
  
  
  ### If adults layed egg, drop out of simulation (eg die, stop moving, etc)
  
  if (egg.check == 1){
    print('dropped dead')
    break
  }
  
  ## Dispersal kernal
  dist <- rgeom(1, prob = mvt.par - mvt.mod) + 1
  theta <- runif(1, 0, 2*pi)
    x <- cos(theta) * dist
    y <- sin(theta) * dist
  
  landing <- round(cbind(x,y))
  
  tmp.mvt <- locations[i-1,] + landing
  
  locations[i,] <- cbind(tmp.mvt[,1] %% ncol(A),
                         tmp.mvt[,2] %% nrow(A))
    
    ## change invidiauls landing at '0' index to the edge
    locations[which(locations == 0)] <- sample(length(which(locations==0)), c(1, nrow(A)))

  encounter[i] <- A[locations[i,1], locations[i,2]]  
    
  
    ## Logging the distances moved
    kings.dist <- distance(rbind(locations[i-1,], locations[i,]), method = 'chebyshev')
    distance.tracker[i,] <-  c(dist, kings.dist)
    
    
    ### Energy spent during movement
    energy[i] <- energy[i-1] * 0.95
    # if (energy <= 0){
    #   break
    # }

    
    # print(i)
}



#

### Trace plot of individual movement
plot(1,1, cex=0, xlim=c(0,ncol(A)), ylim=c(0,ncol(A)))
points(locations[1,1], locations[1,2], pch = 19)
points(locations[i-1,1], locations[i-1,2], pch = 19, col = 2)
for (j in 2:time){
  points(locations[j,1], locations[j,2], pch = 19, col =rgb(0,0,0,0.5), cex = 0.5)
  arrows(x0 = locations[j-1,1], y0 = locations[j-1,2],
         x1 = locations[j,1], y1 = locations[j,2],
         length = 0)
}

#
