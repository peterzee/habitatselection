library(philentropy)
source('generateLandscape_function.R')

patch.dim <- 10

# world <- generateLandscape(patch.dim, (patch.dim^2) * 0.8, 50)
# A <- world$landscape
# x <- array(0:2, dim = c(patch.dim, patch.dim))

# x <- array(sample(0:2, patch.dim^2, prob = c(0,1,5), replace = TRUE), dim = c(patch.dim, patch.dim))
A <- quilted
# plotLandscape(A)

patch.breakdown <- c(nrow(A)^2, sum(A==0), sum(A == 1), sum(A==2))
names(patch.breakdown) <- c('total', 'empty', 'safe', 'risky')
patch.breakdown

index <- matrix(1:dim(A)[1]^2, ncol = dim(A)[1])

## Number of species
num.spp <- 10

## Effect of risk (deterrence baseline)
risk.mag <- rep(0.8, num.spp)
# risk.mag <- runif(num.spp)

## Signal of risk across the landscape
risk.landscape <- A
risk.landscape[risk.landscape != 2] <- NA

## signal variation drawn N(1, sd) -- less than 1 is more risky; greater than 1 is less risky
risk.variation <- 0.0
risk.landscape[which(risk.landscape == 2)] <- rnorm(sum(risk.landscape==2, na.rm = TRUE), 1, risk.variation)

## Determine the risk signals through time (at each patch where an individual arrives through movement)
risk.ind <- which(!is.na(risk.landscape), arr.ind = TRUE)

## Egg landscape
egg.landscape <- array(0, dim = c(nrow(A), ncol(A), num.spp))

#####
## Movement for multiple individuals (population)
pop.size <- 100
pop.start.ind <- array(dim = c(pop.size, 2, num.spp))
for (j in 1:num.spp){
    pop.starts <- sample(nrow(A)^2, pop.size, replace = TRUE)
  for (i in 1:pop.size){
    pop.start.ind[i,,j] <- which(index == pop.starts[i], arr.ind = TRUE)
  }
}
## Timesteps to move around
time <- 100

START <- pop.start.ind
locations <- array(dim = c(time, 2, pop.size, num.spp))
for (j in 1:pop.size){
  for (i in 1:num.spp){
    locations[1,,j,i] <- (pop.start.ind[j,,i])
  }
}


encounter <- array(0, dim = c(time, pop.size, num.spp))
for (j in 1:num.spp){
  for (i in 1:pop.size){
    encounter[1,i,j] <- A[locations[1,1,i,j], locations[1,2,i,j]]
  }
}



risk.signals.time <- array(NA, dim = c(time, pop.size, num.spp))
safe.patch <- array(NA, dim = c(time, pop.size, num.spp))


safe.choices.time <-  array(NA, dim = c(time, pop.size, num.spp))
risk.choices.time <-  array(NA, dim = c(time, pop.size, num.spp))

## Perception cutoff
# p <- 0.8
# p.vec <- rep(0.1, pop.size)
p.array <- array(0.1, dim = c(pop.size, num.spp))

distance.tracker <- array(NA, dim = c(time, pop.size, num.spp))

## Energy 
# energy <- rbind(rep(1, pop.size), array(NA, dim = c(time-1, pop.size)))
# comm.energy <- 
  
drop.dead <- array(dim = c(pop.size, 2, num.spp))

wrap.info <- array(dim = c(time, 6, pop.size, num.spp))
wrap.info[,1,,] <- 0


for (i in 2:time){
  
  mvt.par <- rep(0.9, num.spp)

  for (k in 1:num.spp){
    for (j in 1:pop.size){
      if (is.na(drop.dead[j,1,k])){
        
        egg.check <- 0
        
        ### perceiving through time 
        risk.signals.time[i-1,j,k] <- risk.landscape[locations[i-1,1,j,k], locations[i-1,2,j,k]]
        safe.patch[i-1,j,k] <- (encounter[i-1,j,k] == 1)*1
        
        ### Choice through time
        ##  1. Safe patch location
        if (encounter[i-1,j,k] == 1){
          
          ## random draw to determine choice
          safe.patchCHOICE <- ((runif(1) * safe.patch[i-1,j,k]) > p.array[j,k])
          
          if (safe.patchCHOICE == TRUE){
            
            ## Lay eggs (add eggs to patch)
            egg.check <- 1
            egg.landscape[locations[i-1,1,j,k], locations[i-1,2,j,k],k] <- egg.landscape[locations[i-1,1,j,k], locations[i-1,2,j,k],k] + 1 
            
            safe.choices.time[i-1,j,k] <- 1
            
          } else {
            
            egg.check <- 0
            safe.choices.time[i-1,j,k] <- 0
            
          }
        }
        
        ## 2. Risky location
        
        ## default 'mvt.mod' = 0 (and gets reset) // only changes in case of declined risky patch
        mvt.mod <- 0
        
        if (encounter[i-1,j,k] == 2){
          
          ## random draw to determine choice (modified by both magnitude, and signal)
          risk.patchCHOICE <- ((runif(1) * (risk.signals.time[i-1,j,k])) > p.array[j,k] + risk.mag[k])
          
          if (risk.patchCHOICE == TRUE){
            
            ## Lay eggs (add eggs to patch)
            egg.check <- 1
            egg.landscape[locations[i-1,1,j,k], locations[i-1,2,j,k],k] <- egg.landscape[locations[i-1,1,j,k], locations[i-1,2,j,k],k] + 1 
            
            risk.choices.time[i-1,j,k] <- 1
            
          } else {    
            
            egg.check <- 0
            risk.choices.time[i-1,j,k] <- 0
            mvt.mod <- 0.19
            
          }
        }
        
        
        # ### If adults layed egg, drop out of simulation (eg die, stop moving, etc)
        if (egg.check == 1){
          drop.dead[j,,k] <- c(1,i)
        }
        
        ## Dispersal kernal
        realized.disp.prob <- mvt.par[k] - mvt.mod
          if (realized.disp.prob < 0.01){
            realized.disp.prob <- 0.01
          }
        dist <- rgeom(1, prob = realized.disp.prob) + 1
        theta <- runif(1, 0, 2*pi)
          x <- cos(theta) * dist
          y <- sin(theta) * dist
        
        landing <- round(cbind(x,y))
        
        tmp.mvt <- locations[i-1,,j,k] + landing
        
        wrap.info[i,2:3,j,k] <- tmp.mvt
        if(sum((tmp.mvt > dim(A)[1] | tmp.mvt < 1)) > 0){
          wrap.info[i,1,j,k] <- 1
        }
        
          tmp.mvt[tmp.mvt <= 0] <- tmp.mvt[tmp.mvt <= 0] - 1
          tmp.mvt[tmp.mvt > (nrow(A))] <- tmp.mvt[tmp.mvt > (nrow(A))] + 1
    
        locations[i,,j,k] <- cbind(tmp.mvt[,1] %% (ncol(A) + 1),
                                   tmp.mvt[,2] %% (nrow(A) + 1))
        
        ## change invidiauls landing at '0' index to the edge
        locations[which(locations == 0)] <- sample(length(which(locations==0)), c(1, nrow(A)))
        
        encounter[i,j,k] <- A[locations[i,1,j,k], locations[i,2,j,k]]  
        
        
        ## Logging the distances moved
        # kings.dist <- distance(rbind(locations[i-1,,j], locations[i,,j]), method = 'chebyshev')
        distance.tracker[i,j,k] <-  dist
        
        
        ### Energy spent during movement
        # energy[i,j] <- energy[i-1,j] * 0.95
        # if (energy <= 0){
        #   break
        # }
        
        # print(i)
        
      } else {
        j <- j+1 ## anybody home? (forego individual if dropped dead
      }
    } ## individuals
  } ## species 
} ## time


