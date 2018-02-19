## 
   pop.habitatselection <- function(POP.SIZE, LANDSCAPE, RISK.MAG, PERCEPTION, MVT, MVT.MOD){   
      
      A <- LANDSCAPE
      # plotLandscape(A)
      
      patch.breakdown <- c(nrow(A)^2, sum(A==0), sum(A == 1), sum(A==2))
      names(patch.breakdown) <- c('total', 'empty', 'safe', 'risky')
      patch.breakdown
      
      index <- matrix(1:dim(A)[1]^2, ncol = dim(A)[1])
      
      
      ## Effect of risk (deterrence baseline)
      risk.mag <- RISK.MAG
      
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
      
      #####
      ## Movement for multiple individuals (population)
      pop.size <- POP.SIZE
      pop.starts <- sample(nrow(A)^2, pop.size, replace = TRUE)
      pop.start.ind <- array(dim = c(pop.size, 2))
      for (i in 1:pop.size){
        pop.start.ind[i,] <- which(index == pop.starts[i], arr.ind = TRUE)
      }
      
      ## Timesteps to move around
      time <- 50
      
      START <- pop.start.ind
      locations <- array(dim = c(time, 2, pop.size))
      locations[1,,] <- t(pop.start.ind)
      encounter <- array(0, dim = c(time, pop.size))
      for (i in 1:pop.size){
        encounter[1,i] <- A[locations[1,1,i], locations[1,2,i]]
      }
      
      
      
      risk.signals.time <- array(NA, dim = c(time, pop.size))
      safe.patch <- array(NA, dim = c(time, pop.size))
      
      
      safe.choices.time <-  array(NA, dim = c(time, pop.size))
      risk.choices.time <-  array(NA, dim = c(time, pop.size))
      
      ## Perception cutoff
      # p <- 0.8
      p.vec <- rep(PERCEPTION, pop.size)
      
      distance.tracker <- array(NA, dim = c(time, pop.size))
      
      ## Energy 
      energy <- rbind(rep(1, pop.size), array(NA, dim = c(time-1, pop.size)))
      
      drop.dead <- array(dim = c(pop.size, 2))
      
      for (i in 2:time){
        
        mvt.par <- MVT
        
        for (j in 1:pop.size){
          if (is.na(drop.dead[j,1])){
            egg.check <- 0
            
            ### perceiving through time 
            risk.signals.time[i-1,j] <- risk.landscape[locations[i-1,1,j], locations[i-1,2,j]]
            safe.patch[i-1,j] <- (encounter[i-1,j] == 1)*1
            
            ### Choice through time
            ##  1. Safe patch location
            if (encounter[i-1,j] == 1){
              
              ## random draw to determine choice
              safe.patchCHOICE <- ((runif(1) * safe.patch[i-1,j]) > p.vec[j])
              
              if (safe.patchCHOICE == TRUE){
                
                ## Lay eggs (add eggs to patch)
                egg.check <- 1
                egg.landscape[locations[i-1,1,j], locations[i-1,2,j]] <- egg.landscape[locations[i-1,1,j], locations[i-1,2,j]] + 1 
                
                safe.choices.time[i-1,j] <- 1
                
              } else {
                
                egg.check <- 0
                safe.choices.time[i-1,j] <- 0
                
              }
            }
            
            ## 2. Risky location
            
            ## default 'mvt.mod' = 0 (and gets reset) // only changes in case of declined risky patch
            mvt.mod <- 0
            
            if (encounter[i-1,j] == 2){
              
              ## random draw to determine choice (modified by both magnitude, and signal)
              risk.patchCHOICE <- ((runif(1) * (risk.signals.time[i-1,j])) > p.vec[j] + risk.mag)
              
              if (risk.patchCHOICE == TRUE){
                
                ## Lay eggs (add eggs to patch)
                egg.check <- 1
                egg.landscape[locations[i-1,1,j], locations[i-1,2,j]] <- egg.landscape[locations[i-1,1,j], locations[i-1,2,j]] + 1 
                
                risk.choices.time[i-1,j] <- 1
              } else {    
                egg.check <- 0
                risk.choices.time[i-1,j] <- 0
                mvt.mod <- MVT.MOD
                
              }
            }
            
            
            # ### If adults layed egg, drop out of simulation (eg die, stop moving, etc)
            if (egg.check == 1){
              drop.dead[j,] <- c(1,i)
            }
            
            ## Dispersal kernal
            dist <- rgeom(1, prob = mvt.par - mvt.mod) + 1
            theta <- runif(1, 0, 2*pi)
            x <- cos(theta) * dist
            y <- sin(theta) * dist
            
            landing <- round(cbind(x,y))
            
            tmp.mvt <- locations[i-1,,j] + landing
            
            tmp.mvt[tmp.mvt <= 0] <- tmp.mvt[tmp.mvt <= 0] - 1
            tmp.mvt[tmp.mvt > (nrow(A))] <- tmp.mvt[tmp.mvt > (nrow(A))] + 1
            
            locations[i,,j] <- cbind(tmp.mvt[,1] %% (ncol(A) + 1),
                                     tmp.mvt[,2] %% (nrow(A) + 1))
            
            ## change invidiauls landing at '0' index to the edge
            locations[which(locations == 0)] <- sample(length(which(locations==0)), c(1, nrow(A)))
            
            encounter[i,j] <- A[locations[i,1,j], locations[i,2,j]]  
            
            
            ## Logging the distances moved
            # kings.dist <- distance(rbind(locations[i-1,,j], locations[i,,j]), method = 'chebyshev')
            distance.tracker[i,j] <-  dist
            
            
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
      } ## time
      
  
      
      ## output
      inputs <- list(POP.SIZE = POP.SIZE,
                     LANDSCAPE = LANDSCAPE,
                     RISK.MAG = RISK.MAG,
                     PERCEPTION = PERCEPTION,
                     MVT = MVT,
                     MVT.MOD = MVT.MOD)
      
      output <- list(inputs = inputs,
                     A = A,
                     egg.landscape = egg.landscape,
                     distance.tracker = distance.tracker)
      
      return(output)
   }
   #  
   # 
   # patch.dim <- 10
   # x <- array(sample(0:2, patch.dim^2, prob = c(0,1,5), replace = TRUE), dim = c(patch.dim, patch.dim))
   # 
   # sim <- pop.habitatselection(POP.SIZE = 100,
   #                             LANDSCAPE = x,
   #                             RISK.MAG = 0.8,
   #                             PERCEPTION = 0.1,
   #                             MVT = 0.9,
   #                             MVT.MOD = 0.1)
   # 