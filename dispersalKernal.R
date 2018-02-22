## Dispersal kernal
disp.kernal <- function(N, MVT.PAR, MVT.MOD, LANDSCAPE){
  
    A <- LANDSCAPE  
  
    mvt.par <- MVT.PAR
    mvt.mod <- MVT.MOD
    
    
    ### Unodified!

    dist <- rgeom(N, prob = mvt.par) + 1
    theta <- runif(N, 0, 2*pi)
    x <- cos(theta) * dist
    y <- sin(theta) * dist
    
    landing <- round(cbind(x,y))
    
    tmp.mvt <- c(ncol(A)/2, ncol(A)/2) + landing
    
    tmp.mvt[tmp.mvt <= 0] <- tmp.mvt[tmp.mvt <= 0] - 1
    tmp.mvt[tmp.mvt > (nrow(A))] <- tmp.mvt[tmp.mvt > (nrow(A))] + 1
    
    unmod.locations <- cbind(tmp.mvt[,1] %% (ncol(A) + 1),
                             tmp.mvt[,2] %% (nrow(A) + 1))
    
    ## change invidiauls landing at '0' index to the edge
    unmod.locations[which(unmod.locations == 0)] <- sample(length(which(unmod.locations==0)), c(1, nrow(A)))
 
    
    ### Modified!
    realized.disp.prob <- mvt.par - mvt.mod
    
    if (realized.disp.prob < 0.01){
      realized.disp.prob <- 0.01
    }
    dist <- rgeom(N, prob = realized.disp.prob) + 1
    theta <- runif(N, 0, 2*pi)
    x <- cos(theta) * dist
    y <- sin(theta) * dist
    
    landing <- round(cbind(x,y))
    
    tmp.mvt <- c(ncol(A)/2, ncol(A)/2) + landing
    
    tmp.mvt[tmp.mvt <= 0] <- tmp.mvt[tmp.mvt <= 0] - 1
    tmp.mvt[tmp.mvt > (nrow(A))] <- tmp.mvt[tmp.mvt > (nrow(A))] + 1
    
    mod.locations <- cbind(tmp.mvt[,1] %% (ncol(A) + 1),
                             tmp.mvt[,2] %% (nrow(A) + 1))
    
    ## change invidiauls landing at '0' index to the edge
    mod.locations[which(mod.locations == 0)] <- sample(length(which(mod.locations==0)), c(1, nrow(A)))
    
    
    
    kernal.out <- list(unmod.locations = unmod.locations,
                       mod.locations = mod.locations)
    return(kernal.out)   
}

# 
# x <- disp.kernal(100, 0.9, 0.2)
# x
