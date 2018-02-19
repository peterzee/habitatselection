source('generateLandscape_function.R')

world <- generateLandscape(10, 75, 50)

A <- world$landscape
index <- matrix(1:dim(A)[1]^2, ncol = dim(A)[1])

##movement
start.loc <- sample(world$inputs[[1]]^2, 1)
start.ind <- which(index == start.loc, arr.ind=TRUE)


START <- start.ind
time <- 100
locations <- array(dim = c(time, 2))
locations[1,] <- start.ind

encounter <- rep(NA, time)

for (i in 2:time){
    ## Dispersal kernal
    dist <- rgeom(1, prob = 0.6) + 1
    theta <- runif(1, 0, 2*pi)
      x <- cos(theta) * dist
      y <- sin(theta) * dist
      
    landing <- round(cbind(x,y))
    
    tmp.mvt <- locations[i-1,] + landing
    
      tmp.mvt[tmp.mvt <= 0] <- tmp.mvt[tmp.mvt <= 0] - 1
      tmp.mvt[tmp.mvt > (nrow(A))] <- tmp.mvt[tmp.mvt > (nrow(A))] + 1
    
    locations[i,] <- cbind(tmp.mvt[,1] %% (ncol(A) + 1),
                           tmp.mvt[,2] %% (nrow(A) + 1))
    
    encounter[i] <- A[locations[i,1], locations[i,2]]  

    }





### Trace plot of individual movement
plot(1,1, cex=0, xlim=c(0,ncol(A)), ylim=c(0,ncol(A)), ann=FALSE, axes=FALSE)
points(locations[1,1], locations[1,2], pch = 19)
points(locations[time,1], locations[time,2], pch = 19, col = 2)
for (i in 2:time){
  points(locations[i,1], locations[i,2], pch = 19, col =rgb(0,0,0,0.5), cex = 0.5)
  arrows(x0 = locations[i-1,1], y0 = locations[i-1,2],
         x1 = locations[i,1], y1 = locations[i,2],
        length = 0)
}
box()





### 