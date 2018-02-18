source('generateLandscape_function.R')

world <- generateLandscape(50, 100, 50)

A <- world$landscape
index <- matrix(1:dim(A)[1]^2, ncol = dim(A)[1])

##movement for multiple individuals (population)
pop.size <- 100
pop.starts <- sample(world$inputs[[1]]^2, pop.size)
pop.start.ind <- array(dim = c(pop.size, 2))
for (i in 1:pop.size){
  pop.start.ind[i,] <- which(index == pop.starts[i], arr.ind = TRUE)
}


START <- pop.start.ind
time <- 100
locations <- array(dim = c(time, 2, pop.size))
locations[1,,] <- t(pop.start.ind)

for (i in 2:time){

  ##kernal
dist <- rgeom(pop.size, prob = 0.9) + 1
# dist <- rnorm(1000, 5, 5)
# dist <- 3 * rgamma(1000, 8, 3)
theta <- runif(pop.size, 0, 2*pi)
  x <- cos(theta) * dist
  y <- sin(theta) * dist
  
landing <- round(cbind(x,y))

tmp.mvt <- locations[i-1,,] + t(landing)

  tmp.mvt[tmp.mvt <= 0] <- tmp.mvt[tmp.mvt <= 0] - 1
  tmp.mvt[tmp.mvt > (nrow(A))] <- tmp.mvt[tmp.mvt > (nrow(A))] + 1

locations[i,,j] <- cbind(tmp.mvt[,1] %% (ncol(A) + 1),
                         tmp.mvt[,2] %% (nrow(A) + 1))

}


## Environment encounters through time
encounter <- array(dim=c(pop.size, time))
for (i in 1:time){
  for (j in 1:pop.size){
  encounter[j,i] <- A[locations[i,1,j], locations[i,2,j]]  
  }
}

# mean(rowSums(encounter == 1))
# mean(rowSums(encounter == 2))





# ########################################################################################
# ########################################################################################
# 
# plotLandscape(A)
# ### Trace plot of individual movement
# # plot(1,1, cex=0, xlim=c(0,ncol(A)), ylim=c(0,ncol(A)))
# for (j in 1:pop.size){
# points(locations[1,1,j], locations[1,2,j], pch = 19)
# points(locations[time,1,j], locations[time,2,j], pch = 19, col = 2)
# for (i in 2:time){
#   points(locations[i,1,j], locations[i,2,j], pch = 19, col =rgb(0,0,0,0.5), cex = 0.75)
#   arrows(x0 = locations[i-1,1,j], y0 = locations[i-1,2,j],
#          x1 = locations[i,1,j], y1 = locations[i,2,j],
#         length = 0, col= j,
#         lwd = 2)
# }
# }
# 

