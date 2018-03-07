# ###### IN PROGRESS ######
# A <- a$module.landscape
# E <- comm.out$egg.landscape

# index <- array(1:length(A), dim = c(nrow(A), ncol(A)))
# aDiv <- array(NA, dim = c(nrow(A), ncol(A)))
# 
# shannon.div <- array(NA, dim = c(nrow(A), ncol(A)))
# 
# for (i in 1:length(index)){
#   
#   id <- which(index == i, arr.ind =TRUE)  
#   loc.comm <- E[id[1], id[2],]
#   aDiv[i] <- sum(loc.comm > 0)
#   shannon.div[i] <- diversity(loc.comm, index = 'shannon')
#   
# }
library(vegan)


  plot.community.eggs <- function(EGG.LANDSCAPE, LANDSCAPE, METRIC = 'shannon', overlay = TRUE){
  
  if (overlay == TRUE){
    par(new = TRUE)
  }
    
  A <- LANDSCAPE
  E <- EGG.LANDSCAPE
  
  index <- array(1:length(A), dim = c(nrow(A), ncol(A)))
  aDiv <- array(NA, dim = c(nrow(A), ncol(A)))
  
  shannon.div <- array(NA, dim = c(nrow(A), ncol(A)))
  
  for (i in 1:length(index)){
    
    id <- which(index == i, arr.ind =TRUE)  
    loc.comm <- E[id[1], id[2],]
    aDiv[i] <- sum(loc.comm > 0)
    shannon.div[i] <- diversity(loc.comm, index = 'shannon')
    
  }
  
  par(mar = c(1,1,1,1))
  plot(1,1, cex=0, 
       xlim = c(1,nrow(A)), 
       ylim = c(1,ncol(A)),
       ann = FALSE,
       axes = FALSE)
  box()
  
  
  
 
  if (METRIC == 'shannon'){
    
  points(which(A[1,] > 0), rep(1, sum(A[1,] > 0)), pch = 19, col = rgb(0,0,1, shannon.div[1,]/max(shannon.div)), cex = 2)
  points(which(A[1,] > 0), rep(1, sum(A[1,] > 0)), pch = 1, cex = 2)

    for (i in 2:nrow(A)){
    
    points(which(A[i,] > 0), rep(i, sum(A[i,] > 0)), pch = 19, col = rgb(0,0,1, shannon.div[i,]/max(shannon.div)), cex = 2)
    points(which(A[i,] > 0), rep(i, sum(A[i,] > 0)), pch = 1, cex = 2)
      
  }
  } else {
    
    points(which(A[1,] > 0), rep(1, sum(A[1,] > 0)), pch = 19, col = rgb(0,0,1, aDiv[1,]/max(aDiv)), cex = 2)
    points(which(A[1,] > 0), rep(1, sum(A[1,] > 0)), pch = 1, cex = 2)
    
    for (i in 2:nrow(A)){
      
      points(which(A[i,] > 0), rep(i, sum(A[i,] > 0)), pch = 19, col = rgb(0,0,1, aDiv[i,]/max(aDiv)), cex = 2)
      points(which(A[i,] > 0), rep(i, sum(A[i,] > 0)), pch = 1, cex = 2)
      
    }
  }
    
  
  
}

par(mfrow=c(2,1))
plotLandscape(a$module.landscape)
plot.community.eggs(comm.out$egg.landscape, a$module.landscape, overlay = FALSE)
plot.community.eggs(comm.out$egg.landscape, a$module.landscape, METRIC = "aDiv", overlay = FALSE)

