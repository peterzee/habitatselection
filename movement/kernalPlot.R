# ## Plot the disperasl kernal from a focal indidivual
kernalPlot <- function(landscape, add = FALSE){
  if (add == TRUE){
    par(new = TRUE)
  }
  
  A <- landscape
  
  plot(1,1, cex=0, 
       xlim=c(1,ncol(A)), ylim=c(1,ncol(A)), 
       ann = FALSE, axes = FALSE)
  points(ncol(A)/2, ncol(A)/2, pch = 4, cex=3)
  
  points(b$mod.locations[,1], b$mod.locations[,2], col = rgb(0.76, 0 , 0.76, 0.1), pch = 19, cex = 3, lwd = 2)
  points(b$unmod.locations[,1], b$unmod.locations[,2], col = rgb(0,0.5,0.5,0.1), pch = 19, cex = 2, lwd = 2)
  
  
  
  
  box()
}