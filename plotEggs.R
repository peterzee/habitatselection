plotEggs <- function(EGG.LANDSCAPE, LANDSCAPE){
  
  A <- LANDSCAPE
  E <- EGG.LANDSCAPE
  par(mar = c(1,1,1,1))
  plot(1,1, cex=0, 
       xlim = c(1,nrow(E)), 
       ylim = c(1,ncol(E)),
       ann = FALSE,
       axes = FALSE)
  box()
  
  gradient <- c(0, 1/ seq(max(E), 1, length.out = max(E)))
  # gradient <- rep(0, max(E) + 1)
    
  points(which(A[1,] == 1), rep(1, sum(A[1,] == 1)), pch = 19, col = rgb(0,0,1, gradient[E[1,which(A[1,] == 1)]+1]), cex = 2)
  points(which(A[1,] == 2), rep(1, sum(A[1,] == 2)), pch = 19, col = rgb(1,0,0, gradient[E[1,which(A[1,] == 2)]+1]), cex = 2)
  
  points(which(A[1,] == 1), rep(1, sum(A[1,] == 1)), pch = 1, col = rgb(0,0,1, 0.6), cex = 2)  
  points(which(A[1,] == 2), rep(1, sum(A[1,] == 2)), pch = 1, col = rgb(1,0,0, 0.6), cex = 2)  
  
  if (sum(E[1, which(A[1,] > 0)]) > 0){
    text(which(A[1,] > 0), rep(1, sum(A[1,] > 0)), labels = E[1, which(A[1,] > 0)], cex = 0.7)
  }
  
  for (i in 2:ncol(E)){
  
      points(which(A[i,] == 1), rep(i, sum(A[i,] == 1)), pch = 19, col = rgb(0,0,1, gradient[E[i,which(A[i,] == 1)]+1]), cex = 2)
      points(which(A[i,] == 2), rep(i, sum(A[i,] == 2)), pch = 19, col = rgb(1,0,0, gradient[E[i,which(A[i,] == 2)]+1]), cex = 2)
    
      points(which(A[i,] == 1), rep(i, sum(A[i,] == 1)), pch = 1, col = rgb(0,0,1, 0.6), cex = 2)  
      points(which(A[i,] == 2), rep(i, sum(A[i,] == 2)), pch = 1, col = rgb(1,0,0, 0.6), cex = 2)  
      
      if (sum(E[i, which(A[i,] > 0)]) > 0){
        text(which(A[i,] > 0), rep(i, sum(A[i,] > 0)), labels = E[i, which(A[i,] > 0)], cex = 0.7)
      }
    }
  
  
}

# plotEggs(egg.landscape, quilted)

