plotEggs <- function(EGG.LANDSCAPE, LANDSCAPE){
  
  A <- LANDSCAPE
  E <- EGG.LANDSCAPE
  
  plot(1,1, cex=0, 
       xlim = c(1,nrow(E)), 
       ylim = c(1,ncol(E)))
  
  gradient <- c(0, 1/ seq(max(E), 1, length.out = max(E)))
  
  points(1:ncol(E), rep(1, ncol(E)), pch = 19, col = rgb(1,0,0, gradient[E[1,]+1]), cex = 2)
  
  points(which(A[1,] == 1), rep(1, sum(A[1,] == 1)), pch = 1, col = rgb(0,0,1, 1), cex = 2)  
  points(which(A[1,] == 2), rep(1, sum(A[1,] == 2)), pch = 1, col = rgb(1,0,0, 1), cex = 2)  
  
  
  # text(which(A[1,] > 0), rep(1, sum(A[1,] >0)), labels = E[1, which(A[1,] > 0)], cex = 0.7)
  
  for (i in 2:ncol(E)){
  
      points(1:ncol(E), rep(i, ncol(E)), pch = 19, col = rgb(1,0,0, gradient[E[i,]+1]), cex = 2)
  
      points(which(A[i,] == 1), rep(i, sum(A[i,] == 1)), pch = 1, col = rgb(0,0,1, 1), cex = 2)  
      points(which(A[i,] == 2), rep(i, sum(A[i,] == 2)), pch = 1, col = rgb(1,0,0, 1), cex = 2)  
      
      text(which(A[i,] > 0), rep(i, sum(A[i,] >0)), labels = E[i, which(A[i,] > 0)], cex = 0.7)
      
    }
  
  
}

plotEggs(egg.landscape, quilted)
