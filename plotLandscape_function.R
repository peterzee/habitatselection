# qq <- generateLandscape(25, 100, 25)
plotLandscape <- function(landscape){
  
  A <- landscape
  
  col.vec <- c(0,'dodgerblue', 'tomato')
    
  
  
  plot(1,1, cex = 0, xlim = c(1,nrow(A)), 
                     ylim=c(1,ncol(A)),
       axes = FALSE, ann = FALSE)
    points(rep(1, ncol(A)), 1:nrow(A),
         pch = 19,
         col = col.vec[A[,1]+1])
    for (i in 2:nrow(A)){
      points(rep(i, ncol(A)), 1:nrow(A),
             pch = 19,
             col = col.vec[A[,i]+1])
    }
  box()
  
  
  
}

# plotLandscape(qq$landscape)
