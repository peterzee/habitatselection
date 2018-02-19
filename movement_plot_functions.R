## Movement plots

traceMovement <- function(trace.data, landscape, overlay = TRUE){
    
  locations <- trace.data
  A <- landscape
  
  if (overlay == TRUE){
  
  points(1,1, cex=0, xlim=c(0,ncol(A)), ylim=c(0,ncol(A)))
    points(locations[1,1], locations[1,2], pch = 19)
    points(locations[time,1], locations[time,2], pch = 19, col = 2)
    for (i in 2:time){
      points(locations[i,1], locations[i,2], pch = 19, col =rgb(0,0,0,0.5), cex = 0.5)
      arrows(x0 = locations[i-1,1], y0 = locations[i-1,2],
             x1 = locations[i,1], y1 = locations[i,2],
             length = 0)
    }
  } else {
          plot(1,1, cex=0, xlim=c(0,ncol(A)), ylim=c(0,ncol(A)))
          points(locations[1,1], locations[1,2], pch = 19)
          points(locations[time,1], locations[time,2], pch = 19, col = 2)
          for (i in 2:time){
            points(locations[i,1], locations[i,2], pch = 19, col =rgb(0,0,0,0.5), cex = 0.5)
            arrows(x0 = locations[i-1,1], y0 = locations[i-1,2],
                   x1 = locations[i,1], y1 = locations[i,2],
                   length = 0)
          }
        }
}


# ## Plot the disperasl kernal from a focal indidivual
# kernalPlot <- function(landscape){
# 
#   
#     plot(1,1, cex=0, xlim=c(0,ncol(A)), ylim=c(0,ncol(A)))
#     points(ncol(A)/2, ncol(A)/2, pch = 19)
#     points(end.ind, col = rgb(1,0,0,0.25), pch = 19, cex = 0.75, lwd = 0)
# }