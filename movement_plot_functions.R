## Movement plots

traceMovement <- function(individual, trace.data, landscape, overlay = TRUE){
    
  IND <- individual
  loc <- trace.data
  A <- landscape
  
  if (overlay == TRUE){
  
    points(1,1, cex = 0)
    for (i in 2:time){
      points(loc[i,1], loc[i,2], pch = 19, col = rgb(0,0,0,0.5), cex = 0.5)
      
      ## stays 'inbounds'
      if (wrap.info[i,1,IND] == 0){
        
        arrows(x0 = loc[i-1,1], y0 = loc[i-1,2],
               x1 = loc[i,1], y1 = loc[i,2],
               length = 0)
        
      } else {
        
        ## wraps around ('out of bounds')
        category.sort <- c(
          wrap.info[i,2,IND] < 1,
          wrap.info[i,2,IND] > ncol(A),
          wrap.info[i,3,IND] < 1,
          wrap.info[i,3,IND] > nrow(A)
        )
        x <- category.sort
        if ( sum(x == c(1,0,0,0)) == 4 ){ category <- 'lr' }
        if ( sum(x == c(0,1,0,0)) == 4 ){ category <- 'rl' }
        if ( sum(x == c(0,0,1,0)) == 4 ){ category <- 'bt' }
        if ( sum(x == c(0,0,0,1)) == 4 ){ category <- 'tb' }
        if ( sum(x == c(1,0,1,0)) == 4 ){ category <- 'lrbt' }
        if ( sum(x == c(1,0,0,1)) == 4 ){ category <- 'lrtb' }
        if ( sum(x == c(0,1,1,0)) == 4 ){ category <- 'rlbt' }
        if ( sum(x == c(0,1,0,1)) == 4 ){ category <- 'rltb' }
        
        ## LEFT TO RIGHT
        if (category == 'lr') {
          #outgoing arrow
          arrows(x0 = loc[i-1,1], y0 = loc[i-1,2],
                 x1 = 0, y1 = mean(loc[i-1,2], loc[i,2]),
                 length = 0)
          
          #incoming arrow
          arrows(x0 = ncol(A) + 1, y0 = mean(loc[i-1,2], loc[i,2]),
                 x1 = loc[i,1], y1 = loc[i,2],
                 length = 0)
        }
        ## RIGHT TO LEFT
        if (category == 'rl') {
          #outgoing arrow
          arrows(x0 = loc[i-1,1], y0 = loc[i-1,2],
                 x1 = ncol(A) + 1, y1 = mean(loc[i-1,2], loc[i,2]),
                 length = 0)
          
          #incoming arrow
          arrows(x0 = 0, y0 = mean(loc[i-1,2], loc[i,2]),
                 x1 = loc[i,1], y1 = loc[i,2],
                 length = 0)
        }
        ## BOTTOM TO TOP
        if (category == 'bt') {
          #outgoing arrow
          arrows(x0 = loc[i-1,1], y0 = loc[i-1,2],
                 x1 = mean(loc[i-1,1], loc[i,1]), y1 = 0,
                 length = 0)
          
          #incoming arrow
          arrows(x0 = mean(loc[i-1,1], loc[i,1]), y0 = nrow(A) + 1,
                 x1 = loc[i,1], y1 = loc[i,2],
                 length = 0)
        }
        ## BOTTOM TO TOP
        if (category == 'tb') {
          #outgoing arrow
          arrows(x0 = loc[i-1,1], y0 = loc[i-1,2],
                 x1 = mean(loc[i-1,1], loc[i,1]), y1 = nrow(A) + 1,
                 length = 0)
          
          #incoming arrow
          arrows(x0 = mean(loc[i-1,1], loc[i,1]), y0 = 0,
                 x1 = loc[i,1], y1 = loc[i,2],
                 length = 0)
        }
        
        ## LEFT TO RIGHT, plus vertical wrap
        if (category == 'lrbt' || category == 'lrtb') {
          #outgoing arrow
          arrows(x0 = loc[i-1,1], y0 = loc[i-1,2],
                 x1 = loc[i-1,1], y1 = 0,
                 length = 0)
          
          #incoming arrow
          arrows(x0 = loc[i-1,1] + 1, y0 = nrow(A) + 1,
                 x1 = loc[i,1], y1 = loc[i,2],
                 length = 0)
        }
        ## RIGHT TO LEFT, plus vertical wrap
        if (category == 'rlbt' || category == 'rltb') {
          #outgoing arrow
          arrows(x0 = loc[i-1,1], y0 = loc[i-1,2],
                 x1 = ncol(A) + 1, y1 = loc[i-1,2],
                 length = 0)
          
          #incoming arrow
          arrows(x0 = loc[i-1,1], y0 = 0,
                 x1 = loc[i,1], y1 = loc[i,2],
                 length = 0)
        }
        
      }
      
      points(loc[1,1], loc[1,2], pch = 13, col ='forestgreen', cex = 2)
      points(loc[max(which(!is.na(loc[,1]))),1], loc[max(which(!is.na(loc[,1]))),2], pch = 13, col = 'red', cex = 2)
    }
    box()
    
    
    } else {
       
      plot(1,1, cex = 0, 
           xlim=c(1,ncol(A)), 
           ylim=c(1,nrow(A)), 
           ann=FALSE, axes=FALSE)
      for (i in 2:time){
        points(loc[i,1], loc[i,2], pch = 19, col =rgb(0,0,0,0.5), cex = 0.5)
        
        ## stays 'inbounds'
        if (wrap.info[i,1,IND] == 0){
          
          arrows(x0 = loc[i-1,1], y0 = loc[i-1,2],
                 x1 = loc[i,1], y1 = loc[i,2],
                 length = 0)
          
        } else {
          
          ## wraps around ('out of bounds')
          category.sort <- c(
            wrap.info[i,2,IND] < 1,
            wrap.info[i,2,IND] > ncol(A),
            wrap.info[i,3,IND] < 1,
            wrap.info[i,3,IND] > nrow(A)
          )
          x <- category.sort
          if ( sum(x == c(1,0,0,0)) == 4 ){ category <- 'lr' }
          if ( sum(x == c(0,1,0,0)) == 4 ){ category <- 'rl' }
          if ( sum(x == c(0,0,1,0)) == 4 ){ category <- 'bt' }
          if ( sum(x == c(0,0,0,1)) == 4 ){ category <- 'tb' }
          if ( sum(x == c(1,0,1,0)) == 4 ){ category <- 'lrbt' }
          if ( sum(x == c(1,0,0,1)) == 4 ){ category <- 'lrtb' }
          if ( sum(x == c(0,1,1,0)) == 4 ){ category <- 'rlbt' }
          if ( sum(x == c(0,1,0,1)) == 4 ){ category <- 'rltb' }
          
          ## LEFT TO RIGHT
          if (category == 'lr') {
            #outgoing arrow
            arrows(x0 = loc[i-1,1], y0 = loc[i-1,2],
                   x1 = 0, y1 = mean(loc[i-1,2], loc[i,2]),
                   length = 0)
            
            #incoming arrow
            arrows(x0 = ncol(A) + 1, y0 = mean(loc[i-1,2], loc[i,2]),
                   x1 = loc[i,1], y1 = loc[i,2],
                   length = 0)
          }
          ## RIGHT TO LEFT
          if (category == 'rl') {
            #outgoing arrow
            arrows(x0 = loc[i-1,1], y0 = loc[i-1,2],
                   x1 = ncol(A) + 1, y1 = mean(loc[i-1,2], loc[i,2]),
                   length = 0)
            
            #incoming arrow
            arrows(x0 = 0, y0 = mean(loc[i-1,2], loc[i,2]),
                   x1 = loc[i,1], y1 = loc[i,2],
                   length = 0)
          }
          ## BOTTOM TO TOP
          if (category == 'bt') {
            #outgoing arrow
            arrows(x0 = loc[i-1,1], y0 = loc[i-1,2],
                   x1 = mean(loc[i-1,1], loc[i,1]), y1 = 0,
                   length = 0)
            
            #incoming arrow
            arrows(x0 = mean(loc[i-1,1], loc[i,1]), y0 = nrow(A) + 1,
                   x1 = loc[i,1], y1 = loc[i,2],
                   length = 0)
          }
          ## BOTTOM TO TOP
          if (category == 'tb') {
            #outgoing arrow
            arrows(x0 = loc[i-1,1], y0 = loc[i-1,2],
                   x1 = mean(loc[i-1,1], loc[i,1]), y1 = nrow(A) + 1,
                   length = 0)
            
            #incoming arrow
            arrows(x0 = mean(loc[i-1,1], loc[i,1]), y0 = 0,
                   x1 = loc[i,1], y1 = loc[i,2],
                   length = 0)
          }
          ## LEFT TO RIGHT, plus vertical wrap
          if (category == 'lrbt' || category == 'lrtb') {
            #outgoing arrow
            arrows(x0 = loc[i-1,1], y0 = loc[i-1,2],
                   x1 = 0, y1 = loc[i-1,2],
                   length = 0)
            
            #incoming arrow
            arrows(x0 = ncol(A) + 1, y1 = loc[i-1,2],
                   x1 = loc[i,1], y1 = loc[i,2],
                   length = 0)
          }
          ## RIGHT TO LEFT, plus vertical wrap
          if (category == 'rlbt' || category == 'rltb') {
            #outgoing arrow
            arrows(x0 = loc[i-1,1], y0 = loc[i-1,2],
                   x1 = ncol(A) + 1, y1 = loc[i-1,2],
                   length = 0)
            
            #incoming arrow
            arrows(x0 = 0, y1 = loc[i-1,2],
                   x1 = loc[i,1], y1 = loc[i,2],
                   length = 0)
          }
          
        }
        
        points(loc[1,1], loc[1,2], pch = 13, col ='forestgreen', cex = 2)
        points(loc[max(which(!is.na(loc[,1]))),1], loc[max(which(!is.na(loc[,1]))),2], pch = 13, col = 'red', cex = 2)
      }
      box()
    
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