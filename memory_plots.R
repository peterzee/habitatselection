x <- c(0,0,1,1,0,1,0,1,1,0,1,1,0,1,0,0,0,1)

# plot(cumsum(x), type='S',ann=FALSE, axes=FALSE,lwd=2)

par(mfrow=c(4,1), mar = c(0,1,0,0))

for (i in c(0,6,12,15)){
  fff <- i
  # plot(c(rep(0,fff), cumsum(x[fff:length(x)])), type='S',ann=FALSE, axes=FALSE,lwd=2, ylim=c(0,max(cumsum(x))))
  plot(1,1,cex=0, xlim=c(0,20), ylim=c(0,max(cumsum(x))), ann = FALSE, axes = FALSE)
  
  polygon(c(1:length(cumsum(x)), length(cumsum(x)):1),
          c(c(rep(0,fff), cumsum(x[(fff+1):length(x)])), rep(0,length(cumsum(x)))),
          col = rgb(1,0,0,0.25))
  
  
}




