# meta.table

par(mfrow=c(7,5), mar = c(0,0,0,0))

for (EMM in 1:3){
for (eye in 1:length(unique(meta.table[,"landscape.ratio"]))){

  for (jay in 1:length(unique(meta.table[,"risk"]))){
  
    plot(1,1, cex = 0, xlim= c(0,10), ylim=c(0,20), ann = FALSE, axes = FALSE)
    box()

      for (kay in 1:length(unique(meta.table[,"movement"]))){

      sim.subset <- which(meta.table[,'landscape.ratio'] == unique(meta.table[,"landscape.ratio"])[eye] & 
                          meta.table[,"risk"] == unique(meta.table[,"risk"])[jay] &
                          meta.table[,"movement"] == unique(meta.table[,"movement"])[kay]) 

      L <- length(sim.subset)
      means.table <- array(dim=c(L, 20))

      for (i in 1:L){
          
          SIM <- sim.subset[i]
      
          a <- OUTPUT.LIST[[SIM]]$moore.out$neighborhood.totals[,"total.eggs.safe", EMM]
          b <- OUTPUT.LIST[[SIM]]$moore.out$neighborhood.totals[,"n.safe.patches", EMM]
          c <- OUTPUT.LIST[[SIM]]$moore.out$neighborhood.totals[,"n.risk.patches", EMM]
          
          means <- tapply(a/b, c, mean)
          means.table[i,1:length(means)] <- means
      }

      points(colMeans(means.table, na.rm = TRUE), type = 'b', pch = 19, col = kay)
      abline(h=3, lty=3, lwd=0.5)
      
    }
  }
}
}
# plot(colMeans(means.table), type = 'b', ylim = c(0,10))


# plot(means.table[,1])

# plot(c, (a/b))
# points(as.numeric(names(means)), means, col=2, cex=2,pch=19)