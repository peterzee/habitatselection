# meta.table

par(mfrow=c(3,6), mar = c(0,0,0,0))

for (EMM in 1:3){
for (eye in 1:length(unique(meta.table[,"landscape.structure"]))){

      for (kay in 1:length(unique(meta.table[,"movement"]))){
  
    plot(1,1, cex = 0, xlim= c(0,10), ylim=c(0, 30), ann = FALSE, axes = FALSE)
    box()

  for (jay in 1:length(unique(meta.table[,"risk"]))){

      sim.subset <- which(meta.table[,'landscape.structure'] == unique(meta.table[,"landscape.structure"])[eye] & 
                          meta.table[,"risk"] == unique(meta.table[,"risk"])[jay] &
                          meta.table[,"movement"] == unique(meta.table[,"movement"])[kay]) 

      L <- length(sim.subset)
      means.table <- array(dim=c(L, ((2*3+1)^2)))

      for (i in 1:L){
          
          SIM <- sim.subset[i]
      
          a <- OUTPUT.LIST[[SIM]]$moore.out$neighborhood.totals[,"total.eggs.safe", EMM]
          b <- OUTPUT.LIST[[SIM]]$moore.out$neighborhood.totals[,"n.safe.patches", EMM]
          c <- OUTPUT.LIST[[SIM]]$moore.out$neighborhood.totals[,"n.risk.patches", EMM]
          
          means <- tapply(a/b, c, mean)
          means.table[i,1:length(means)] <- means
      }

      points(colMeans(means.table, na.rm = TRUE), type = 'l', col = jay)
      abline(h = 3, lty = 3, lwd = 0.5)
      legend('topright', legend = c(EMM, 
                                    unique(meta.table[,"landscape.structure"])[eye],
                                    unique(meta.table[,"movement"])[kay]))
      
    }
  }
}
}
