# meta.table


  for (eye in 1:length(unique(meta.table[,"landscape.ratio"]))){
    
    for (kay in 1:length(unique(meta.table[,"movement"]))){
      
      plot(1,1, cex = 0, xlim= c(0,10), ylim=c(0, 30), ann = FALSE, axes = FALSE)
      box()
      
      for (jay in 1:length(unique(meta.table[,"risk"]))){
        
        sim.subset <- which(meta.table[,'landscape.ratio'] == unique(meta.table[,"landscape.ratio"])[eye] & 
                              meta.table[,"risk"] == unique(meta.table[,"risk"])[jay] &
                              meta.table[,"movement"] == unique(meta.table[,"movement"])[kay]) 
        
        L <- length(sim.subset)
        # means.table <- array(dim=c(L, ((2*3+1)^2)))
        
        for (i in 1:L){
          
          SIM <- sim.subset[i]
          
          a <- OUTPUT.LIST[[SIM]]$module.out$module.table[,"n.risky"]
          b <- OUTPUT.LIST[[SIM]]$module.out$module.table[,"mean.eggs.safe"]
          c <- OUTPUT.LIST[[SIM]]$module.out$module.table[,"module.risk"]
          
          means <- tapply(a/b, c, mean)
          means.table[i,1:length(means)] <- means
        }
        
        points(colMeans(means.table, na.rm = TRUE), type = 'l', col = jay)
        abline(h = 3, lty = 3, lwd = 0.5)
        legend('topright', legend = c(unique(meta.table[,"landscape.ratio"])[eye],
                                      unique(meta.table[,"movement"])[kay]))
        
      
    }
  }
}
