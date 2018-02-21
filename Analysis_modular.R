# meta.table

byScore <- TRUE

  for (eye in 1:length(unique(meta.table[,"landscape.structure"]))){
    
    for (kay in 1:length(unique(meta.table[,"movement"]))){
      
      plot(1,1, cex = 0, xlim= c(0,10), ylim=c(0, 3), ann = FALSE, axes = FALSE)
      box()
      
      for (jay in 1:length(unique(meta.table[,"risk"]))){
        
        sim.subset <- which(meta.table[,'landscape.structure'] == unique(meta.table[,"landscape.structure"])[eye] & 
                              meta.table[,"risk"] == unique(meta.table[,"risk"])[jay] &
                              meta.table[,"movement"] == unique(meta.table[,"movement"])[kay]) 
        
        L <- length(sim.subset)
        mean.number.table <- array(dim=c(L, 20))
        # mean.score.list <- matrix(list(), L, 2)
        
        for (i in 1:L){
          
          SIM <- sim.subset[i]
          
          a <- OUTPUT.LIST[[SIM]]$module.out$module.table[,"n.risky"]
          b <- OUTPUT.LIST[[SIM]]$module.out$module.table[,"mean.egg.safe"]
          
          c <- OUTPUT.LIST[[SIM]]$module.out$module.table[,"module.risk"]
          
          mean.by.number <- tapply(b, a, mean, na.rm = TRUE)
          mean.by.score <- tapply(b, c, mean, na.rm = TRUE)
          
          mean.number.table[i, as.numeric(names(mean.by.number))+1] <- mean.by.number
          mean.score.list[[i,1]] <- as.numeric(names(mean.by.score))
          mean.score.list[[i,2]] <- mean.by.score
          
        }
        
        # if (byScore == TRUE){
        #   points(colMeans(mean.number.table, na.rm = TRUE), type = 'l', col = jay)
        #   abline(h = 3, lty = 3, lwd = 0.5)
        #   legend('topright', legend = c(unique(meta.table[,"landscape.ratio"])[eye],
        #                                 unique(meta.table[,"movement"])[kay]))
        # } else {
        
          points(colMeans(mean.number.table, na.rm = TRUE), type = 'l', col = jay)
          abline(h = 3, lty = 3, lwd = 0.5)
          legend('topright', legend = c(unique(meta.table[,"landscape.structure"])[eye],
                                        unique(meta.table[,"movement"])[kay]))
          
        # }
      
    }
  }
}
