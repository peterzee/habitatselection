## module neighborhood function

module.neighborhood <- function(LANDSCAPE, EGG.LANDSCAPE, MODULE.DIM, MOD.EXTRACT, MOD.INDEX){
  
  A <- LANDSCAPE
  egg.landscape <- EGG.LANDSCAPE
  
  module.dim <- MODULE.DIM
  module.extract <- MOD.EXTRACT
  mod.index <- MOD.INDEX
  
  ## Module neighborhoods
  ## module summary
  module.table <- array(dim = c(module.dim^2, 9))
  colnames(module.table) <- c('id', 'n.empty', 'n.safe', 'n.risky', 'n.egg.safe', 'n.egg.risky', 'mean.egg.safe', 'mean.egg.risky', 'module.risk')
  for (i in 1:nrow(module.table)){
    
    module.table[i,1] <- i 
    
    module.table[i,2:4] <- c(sum(A[module.extract[,,i]] == 0),
                             sum(A[module.extract[,,i]] == 1), 
                             sum(A[module.extract[,,i]] == 2))
    module.table[i,5:6] <- c(sum(egg.landscape[module.extract[,,i]][which(A[module.extract[,,i]] == 1)]),
                             sum(egg.landscape[module.extract[,,i]][which(A[module.extract[,,i]] == 2)]))
    module.table[i,7:8] <- c(sum(egg.landscape[module.extract[,,i]][which(A[module.extract[,,i]] == 1)]) / sum(A[module.extract[,,i]] == 1),
                             sum(egg.landscape[module.extract[,,i]][which(A[module.extract[,,i]] == 2)]) / sum(A[module.extract[,,i]] == 2))
    module.table[i,9] <- module.table[i,6] / sum(module.table[i,2:4])
  }
  
  
  ## adjacnt modules (basically a moore neighborhood for modules)
  range <- 1
  module.moore <-array(dim = c((2*range + 1), (2*range + 1), length(mod.index)))
  for (i in 1:length(mod.index)){
    
    ROW <- which(mod.index == i, arr.ind = TRUE)[1]
    COL <- which(mod.index == i, arr.ind = TRUE)[2]
    
    row.range <- (ROW - range):(ROW + range)
    row.range[row.range <= 0] <- row.range[row.range <= 0] - 1
    row.range[row.range > (nrow(mod.index))] <- row.range[row.range > (nrow(mod.index))] + 1
    
    
    col.range <- (COL - range):(COL + range)
    col.range[col.range <= 0] <- col.range[col.range <= 0] - 1
    col.range[col.range > (ncol(mod.index))] <- col.range[col.range > (ncol(mod.index))] + 1
    
    v.range <- row.range %% (nrow(mod.index) + 1)
    h.range <- col.range %% (ncol(mod.index) + 1)
    
    module.moore[,,i] <- mod.index[v.range, h.range]
    module.moore[range + 1, range + 1,i] <- NA  
  }
  
  mean.module.risk <- rep(NA, nrow(module.table))
  for (i in 1:nrow(module.table)){
    mean.module.risk[i]<- mean(module.table[module.moore[,,i], 'module.risk'], na.rm = TRUE)
  }
  

  out <- list(module.table = module.table,
              mean.module.risk = mean.module.risk)
  
  return(out)
}


#

blah <- module.neighborhood(sim$A, sim$egg.landscape, a$inputs$MODULE.DIM, MOD.EXTRACT = a$module.extract, MOD.INDEX = a$mod.index)
