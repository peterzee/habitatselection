## module neighborhoods
# quilted
# A
# module.extract
# module.dim
# 
# egg.landscape
# 
# 
# tapply(c(egg.landscape), c(A), mean)
# 
# tapply(egg.landscape[module.extract[,,6]], A[module.extract[,,6]], mean)



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

plot(module.table[,"n.risky"], module.table[,"mean.egg.safe"],
     xlab = 'number of risky patches in module', ylab = 'mean eggs / safe patch in module')


## adjacnt modules (basically a moore neighborhood for modules)
module.moore <-array(dim = c(nrow(mod.index), ncol(mod.index), length(mod.index)))
for (i in 1:length(mod.index)){
  
  ROW <- which(mod.index == i, arr.ind = TRUE)[1]
  COL <- which(mod.index == i, arr.ind = TRUE)[2]
  
  range <- 1
  
  row.range <- (ROW - range):(ROW + range)
    row.range[row.range <= 0] <- row.range[row.range <= 0] - 1
    row.range[row.range > (nrow(mod.index))] <- row.range[row.range > (nrow(mod.index))] + 1
  
  
  col.range <- (COL - range):(COL + range)
    col.range[col.range <= 0] <- col.range[col.range <= 0] - 1
    col.range[col.range > (ncol(mod.index))] <- col.range[col.range > (ncol(mod.index))] + 1
  
  v.range <- row.range %% (nrow(mod.index)+1)
  h.range <- col.range %% (ncol(mod.index)+1)
  
  module.moore[,,i] <- mod.index[v.range, h.range]
  module.moore[range + 1, range + 1,i] <- NA  
}


mean.module.risk <- rep(NA, nrow(module.table))
for (i in 1:nrow(module.table)){
  mean.module.risk[i]<- mean(module.table[module.moore[,,i], 'module.risk'], na.rm = TRUE)
}

plot(mean.module.risk, module.table[,"mean.egg.safe"],
     xlab = 'mean risk of surrounding modules', ylab = 'eggs / safe patch in focal module')


## Module Tracking (location)
## module id through time
module.tracker <- array(dim = c(time, pop.size))    
## changes in module id (within / among)
change.module <- rep(NA, pop.size) 
prop.habitable <- array(dim = c(pop.size ,4))
for (j in 1:pop.size){
  for (i in 1:time){
    module.tracker[i,j] <- which(module.extract == index[locations[i,1,j], locations[i,2,j]], arr.ind = TRUE)[3]
    }
    change.module[j] <- sum(!is.na(rle(module.tracker[,j])$values))-1
    prop.habitable[j,] <- c( sum(encounter[,j] > 0) / sum(!is.na(module.tracker[,j])) ,
                             sum(encounter[,j] == 0) / sum(!is.na(module.tracker[,j])),
                             sum(encounter[,j] == 1) / sum(!is.na(module.tracker[,j])),
                             sum(encounter[,j] == 2) / sum(!is.na(module.tracker[,j])))
}

plot(prop.habitable[,1], change.module)
plot(prop.habitable[,4], change.module)
