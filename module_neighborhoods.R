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
module.table <- array(dim = c(module.dim^2, 8))
colnames(module.table) <- c('id', 'n.empty', 'n.safe', 'n.risky', 'n.egg.safe', 'n.egg.risky', 'mean.egg.safe', 'mean.egg.risky')
for (i in 1:nrow(module.table)){
  
  module.table[i,1] <- i 
  
  module.table[i,2:4] <- c(sum(A[module.extract[,,i]] == 0),
                           sum(A[module.extract[,,i]] == 1), 
                           sum(A[module.extract[,,i]] == 2))
  module.table[i,5:6] <- c(sum(egg.landscape[module.extract[,,i]][which(A[module.extract[,,i]] == 1)]),
                           sum(egg.landscape[module.extract[,,i]][which(A[module.extract[,,i]] == 2)]))
  module.table[i,7:8] <- c(sum(egg.landscape[module.extract[,,i]][which(A[module.extract[,,i]] == 1)]) / sum(A[module.extract[,,i]] == 1),
                           sum(egg.landscape[module.extract[,,i]][which(A[module.extract[,,i]] == 2)]) / sum(A[module.extract[,,i]] == 2))
  
}

plot(module.table[,"n.risky"], module.table[,"mean.egg.safe"])
