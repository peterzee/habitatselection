source('plotLandscape_function.R')

generateModular <- function(MOD.DIM, MATRIX.SIZE, PATCH.DIM){

  module.dim <- MOD.DIM
  hab.matrix <- MATRIX.SIZE
  patch.dim <- PATCH.DIM
  
  mod.index <- array(1:module.dim^2, dim = c(module.dim, module.dim))
  tmp.ind <- which(mod.index > 0, arr.ind = TRUE)
  
  translateIndex <- function(x){
    ((x-1) * (hab.matrix + patch.dim)) + 1
  }
  
  quilted.index <- translateIndex(tmp.ind)
  quilted <- array(dim = c(module.dim * (hab.matrix + patch.dim), module.dim * (hab.matrix + patch.dim)))
  
  count <- 0
  
  for (i in 1:length(mod.index)){
  
    count <- count + 1
  
    hab.module <- array(0, dim = c(hab.matrix + patch.dim, hab.matrix + patch.dim))
  
    # x <- array(sample(0:2, patch.dim^2, prob = c(1,1,1), replace = TRUE), dim = c(patch.dim, patch.dim))
    x <- array(sample(0:2, patch.dim^2, prob = c(0, 1, runif(1, 0, 5)), replace = TRUE), dim = c(patch.dim, patch.dim))
    
    hab.module[((hab.matrix / 2) + 1) : (((hab.matrix / 2)) + patch.dim), ((hab.matrix / 2) + 1) : (((hab.matrix / 2)) + patch.dim) ] <- x
    
    A <- hab.module
    
    quilted[quilted.index[i,1] : (quilted.index[i,1] + (hab.matrix + patch.dim - 1)),
            quilted.index[i,2] : (quilted.index[i,2] + (hab.matrix + patch.dim - 1))] <- A
  
  }

return(quilted)  
}

a <- generateModular(5, 3, 5)
plotLandscape(a)



