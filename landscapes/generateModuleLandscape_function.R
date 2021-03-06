# generateModuleLandscape
# source('plotLandscape_function.R')
source('landscapes/shuffleLandscape.R')


generateModuleLandscape <- function(MATRIX.SIZE, PATCH.DIM, MODULE.DIM, STRUCTURE = TRUE, SHUFFLE = FALSE){
  
  
  is.structured <- STRUCTURE
  is.shuffled <- SHUFFLE
  
  
  hab.matrix <- MATRIX.SIZE
  patch.dim <- PATCH.DIM
  module.dim <- MODULE.DIM     ## must be even number
  
  
  mod.index <- array(1:module.dim^2, dim = c(module.dim, module.dim))
  tmp.ind <- which(mod.index > 0, arr.ind = TRUE)
  
  translateIndex <- function(x){
    ((x-1) * (hab.matrix + patch.dim)) + 1
  }
  
  quilted.index <- translateIndex(tmp.ind)
  quilted <- array(dim = c(module.dim * (hab.matrix + patch.dim), module.dim * (hab.matrix + patch.dim)))
  total.index <- array(1:length(quilted), dim = c(module.dim * (hab.matrix + patch.dim), module.dim * (hab.matrix + patch.dim)))
  module.extract <- array(dim = c((hab.matrix + patch.dim), (hab.matrix + patch.dim), length(mod.index)))
  
  count <- 0
  
  risk.vector <- c(seq(0, 2, length.out = (module.dim/2) - 1), NA, NA,
                   rev(seq(0, 2, length.out = (module.dim/2) - 1)))
  
  for (i in 1:length(mod.index)){
    
    count <- count + 1
    
    hab.module <- array(0, dim = c(hab.matrix + patch.dim, hab.matrix + patch.dim))
    
    ##########################################
    
    ### heterogeneity among modules
    module.col <- (which(unique(quilted.index[,2]) == quilted.index[i,2]))
    if (is.structured == TRUE){
      if (module.col == (module.dim/2) || module.col == ((module.dim/2) + 1)){
        
        x <- array(sample(0:2, patch.dim^2, prob = c(0, 0, 1), replace = TRUE), dim = c(patch.dim, patch.dim))
        
      } else {
        
        x <- array(sample(0:2, patch.dim^2, prob = c(0, 1, risk.vector[module.col]), replace = TRUE), dim = c(patch.dim, patch.dim))
      }
    } else {
      x <- array(sample(0:2, patch.dim^2, prob = c(0,1,1), replace = TRUE), dim = c(patch.dim, patch.dim))
    }
    
    ##########################################
    
    
    hab.module[((hab.matrix / 2) + 1) : (((hab.matrix / 2)) + patch.dim), ((hab.matrix / 2) + 1) : (((hab.matrix / 2)) + patch.dim) ] <- x
    
    A <- hab.module
    
    
    
    quilted[quilted.index[i,1] : (quilted.index[i,1] + (hab.matrix + patch.dim - 1)),
            quilted.index[i,2] : (quilted.index[i,2] + (hab.matrix + patch.dim - 1))] <- A
    
    module.extract[,,i] <- total.index[quilted.index[i,1] : (quilted.index[i,1] + (hab.matrix + patch.dim - 1)),
                                       quilted.index[i,2] : (quilted.index[i,2] + (hab.matrix + patch.dim - 1))]
  }
  
  if (is.shuffled == TRUE){
    shuffleLandscape <- function(module.landscape){
      
      quilted <- module.landscape  
      
      shuffle.quilt <- array(dim = c(nrow(quilted), ncol(quilted)))
      shuffle.sample <- sample(mod.index, max(mod.index))
      for (i in 1:max(mod.index)){
        
        shuffle.quilt[module.extract[,,i]] <- quilted[module.extract[,,shuffle.sample[i]]] 
        
      }
      
      
      shuffled <- shuffle.quilt      
      return(shuffled)
    }
    quilted <- shuffleLandscape(quilted)
    
  }
  
  inputs <- list(MATRIX.SIZE = MATRIX.SIZE, 
                 PATCH.DIM = PATCH.DIM, 
                 MODULE.DIM = MODULE.DIM)
  
  out <- list(inputs = inputs,
              module.landscape = quilted,
              module.extract = module.extract,
              mod.index = mod.index)
  
return(out)
}

# 
# a <- generateModuleLandscape(MATRIX.SIZE = 2, PATCH.DIM = 4, MODULE.DIM = 6, STRUCTURE = TRUE, SHUFFLE = TRUE)
# a$module.landscape
# plotLandscape(a$module.landscape)
