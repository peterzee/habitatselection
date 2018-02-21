# shuffle module landscape
source('plotLandscape_function.R')

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


# a <- shuffleLandscape(quilted)
# 
# plotLandscape(a)
