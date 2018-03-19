# Generate Landscape
generateLandscape <- function(SIZE, N.PATCHES, PROP.RISKY){

landscape.size <- SIZE^2
n.patch <- N.PATCHES
prop.risk.patch <- PROP.RISKY

if (N.PATCHES > landscape.size){
  stop("too many patches")
  }

## set empty landscape 
landscape <- array(0, dim = c(SIZE, SIZE))
## draw random patches
patches <- sample(landscape.size, 
                  n.patch, 
                  replace = FALSE)
## patches are coded by '1'
landscape[patches] <- 1
## indices of all patches
patch.ind <- which(landscape == 1, arr.ind = TRUE)
## number of risky patches
n.risk.patch <- round((prop.risk.patch/100) * n.patch)
## draw random patches to be risky
risk.patches <- sample(n.patch, n.risk.patch)
## risky patches coded as '2'
landscape[patches[risk.patches]] <- 2
## indeices of risky patches
risk.ind <- which(landscape == 2, arr.ind = TRUE)
## indicies of safe patches
safe.ind <- which(landscape == 1, arr.ind = TRUE)

inputs <- list(landscape.size <- SIZE,
               n.patch <- N.PATCHES,
               prop.risk.patch <- PROP.RISKY)

output <- list(inputs = inputs,
               landscape = landscape,
               patch.ind = patch.ind,
               risk.ind = risk.ind,
               safe.ind = safe.ind)

return(output)
}




#world <- generateLandscape(SIZE = 10, N.PATCHES = 50, PROP.RISKY = 50)
