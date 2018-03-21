source('header.R')
source('params.R')

rep_traces <- function(x){
  y <- params_population$POP.SIZE
  ind <- sample(y, x)
    for (i in seq_along(ind)){
      traceMovement(ind[i], trace.data = sim$locations[,,ind[i]],landscape, overlay = TRUE)
    }

}