## Run everything together (habitat selection)

# source('install_packages.R')
source('header.R')

## Parameter values
source('parameters.R')

## 1/ Generate landscape(s)
landscape <- do.call(generateWavelet_landscape, 
                     params_landscape)

## 2/ Run simulations
params_population$LANDSCAPE <- landscape
sim <- do.call(pop.habitatselection, 
               params_population)

## 3/ Moore neighborhoods
params_moore$LANDSCAPE <- landscape
params_moore$EGG.LANDSCAPE <- sim$egg.landscape
moore <- do.call(moore.summary, 
                 params_moore)


