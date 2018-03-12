### run functions
source('generateModuleLandscape_function.R')
source('plotLandscape_function.R')
source('shuffleLandscape.R')
source('population_function.R')
source('community_function.R')
source('moore_function.R')
source('module_neighborhood_function.R')
source('plotEggs.R')
source('eggHatching.R')

## Generate the landscape
a <- generateModuleLandscape(MATRIX.SIZE = 0, 
                             PATCH.DIM = 4, 
                             MODULE.DIM = 10, 
                             STRUCTURE = TRUE,
                             SHUFFLE = TRUE)
# 
# ## Simulation the population
sim <- pop.habitatselection(POP.SIZE = 250,
                            LANDSCAPE = blah,#a$module.landscape,
                            RISK.MAG = 0.0,
                            PERCEPTION = 0.2,
                            MVT = 1,
                            MVT.MOD = 0,
                            MEM.DEPTH = 5,
                            MEM.WEIGHT = 0.1)

## Community simulations
# comm.out <- community.habselection(NUM.SPP = 10,
#                                    POP.SIZE = 250,
#                                    LANDSCAPE = a$module.landscape,
#                                    RISK.MAG = 0.0,
#                                    PERCEPTION = 0.1,
#                                    MVT = 1,
#                                    MVT.MOD = -4)

# ## Moore neighborhoods
# moore.out <- moore.summary(LANDSCAPE = a$module.landscape,
#                            EGG.LANDSCAPE = sim$egg.landscape,
#                            MOORE.RANGE = 3)
# 
# Module neighborhoods
module.out <- module.neighborhood(LANDSCAPE = a$module.landscape,
                                  EGG.LANDSCAPE = sim$egg.landscape,
                                  MODULE.DIM = a$inputs$MODULE.DIM,
                                  MOD.EXTRACT = a$module.extract,
                                  MOD.INDEX = a$mod.index)

#

####################################################################################
par(mfrow = c(1,1))
plotLandscape(a$module.landscape)
plotEggs(EGG.LANDSCAPE = sim$egg.landscape, LANDSCAPE = a$module.landscape)

hatched.landscape <- egg.hatched(sim$egg.landscape, FECUNDITY = 1, P.EMERGE = 1, PENALTY = 0.5)
sum(hatched.landscape)
prop.hatched <- sum(hatched.landscape) / sim$inputs$POP.SIZE

par(mfrow = c(1,1), mar = c(3,4,1,1))
boxplot(sim$egg.landscape[a$module.landscape == 1],
        sim$egg.landscape[a$module.landscape == 2],
        names = c('safe', 'risky'),
        ylab = 'eggs per patch',
        col = 'thistle')


## Plots for local and regional context dependencies

## Local (mean number of eggs in safe patches as function of patches within module)
par(mfrow = c(2,1), mar = c(4,4,1,1))
plot(module.out$module.table[,"n.risky"], module.out$module.table[,"mean.egg.safe"],
     xlab = 'Number of risky patches in module', ylab = 'Mean eggs / safe patch in module')
  
  fit.local <- lm(module.out$module.table[,"mean.egg.safe"] ~ module.out$module.table[,"n.risky"])
  summary(fit.local)
  if (summary(fit.local)$coefficients[2,4] < 0.05) { abline(fit.local) }

## Regional (mean number of eggs in safe patches as function of surrounding modules)  
plot(module.out$mean.module.risk, module.out$module.table[,"mean.egg.safe"],
     xlab = 'mean risk of surrounding modules', ylab = 'eggs / safe patch in focal module')

  fit.regional <- lm(module.out$module.table[,"mean.egg.safe"] ~ module.out$mean.module.risk)
  summary(fit.regional)
  if (summary(fit.regional)$coefficients[2,4] < 0.05) { abline(fit.regional) }

