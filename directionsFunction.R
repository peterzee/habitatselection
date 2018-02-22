### run functions
source('generateModuleLandscape_function.R')
source('plotLandscape_function.R')
source('shuffleLandscape.R')
source('population_function.R')
source('moore_function.R')
source('module_neighborhood_function.R')
source('plotEggs.R')


##############################################################################
##############################################################################
structure.vec <- c(FALSE, TRUE)
shuffle.vec <- c(FALSE, TRUE)
risk.vec <- c(0.0, 0.8)
perception.vec <- c(0.1, 0.9)
mvt.vec <- c(0.2, 0.9)
mvt.mod.vec <- c(0.19, 0.7)

replicates <- 25

directions.table <- array(dim = c((2^6) * replicates, 12))
names(directions.table) <- c('id', 
                             'replicate',
                             'structure.vec', 
                             'shuffle.vec', 
                             'risk.vec', 
                             'perception.vec', 
                             'mvt.vec', 
                             'mvt.mod.vec',
                             'local.coef',
                             'local.p.value',
                             'regional.coef',
                             'regional.p.value')

count <- 0
##############################################################################
for (ay in 1:length(structure.vec) ) {
  for (be in 1:length(shuffle.vec) ) {
    
    
    ## Generate the landscape
    a <- generateModuleLandscape(MATRIX.SIZE = 1, 
                                 PATCH.DIM = 4, 
                                 MODULE.DIM = 6, 
                                 STRUCTURE = structure.vec[ ay ],
                                 SHUFFLE = shuffle.vec[ be ])
    
    
    for (ce in 1:length(risk.vec) ) {
      for (de in 1:length(perception.vec) ) {
        for (ee in 1:length(mvt.vec) ) {
          for (ef in 1:length(mvt.mod.vec) ) {

            for (ge in 1:replicates){

count <- count + 1            
directions.table[count, 1:8] <- c(count, 
                                  ge,
                                  structure.vec[ ay ] * 1, 
                                  shuffle.vec[ be ] * 1, 
                                  risk.vec[ ce ], 
                                  perception.vec[ de ], 
                                  mvt.vec[ ee ], 
                                  mvt.mod.vec[ ef ]) 


## Simulation the population
sim <- pop.habitatselection(POP.SIZE = 250,
                            LANDSCAPE = a$module.landscape,
                            RISK.MAG = risk.vec[ ce ],
                            PERCEPTION = perception.vec[ de ],
                            MVT = mvt.vec[ ee ],
                            MVT.MOD = mvt.mod.vec[ ef ])

## Moore neighborhoods
# moore.out <- moore.summary(LANDSCAPE = a$module.landscape,
#                            EGG.LANDSCAPE = sim$egg.landscape,
#                            MOORE.RANGE = 3)

## Module neighborhoods
module.out <- module.neighborhood(LANDSCAPE = a$module.landscape,
                                  EGG.LANDSCAPE = sim$egg.landscape, 
                                  MODULE.DIM = a$inputs$MODULE.DIM,
                                  MOD.EXTRACT = a$module.extract, 
                                  MOD.INDEX = a$mod.index)




fit.local <- lm(module.out$module.table[,"mean.egg.safe"] ~ module.out$module.table[,"n.risky"])
summary(fit.local)

  directions.table[count, 9:10] <- c(summary(fit.local)$coef[2,1], summary(fit.local)$coef[2,4])

  
if (perception.vec[ de ] + risk.vec[ ce ] < 1) {
  fit.regional <- lm(module.out$module.table[,"mean.egg.safe"] ~ module.out$mean.module.risk)
  summary(fit.regional)

    directions.table[count, 11:12] <- c(summary(fit.regional)$coef[2,1], summary(fit.regional)$coef[2,4])
  }


print(count)
            }
          }
        }
      }
    }
  }
}



directions.table[which(directions.table[,9] < 0), 1:8]
directions.table[which(directions.table[,10] < 0.05), 1:8]

directions.table[which(directions.table[,11] < 0.05), 1:8]
directions.table[which(directions.table[,12] < 0.05), 1:8]


x <- array(dim = c(10,12,nrow(directions.table) / replicates))
count <- 0
for (i in seq(1, nrow(directions.table), by = dim(x)[1])){
  count <- count + 1
  x[,,count] <- directions.table[i:(i + (dim(x)[1] - 1)),]
}


######
par(mfrow=c(1,2))
local.estimates <- x[,9,]
local.pvalues <- x[,10,]
which(local.pvalues <= 0.05, arr.ind = TRUE)
barplot(table(which(local.pvalues <= 0.05, arr.ind = TRUE)[,2]) / dim(x)[1],
        main = 'number significant/local/')


local.estimates[which(local.pvalues > 0.05)] <- NA
barplot(colMeans(local.estimates, na.rm = TRUE),
        main = 'mean of sig estimates/local')
boxplot(local.estimates, col = 'thistle',
        main = 'mean of sig estimates/local')
abline(h = 0, lty = 2, lwd = 0.5)

regional.estimates <- x[,11,]
regional.pvalues <- x[,12,]
which(regional.pvalues <= 0.05, arr.ind = TRUE)
barplot(table(which(regional.pvalues <= 0.05, arr.ind = TRUE)[,2]) / dim(x)[1],
        main = 'number significant/regional/')

regional.estimates[which(regional.pvalues > 0.05)] <- NA
barplot(colMeans(regional.estimates, na.rm = TRUE),
        main = 'mean of sig estimates/regional')
boxplot(regional.estimates, col = 'thistle',
        main = 'mean of sig estimates/regional')
abline(h = 0, lty = 2, lwd = 0.5)



# par(mfrow=c(1,2))
hist(local.estimates[which(local.pvalues <= 0.05)])
hist(regional.estimates[which(regional.pvalues <= 0.05)])
