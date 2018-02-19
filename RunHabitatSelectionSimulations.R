## Running
source('population_function.R')
sourece('moore_function.R')


patch.dim <- 10
x <- array(sample(0:2, patch.dim^2, prob = c(0,1,5), replace = TRUE), dim = c(patch.dim, patch.dim))

sim.out <- pop.habitatselection(POP.SIZE = 100,
                            LANDSCAPE = x,
                            RISK.MAG = 0.8,
                            PERCEPTION = 0.1,
                            MVT = 0.9,
                            MVT.MOD = 0.1)

moore.out <- moore.summary(LANDSCAPE = sim.out$A,
                           EGG.LANDSCAPE = sim.out$egg.landscape,
                           MOORE.RANGE = 3)


str(moore.out)
