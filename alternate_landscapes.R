## screwing around with landscape structures
source('plotLandscape_function.R')


x <- array(c(0:2), dim = c(10,10))
# table(x)

y <- array(sample(0:2, 100, prob = c(1,1,10), replace = TRUE), dim = c(10,10))

plotLandscape(y)




