## screwing around with landscape structures
source('plotLandscape_function.R')


x <- array(c(0:2), dim = c(10,10))

y <- array(sample(0:2, 100, prob = c(0.5,2,1), replace = TRUE), dim = c(10,10))

table(x)
plotLandscape(y)





