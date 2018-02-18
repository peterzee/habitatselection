## screwing around with landscape structures

x <- array(c(0:2), dim = c(10,10))

y <- array(sample(0:2, 100, prob = c(1,2,1), replace = TRUE), 
           dim = c(10,10))


plotLandscape(y)





