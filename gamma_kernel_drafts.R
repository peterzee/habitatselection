x <- seq(0,20, by=0.1)


plot(x, dgamma(x, 1, 1), type = 'l', lwd = 3, 
     xlab = "Distance", ylab = "Frequency", col = 'dodgerblue')
points(x, dgamma(x, 5, 1), type = 'l', lwd = 3, col = 'forestgreen' )
points(x, dgamma(x, 9, 1), type = 'l', lwd = 3, col = 'thistle')



boxplot(rgamma(500, 1, 1))

param <- c(1, 5, 9)
tab <- array(dim=c(length(param), 500))
for (i in 1:length(param)){
  tab[i,] <- rgamma(500, param[i], 1)  
}


boxplot(t(tab), names = param,
        xlab = "Shape parameter",
        ylab = "Distance",
        col = c('dodgerblue', 'forestgreen', 'thistle'),
        boxlty = 0,
        staplelty = 0,
        whiskylty = 1)

