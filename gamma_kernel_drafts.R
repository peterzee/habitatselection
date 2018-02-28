x <- seq(0,20, by=0.1)


plot(x, dgamma(x, 4, 1), type = 'l')



boxplot(rgamma(500, 1, 1))

param <- c(1,8,15)
tab <- array(dim=c(length(param), 500))
for (i in 1:length(param)){
  tab[i,] <- rgamma(500, param[i], 1)  
}


boxplot(t(tab))

