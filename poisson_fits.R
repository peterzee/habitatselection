fit.local <- lm(moore.out$big.table[,"n.eggs",1] ~ moore.out$big.table[,"risk.score",1])
summary(fit.local)


plot( jitter(moore.out$big.table[,"risk.score",1]), 
      jitter(moore.out$big.table[,"n.eggs",1]))
      
abline(summary(fit.local))


hist(moore.out$big.table[,"n.eggs",1])


glm.fit <- glm(moore.out$big.table[,"n.eggs",1] ~ moore.out$big.table[,"risk.score",1], 
               family = poisson(link = 'log'))
summary(glm.fit)

pois.pred <- predict(glm.fit, type="response")
lines(moore.out$big.table[,"risk.score",1], pois.pred, col="blue", lwd=2)



zfit <- zeroinfl(moore.out$big.table[,"n.eggs",1] ~ moore.out$big.table[,"risk.score",1])
summary(zfit)

vuong(glm.fit, zfit)