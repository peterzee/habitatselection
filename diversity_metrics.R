###  diversity_metrics
library(vegan)

A <- a$module.landscape
index <- array(1:length(A), dim(A))

egg.landscape <- comm.out$egg.landscape

aDiv <- array(NA, dim = c(nrow(A), ncol(A)))
shannon.div <- array(NA, dim = c(nrow(A), ncol(A)))
for (i in 1:length(index)){
  
  id <- which(index == i, arr.ind =TRUE)  
  loc.comm <- egg.landscape[id[1], id[2],]
  aDiv[i] <- sum(loc.comm > 0)
  shannon.div[i] <- diversity(loc.comm, index = 'shannon')
  
  }

plot(jitter(aDiv, amount = 0.05), shannon.div, xlab = "Richness", ylab = "Shannon index", cex = 1, lwd = 0.5, xlim= c(0,10),ylim=c(0,3))
abline(0,1, lty=2)


### with richness
mean.alpha <- tapply(c(aDiv), c(A), mean)
gamma <- sum(apply(egg.landscape, 3, function(x) sum(x) > 0))

beta <- gamma / mean.alpha

mean.alpha
beta
gamma


total.mean.alpha <- mean(tapply(c(aDiv[A > 0]), which(c(A > 0)), mean))
total.beta <- gamma / total.mean.alpha

richness.div.metrics <-c(gamma, 
                         total.mean.alpha,
                         total.beta,
                         mean.alpha,
                         beta)

barplot(richness.div.metrics, border =FALSE,names.arg = c('gamma', 
                                            'total alpha', 
                                            'total beta', 
                                            'safe alpha', 
                                            'risk alpha',
                                            'safe beta', 
                                            'risk beta'), las = 2, ylab = "Diversity metric")



### with shannon index
shannon.mean.alpha <- tapply(c(shannon.div), c(A), mean)
shannon.gamma <- diversity(egg.landscape, index = 'shannon')

shannon.beta <- shannon.gamma / shannon.mean.alpha

shannon.mean.alpha
shannon.beta
shannon.gamma


total.shannon.mean.alpha <- mean(tapply(c(shannon.div[A > 0]), which(c(A > 0)), mean))
total.shannon.beta <- shannon.gamma / total.shannon.mean.alpha

shannon.richness.div.metrics <-c(shannon.gamma, 
                         total.shannon.mean.alpha,
                         total.shannon.beta,
                         shannon.mean.alpha,
                         shannon.beta)

barplot(shannon.richness.div.metrics)
