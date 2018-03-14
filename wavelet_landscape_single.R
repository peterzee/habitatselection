source('plotLandscape_function.R')
  
library(waveslim)
library(lattice)

#function to re-scale wavelet coefficients
waveSynth2 <- function(dim, varFun, ..., wavelet = "haar") {
  require(waveslim)
  varFun <- match.fun(varFun)
  dim <- rep(dim, length.out = 2)
  dim <- 2 ^ round(log2(dim))
  x <- matrix(rnorm(prod(dim)), dim[1], dim[2])
  nlev <- log2(min(dim))
  w <- dwt.2d(x, wavelet, nlev)
  freq <- 1 / 2 ^ rep(seq(1, nlev), each = 3)
  for (i in 1:length(freq)) w[[i]] <- w[[i]] * sqrt(varFun(freq[i], ...))
  idwt.2d(w)
}


#this is the specific functional form for rescaling wavelet coefficients

charGauss <- function(x, sigma) exp(-sigma ^ 2 * x ^ 2 / 2)


landscape.size <- 128/2

E <- 5
y <- waveSynth2(landscape.size, charGauss, exp(E), wavelet = "la8")

#rescale environment to range from 0-100
y <- y - min(y)
y <- y * 99.9999/max(y)

F <- 1
x <- waveSynth2(landscape.size, charGauss, exp(F), wavelet = "la8")

#-1 indicates matrix, which is 75% of landscape
prop.matrix <- 0.0
if (prop.matrix == 0){
  x[1:length(x)] <- 1
} else {
  x[x < quantile(x, prop.matrix)] <- -1
  x[x > quantile(x, prop.matrix)] <- 1
}

#combine the reserve mask and the underlying environmental grid
y2 <- x * y
y2[y2 < 0] <- -1

y2
levelplot(y2)



### Convert landscape to 0,1,2 scale

tmp.y2 <- y2

tmp.y2[y2 == -1] <- 0
tmp.y2[y2 > 0 & y2 <= median(y2[y2>0])] <- 1
tmp.y2[y2 > mean(y2[y2>0])] <- 2


hist(tmp.y2)


plotLandscape(tmp.y2)



