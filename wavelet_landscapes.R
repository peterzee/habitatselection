library(waveslim)

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


#To create original intact landscapes & then create fragmented reserves of different sizes
for(E in c(-Inf,0:6)){ #a range of different scales of spatial environmental autocorrelation
  
  #this makes the habitat
  y <- waveSynth2(128, charGauss, exp(E), wavelet = "la8")
  
  #rescale environment to range from 0-100
  y <- y - min(y)
  y <- y * 99.9999/max(y)
  
  #save intact landscape
  name <- paste('E_exp', E, 'orig_set1.csv', sep='') 
  # write.csv(y,name, row.names=FALSE)
  
  for(F in c(-Inf,0:6)){ #create a range of different reserve sizes for the same underlying landscape
    
    #surface used to denote reserves
    x <- waveSynth2(128, charGauss, exp(F), wavelet = "la8")
    
    #-1 indicates matrix, which is 75% of landscape
    x[x < quantile(x, 0.75)] <- -1
    x[x > quantile(x, 0.75)] <- 1
    #combine the reserve mask and the underlying environmental grid
    y2 <- x*y
    y2[y2 < 0] <- -1
    
    #saves the post-habitat loss landscape
    name2 <- paste('E_exp',E,'_F_exp',F,'_set1.csv',sep='')
    # write.csv(y2,name2,row.names=FALSE)
  }
}



