## Spatial dispersal kernal

## evenutally expand this to take any distribution
# dist <- rnorm(1000, 5, 5)
# dist <- 3 * rgamma(1000, 8, 3)

## for now, using geometric distribution

### geometric kernal
geom.spatial.kernal <- function(N, geom.prob, ){


dist <- rgeom(1, prob = 0.6) + 1
# dist <- rnorm(1000, 5, 5)
# dist <- 3 * rgamma(1000, 8, 3)
theta <- runif(1, 0, 2*pi)
x <- cos(theta) * dist
y <- sin(theta) * dist

landing <- round(cbind(x,y))

tmp.mvt <- locations[i-1,] + landing


locations[i,] <- cbind(tmp.mvt[,1] %% ncol(A),
                       tmp.mvt[,2] %% nrow(A))


return()
}




