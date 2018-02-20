aa <- which(wrap.info[,1] > 0)
categories <- rep(NA, length(aa))
for (i in 1:length(aa)){
  
  category.sort <- c(
    wrap.info[aa[i],2] < 1,
    wrap.info[aa[i],2] > ncol(A),
    wrap.info[aa[i],3] < 1,
    wrap.info[aa[i],3] > nrow(A)
  )
  x <- category.sort
  if ( sum(x == c(1,0,0,0)) == 4 ){ categories[i] <- 'lr' }
  if ( sum(x == c(0,1,0,0)) == 4 ){ categories[i] <- 'rl' }
  if ( sum(x == c(0,0,1,0)) == 4 ){ categories[i] <- 'bt' }
  if ( sum(x == c(0,0,0,1)) == 4 ){ categories[i] <- 'tb' }
  if ( sum(x == c(1,0,1,0)) == 4 ){ categories[i] <- 'lrbt' }
  if ( sum(x == c(1,0,0,1)) == 4 ){ categories[i] <- 'lrtb' }
  if ( sum(x == c(0,1,1,0)) == 4 ){ categories[i] <- 'rlbt' }
  if ( sum(x == c(0,1,0,1)) == 4 ){ categories[i] <- 'rltb' }
}

categories
