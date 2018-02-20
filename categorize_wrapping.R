wrap.info[wrap.info[,1] > 0,]

category.sort <- c(
  
  wrap.info[63,2] < 1,
  wrap.info[63,2] > ncol(A),
  wrap.info[63,3] < 1,
  wrap.info[63,3] > nrow(A)
                         
)

categories <- 
x <- category.sort

if ( sum(x == c(1,0,0,0)) == 4 ){ categories <- 'lr' }
if ( sum(x == c(0,1,0,0)) == 4 ){ categories <- 'rl' }
if ( sum(x == c(0,0,1,0)) == 4 ){ categories <- 'bt' }
if ( sum(x == c(0,0,0,1)) == 4 ){ categories <- 'tb' }
if ( sum(x == c(1,0,1,0)) == 4 ){ categories <- 'lrbt' }
if ( sum(x == c(1,0,0,1)) == 4 ){ categories <- 'lrtb' }
if ( sum(x == c(0,1,1,0)) == 4 ){ categories <- 'rlbt' }
if ( sum(x == c(0,1,0,1)) == 4 ){ categories <- 'rltb' }

categories
