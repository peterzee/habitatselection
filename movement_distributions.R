x.max <- 15
xx <- seq(0, 15)
prob.vec <- seq(0.1, 0.9, by = 0.2)
color.vec <- c('black', 'tomato', 'forestgreen', 'dodgerblue', 'thistle')
plot(xx+1, dgeom(xx,0.1), type = 'b', pch = 19, lwd = 3,
     xlim = c(0,max(xx)+1), ylim = c(0,1),
     xlab = "distance", ylab = 'probability')
for (i in 2:length(prob.vec)){
  points(xx+1, dgeom(xx, prob.vec[i]), type = 'b', pch = 19, lwd = 3,
         col = color.vec[i])
}