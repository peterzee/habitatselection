# xxx <- directions.table
# xxx <- safe.directions
# zzz <- risk.directions


# plot(xxx[,11], xxx[,13], ylim=c(-5,5), xlim=c(-5,5))
# points(yyy[,11], yyy[,13], col = 'dodgerblue')
# points(zzz[,11], zzz[,13], col = 'tomato')

par(mfrow=c(2,3))
boxplot(
  xxx[which(xxx[,'env.vec'] == 1), 'local.coef'],
  xxx[which(xxx[,'env.vec'] == 2), 'local.coef'],
  xxx[which(xxx[,'env.vec'] == 4), 'local.coef']
)

boxplot(
  xxx[which(xxx[,'risk.vec'] == 0), 'local.coef'],
  xxx[which(xxx[,'risk.vec'] == 0.2), 'local.coef']
)

boxplot(
  xxx[which(xxx[,'perception.vec'] == 0.3), 'local.coef'],
  xxx[which(xxx[,'perception.vec'] == 0.7), 'local.coef']
)
boxplot(
  xxx[which(xxx[,'mvt.vec'] == 1), 'local.coef'],
  xxx[which(xxx[,'mvt.vec'] == 5), 'local.coef']
)
boxplot(
  xxx[which(xxx[,'mvt.mod.vec'] == 0), 'local.coef'],
  xxx[which(xxx[,'mvt.mod.vec'] == 4), 'local.coef']
)
boxplot(
  xxx[which(xxx[,'mem.depth.vec'] == 0), 'local.coef'],
  xxx[which(xxx[,'mem.depth.vec'] == 10), 'local.coef']
)


boxplot(
  xxx[which(xxx[,'env.vec'] == 1 & xxx[,'mem.depth.vec'] == 0), 'local.coef'],
  xxx[which(xxx[,'env.vec'] == 1 & xxx[,'mem.depth.vec'] == 10), 'local.coef'],
  xxx[which(xxx[,'env.vec'] == 2 & xxx[,'mem.depth.vec'] == 0), 'local.coef'],
  xxx[which(xxx[,'env.vec'] == 2 & xxx[,'mem.depth.vec'] == 10), 'local.coef'],
  xxx[which(xxx[,'env.vec'] == 4 & xxx[,'mem.depth.vec'] == 0), 'local.coef'],
  xxx[which(xxx[,'env.vec'] == 4 & xxx[,'mem.depth.vec'] == 10), 'local.coef']
)

boxplot(
  xxx[which(xxx[,'risk.vec'] == 0.0 & xxx[,'perception.vec'] == 0.3), 'local.coef'],
  xxx[which(xxx[,'risk.vec'] == 0.0 & xxx[,'perception.vec'] == 0.7), 'local.coef'],
  xxx[which(xxx[,'risk.vec'] == 0.2 & xxx[,'perception.vec'] == 0.3), 'local.coef'],
  xxx[which(xxx[,'risk.vec'] == 0.2 & xxx[,'perception.vec'] == 0.7), 'local.coef']
)

boxplot(
  xxx[which(xxx[,'risk.vec'] == 0.0 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.mod.vec'] == 0), 'local.coef'],
  xxx[which(xxx[,'risk.vec'] == 0.0 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.mod.vec'] == 0), 'local.coef'],
  xxx[which(xxx[,'risk.vec'] == 0.2 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.mod.vec'] == 0), 'local.coef'],
  xxx[which(xxx[,'risk.vec'] == 0.2 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.mod.vec'] == 0), 'local.coef'],
  xxx[which(xxx[,'risk.vec'] == 0.0 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.mod.vec'] == 4), 'local.coef'],
  xxx[which(xxx[,'risk.vec'] == 0.0 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.mod.vec'] == 4), 'local.coef'],
  xxx[which(xxx[,'risk.vec'] == 0.2 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.mod.vec'] == 4), 'local.coef'],
  xxx[which(xxx[,'risk.vec'] == 0.2 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.mod.vec'] == 4), 'local.coef']
)


plot(
  xxx[which(xxx[,'env.vec'] == 1 & xxx[,'risk.vec'] == 0.0),'local.coef'],
  prop.safe.eggs[which(xxx[,'env.vec'] == 1 & xxx[,'risk.vec'] == 0.0)]
)


boxplot(
  xxx[which(xxx[,'mem.depth.vec'] == 0), 'local.coef'], 
  xxx[which(xxx[,'mem.depth.vec'] == 10), 'local.coef'],
  col = 'thistle'
)


######### Proportion safe plots ############

boxplot(
  prop.safe.eggs[which(xxx[,'env.vec'] == 1)], 
  prop.safe.eggs[which(xxx[,'env.vec'] == 2)], 
  prop.safe.eggs[which(xxx[,'env.vec'] == 4)]
)

boxplot(
  prop.safe.eggs[which(xxx[,'risk.vec'] == 0)],
  prop.safe.eggs[which(xxx[,'risk.vec'] == 0.2)]
)

boxplot(
  prop.safe.eggs[which(xxx[,'perception.vec'] == 0.3)],
  prop.safe.eggs[which(xxx[,'perception.vec'] == 0.7)]
)
boxplot(
  prop.safe.eggs[which(xxx[,'mvt.vec'] == 1)],
  prop.safe.eggs[which(xxx[,'mvt.vec'] == 5)]
)
boxplot(
  prop.safe.eggs[which(xxx[,'mvt.mod.vec'] == 0)],
  prop.safe.eggs[which(xxx[,'mvt.mod.vec'] == 4)]
)
boxplot(
  prop.safe.eggs[which(xxx[,'mem.depth.vec'] == 0) ],
  prop.safe.eggs[which(xxx[,'mem.depth.vec'] == 10)]
)









boxplot(
  prop.safe.eggs[which(xxx[,'perception.vec'] == 0.3 & xxx[,'risk.vec'] == 0)], 
  prop.safe.eggs[which(xxx[,'perception.vec'] == 0.7 & xxx[,'risk.vec'] == 0)],
  prop.safe.eggs[which(xxx[,'perception.vec'] == 0.3 & xxx[,'risk.vec'] == 0.2)], 
  prop.safe.eggs[which(xxx[,'perception.vec'] == 0.7 & xxx[,'risk.vec'] == 0.2)]
)


boxplot(
  prop.safe.eggs[which(xxx[,'mem.depth.vec'] == 0)], 
  prop.safe.eggs[which(xxx[,'mem.depth.vec'] == 10)]
)


boxplot(
  prop.safe.eggs[which(xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 0)], 
  prop.safe.eggs[which(xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 0)],
  prop.safe.eggs[which(xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 4)],
  prop.safe.eggs[which(xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 4)]
)


boxplot(
  prop.safe.eggs[which(xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 0)], 
  prop.safe.eggs[which(xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 0)],
  prop.safe.eggs[which(xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 4)],
  prop.safe.eggs[which(xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 4)]
)

