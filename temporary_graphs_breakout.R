xxx <- directions.table
# xxx <- safe.directions
# zzz <- risk.directions


plot(xxx[,11], xxx[,13], ylim=c(-5,5), xlim=c(-5,5))
points(yyy[,11], yyy[,13], col = 'dodgerblue')
points(zzz[,11], zzz[,13], col = 'tomato')



boxplot(
  xxx[which(xxx[,'env.vec'] == 1 & xxx[,'mem.depth.vec'] == 0), 'local.coef'],
  xxx[which(xxx[,'env.vec'] == 1 & xxx[,'mem.depth.vec'] == 10), 'local.coef'],
  xxx[which(xxx[,'env.vec'] == 4 & xxx[,'mem.depth.vec'] == 0), 'local.coef'],
  xxx[which(xxx[,'env.vec'] == 4 & xxx[,'mem.depth.vec'] == 10), 'local.coef']
  )

boxplot(
  xxx[which(xxx[,'risk.vec'] == 0.0 & xxx[,'perception.vec'] == 0.3), 'local.coef'],
  xxx[which(xxx[,'risk.vec'] == 0.0 & xxx[,'perception.vec'] == 0.7), 'local.coef'],
  xxx[which(xxx[,'risk.vec'] == 0.2 & xxx[,'perception.vec'] == 0.3), 'local.coef'],
  xxx[which(xxx[,'risk.vec'] == 0.2 & xxx[,'perception.vec'] == 0.7), 'local.coef']
)


plot(
  xxx[which(xxx[,'env.vec'] == 1 & xxx[,'risk.vec'] == 0.0),'local.coef'],
  prop.safe.eggs[which(xxx[,'env.vec'] == 1 & xxx[,'risk.vec'] == 0.0)]
)



boxplot(
  prop.safe.eggs[which(xxx[,'env.vec'] == 1)], 
  prop.safe.eggs[which(xxx[,'env.vec'] == 4)]
)



boxplot(
  prop.safe.eggs[which(xxx[,'perception.vec'] == 0.3)], 
  prop.safe.eggs[which(xxx[,'perception.vec'] == 0.7)]
)


boxplot(
  prop.safe.eggs[which(xxx[,'mem.depth.vec'] == 0)], 
  prop.safe.eggs[which(xxx[,'mem.depth.vec'] == 10)]
)
