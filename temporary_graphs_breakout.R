xxx <- directions.table
# xxx <- safe.directions
# xxx <- risk.directions


boxplot(
  xxx[which(xxx[,'env.vec'] == 1 & xxx[,'mem.weight.vec'] == 0), 'local.coef'],
  xxx[which(xxx[,'env.vec'] == 1 & xxx[,'mem.weight.vec'] == 0.1), 'local.coef'],
  xxx[which(xxx[,'env.vec'] == 1 & xxx[,'mem.weight.vec'] == 0), 'regional.coef'],
  xxx[which(xxx[,'env.vec'] == 1 & xxx[,'mem.weight.vec'] == 0.1), 'regional.coef']
  )
boxplot(
  xxx[which(xxx[,'env.vec'] == 3 & xxx[,'mem.weight.vec'] == 0), 'local.coef'],
  xxx[which(xxx[,'env.vec'] == 3 & xxx[,'mem.weight.vec'] == 0.1), 'local.coef'],
  xxx[which(xxx[,'env.vec'] == 3 & xxx[,'mem.weight.vec'] == 0), 'regional.coef'],
  xxx[which(xxx[,'env.vec'] == 3 & xxx[,'mem.weight.vec'] == 0.1), 'regional.coef']
)
boxplot(
  xxx[which(xxx[,'env.vec'] == 4 & xxx[,'mem.weight.vec'] == 0), 'local.coef'],
  xxx[which(xxx[,'env.vec'] == 4 & xxx[,'mem.weight.vec'] == 0.1), 'local.coef'],
  xxx[which(xxx[,'env.vec'] == 4 & xxx[,'mem.weight.vec'] == 0), 'reigonal.coef'],
  xxx[which(xxx[,'env.vec'] == 4 & xxx[,'mem.weight.vec'] == 0.1), 'reigonal.coef']
)



boxplot(
  xxx[which(xxx[,'mem.weight.vec'] == 0), 'local.coef'],
  xxx[which(xxx[,'mem.weight.vec'] == 0.1), 'local.coef']
)



plot(
  xxx[which(xxx[,'env.vec'] == 1 & xxx[,'risk.vec'] == 0.0),'local.coef'],
  prop.safe.eggs[which(xxx[,'env.vec'] == 1 & xxx[,'risk.vec'] == 0.0)]
)



boxplot(
  prop.safe.eggs[which(xxx[,'mvt.mod.vec'] == 0)], 
  prop.safe.eggs[which(xxx[,'mvt.mod.vec'] == 4)]
)
