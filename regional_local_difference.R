reg_loc_diff <- xxx[,'regional.coef'] - xxx[,'local.coef']


par(mfrow = c(1,2))

boxplot(
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 4)],
  
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 4)],
  
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 0 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 4)],
  col = rep(c(1,2,3), each = 12)
)
abline(h=0, lty=2)


boxplot(
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 1 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 4)],
  
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 0) ],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 0) ],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 0) ],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 4) ],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 4) ],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 4) ],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 0) ],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 0) ],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 0) ],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 4) ],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 4) ],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 2 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 4) ],
  
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.3 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 0)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 0.01 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 1 & xxx[,'mvt.mod.vec'] == 4)],
  reg_loc_diff[which(xxx[,'mem.depth.vec'] == 10 & xxx[,'env.vec'] == 4 & xxx[,'perception.vec'] == 0.7 & xxx[,'mvt.vec'] == 5 & xxx[,'mvt.mod.vec'] == 4)],
  col = rep(c(1,2,3), each =12)
)
abline(h=0, lty=2)

