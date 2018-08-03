summary(lm(directions.table[,'local.coef'] ~ directions.table[,'env.vec']))


LOCAL_COEF <- directions.table[,'local.coef']


env <- directions.table[,'env.vec']
frag <- directions.table[,'frag.vec']
risk <- directions.table[,'risk.vec']
perc <- directions.table[,'perception.vec']
move <- directions.table[,'mvt.vec']
movemod<- directions.table[,'mvt.mod.vec']
memdepth<- directions.table[,'mem.depth.vec']
memweight<- directions.table[,'mem.weight.vec']


lm(LOCAL_COEF ~ env)
# lm(LOCAL_COEF ~ frag)
lm(LOCAL_COEF ~ risk)
lm(LOCAL_COEF ~ perc)
lm(LOCAL_COEF ~ move)
lm(LOCAL_COEF ~ movemod)
lm(LOCAL_COEF ~ memdepth)
# lm(LOCAL_COEF ~ memweight)





full.fit <- lm(LOCAL_COEF ~  env * 
                             risk * 
                             perc * 
                             move * 
                             movemod * 
                             memdepth)

summary(full.fit)




