LOCAL_COEF <- directions.table[,'local.coef']
SAFE_EGGS <- prop.safe.eggs

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





full.fit <- lm(LOCAL_COEF ~  env + 
                             # risk * 
                             memdepth*
                             perc + 
                             move * 
                             movemod)


summary(full.fit)


par(mar=c(25,5,1,1))
barplot(sort(summary(full.fit)$coef[,1]), las = 2)




####### Safe eggs fits

lm(SAFE_EGGS ~ env)
# lm(LOCAL_COEF ~ frag)
lm(SAFE_EGGS ~ risk)


lm(SAFE_EGGS ~ perc)
lm(SAFE_EGGS ~ move)
lm(SAFE_EGGS ~ movemod)
lm(SAFE_EGGS ~ memdepth)
# lm(LOCAL_COEF ~ memweight)





full.eggs.fit <- lm(SAFE_EGGS ~  env * 
                            # risk * 
                            perc * 
                            move * 
                            movemod * 
                            memdepth)

summary(full.eggs.fit)


par(mar=c(25,5,1,1))
barplot(sort(summary(full.eggs.fit)$coef[,1]), las = 2)













