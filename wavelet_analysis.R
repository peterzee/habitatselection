# plotLandscape(landscape)
# dev.off()

r_moore <- 1
qq <- as_data_frame(moore$big.table[,,r_moore])
ww <- as_data_frame(moore$neighborhood.totals[,,r_moore])



qq %>% 
  filter(n.eggs != 0) %>% 
  lm(n.eggs ~ risk.score, data = .) %>% 
  summary()


ww %>% 
  lm(mean.eggs.safe ~ n.risk.patches, data = .) %>% 
  summary() 
  
x <- ww %>% 
  select(n.risk.patches, mean.eggs.safe)

plot(jitter(x$n.risk.patches),x$mean.eggs.safe)
# boxplot(x$mean.eggs.safe ~ x$n.risk.patches)

y <- ww %>% 
  select(n.risk.patches, mean.eggs.safe) %>% 
  count(n.risk.patches)

plot(y$n.risk.patches, y$n)


z <- ww %>% 
  filter(patch.type == 1) %>% 
  select(n.risk.patches, mean.eggs.safe)

plot(jitter(z$n.risk.patches), z$mean.eggs.safe)

