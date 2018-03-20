

qq <- as_data_frame(moore$big.table[,,4])
ww <- as_data_frame(moore$neighborhood.totals[,,4])



qq %>% 
  filter(n.eggs != 0) %>% 
  lm(n.eggs ~ risk.score, data = .) %>% 
  summary()


ww %>% 
  lm(mean.eggs.safe ~ n.risk.patches, data = .) %>% 
  summary() 
  
x <- ww %>% 
  select(n.risk.patches, mean.eggs.safe)
  


plotLandscape(landscape)
dev.off()
plot(jitter(x$n.risk.patches, 0),x$mean.eggs.safe)



