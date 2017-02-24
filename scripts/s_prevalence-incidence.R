setwd("~/PhD/ng_poc/repository/scripts/")

library(xtable)

max.outros <- 1000 # number of parameter sets used in paper


for (popno in c(1,2)){ # population loop
  if(popno == 1){
    seed <- 312774
    pop <- "msm"
  }else{
    seed <- 993734
    pop <- "het" 
  }
  
  load(paste("../data/outros_", pop, "-", seed, "-", pop, ".data", sep=""))
  
  
  odf <- outros[1:max.outros,]
  
  
  # prevL, prevH, prevT, incL, incH, incT, theta
  
  prevM <- apply(odf[,1:3], 2, median)
  prev50low <- apply(odf[,1:3], 2, function(x) quantile(x, 0.25))
  prev50up <- apply(odf[,1:3], 2, function(x) quantile(x, 0.75))
  incM <- median(odf[,6]*odf[,11])
  inc50low <- quantile(odf[,6]*odf[,11], 0.25)
  inc50up <- quantile(odf[,6]*odf[,11], 0.75)
  
  df <- rbind(cbind(prevM, prev50low, prev50up), cbind(incM, inc50low, inc50up))
  df2a <- df[1:3,]*100
  df2b <- df[4,]*100000
  
  df <- rbind(df2a, df2b)
  
  print(xtable(df))
  
  
}
