args = commandArgs(trailingOnly=T)

yearsR <- as.numeric(args[1])
pop <- as.character(args[2])
scenario <- as.numeric(args[3])
pyearsR <- as.numeric(args[4])
pn <- as.numeric(args[5])
n.tp <- as.numeric(args[6])

source(paste("pn_", pn, ".R", sep=""))
timesR <- seq(0,yearsR,pyearsR)

tss.prevL <- matrix(data=NA, nrow=length(timesR), ncol=6)
tss.prevH <- matrix(data=NA, nrow=length(timesR), ncol=6)
tss.prevT <- matrix(data=NA, nrow=length(timesR), ncol=6)

tss.incL <- matrix(data=NA, nrow=length(timesR), ncol=6)
tss.incH <- matrix(data=NA, nrow=length(timesR), ncol=6)
tss.incT <- matrix(data=NA, nrow=length(timesR), ncol=6)

tss.obsincL <- matrix(data=NA, nrow=length(timesR), ncol=6)
tss.obsincH <- matrix(data=NA, nrow=length(timesR), ncol=6)
tss.obsincT <- matrix(data=NA, nrow=length(timesR), ncol=6)

tss.presL <- matrix(data=NA, nrow=length(timesR), ncol=6)
tss.presH <- matrix(data=NA, nrow=length(timesR), ncol=6)
tss.presT <- matrix(data=NA, nrow=length(timesR), ncol=6)

tmp.cumincT <- matrix(data=NA, nrow=length(timesR), ncol=max.outros)
tmp.cumobsincT <- matrix(data=NA, nrow=(length(timesR)), ncol=max.outros)

tmp.res.cumincT <- matrix(data=NA, nrow=length(timesR), ncol=max.outros)
tmp.res.cumobsincT <- matrix(data=NA, nrow=(length(timesR)), ncol=max.outros)


for (tpi in 0:((length(timesR)-1)/n.tp)){
  
  if(length(timesR)-(tpi+1)*(n.tp)<0){
    ceil <- length(timesR)
  }else{
    ceil <- (tpi+1)*n.tp
  }
  
  ts <- (1+tpi*n.tp):ceil
  
  load(paste("../data/12_sus-prev_", pop, "_", scenario, "_", tpi, "_", pn,".data", sep=""))
  tss.prevL[ts,] <- sus.prev[,,1]
  tss.prevH[ts,] <- sus.prev[,,2]
  tss.prevT[ts,] <- sus.prev[,,3]
  
  load(paste("../data/12_sus-inc_", pop, "_", scenario, "_", tpi, "_", pn,".data", sep=""))
  tss.incL[ts,] <- sus.inc[,,1]
  tss.incH[ts,] <- sus.inc[,,2]
  tss.incT[ts,] <- sus.inc[,,3]
  
  load(paste("../data/12_sus-obsinc_", pop, "_", scenario, "_", tpi, "_", pn,".data", sep=""))
  tss.obsincL[ts,] <- sus.obsinc[,,1]
  tss.obsincH[ts,] <- sus.obsinc[,,2]
  tss.obsincT[ts,] <- sus.obsinc[,,3]
  
  load(paste("../data/12_sus-pres_", pop, "_", scenario, "_", tpi, "_", pn,".data", sep=""))
  tss.presL[ts,] <- sus.pres[,,1]
  tss.presH[ts,] <- sus.pres[,,2]
  tss.presT[ts,] <- sus.pres[,,3]
  
  load(paste("../data/12_tmp-incT_", pop, "_", scenario, "_", tpi, "_", pn, ".data", sep=""))
  tmp.cumincT[ts, ] <- tmp.incT[1,,]
  tmp.cumobsincT[ts,] <- tmp.incT[2,,]
  
  load(paste("../data/12_tmp-res-incT_", pop, "_", scenario, "_", tpi, "_", pn, ".data", sep=""))
  tmp.res.cumincT[ts, ] <- tmp.res.incT[1,,]
  tmp.res.cumobsincT[ts,] <- tmp.res.incT[2,,]
}

# summation interval
interval <- 1/pyearsR
inds <- c()
for(ival in 1:((length(timesR)-1)/interval)){
  inds <- c(inds, (ival*interval+1))
}
cumincT <- apply(tmp.cumincT[inds,], 2, cumsum)
cumobsincT <- apply(tmp.cumobsincT[inds,], 2, cumsum)

res.cumincT <- apply(tmp.res.cumincT[inds,], 2, cumsum)
res.cumobsincT <- apply(tmp.res.cumobsincT[inds,], 2, cumsum)

save(tss.prevL, file=paste("../data/12_tss-prevL_", pop, "_", scenario, "_", pn,".data", sep=""))
save(tss.prevH, file=paste("../data/12_tss-prevH_", pop, "_", scenario, "_", pn,".data", sep=""))
save(tss.prevT, file=paste("../data/12_tss-prevT_", pop, "_", scenario, "_", pn,".data", sep=""))

save(tss.incL, file=paste("../data/12_tss-incL_", pop, "_", scenario, "_", pn,".data", sep=""))
save(tss.incH, file=paste("../data/12_tss-incH_", pop, "_", scenario, "_", pn,".data", sep=""))
save(tss.incT, file=paste("../data/12_tss-incT_", pop, "_", scenario, "_", pn,".data", sep=""))

save(tss.obsincL, file=paste("../data/12_tss-obsincL_", pop, "_", scenario, "_", pn,".data", sep=""))
save(tss.obsincH, file=paste("../data/12_tss-obsincH_", pop, "_", scenario, "_", pn,".data", sep=""))
save(tss.obsincT, file=paste("../data/12_tss-obsincT_", pop, "_", scenario, "_", pn,".data", sep=""))

save(tss.presL, file=paste("../data/12_tss-presL_", pop, "_", scenario, "_", pn,".data", sep=""))
save(tss.presH, file=paste("../data/12_tss-presH_", pop, "_", scenario, "_", pn,".data", sep=""))
save(tss.presT, file=paste("../data/12_tss-presT_", pop, "_", scenario, "_", pn,".data", sep=""))

save(cumincT, file=paste("../data/12_cumincT_", pop, "_", scenario, "_", pn,".data", sep=""))
save(cumobsincT, file=paste("../data/12_cumobsincT_", pop, "_", scenario, "_", pn,".data", sep=""))

save(res.cumincT, file=paste("../data/12_res-cumincT_", pop, "_", scenario, "_", pn,".data", sep=""))
save(res.cumobsincT, file=paste("../data/12_res-cumobsincT_", pop, "_", scenario, "_", pn,".data", sep=""))