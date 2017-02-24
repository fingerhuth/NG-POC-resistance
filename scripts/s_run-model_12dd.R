# for timepoint tp, calculate prevalence, incidence and proportion resistance and their summary statistics (median, mean, lower 50%, upper 50%, lower 95%, upper 95%)
# output: only summary statistics!
#       - sus.prev: summary statistics for prevalence
#       - sus.inc: summary statistics for incidence
#       - sus.pres: summary statistics for proportion resistance

args = commandArgs(trailingOnly=T)

tpi <- as.numeric(args[1])
scenario <- as.numeric(args[2])
max.outros <- as.numeric(args[3])
pop <- as.character(args[4])
yearsR <- as.numeric(args[5])
pyearsR <- as.numeric(args[6])
pn <- as.numeric(args[7])
n.tp <- as.numeric(args[8])
n.sim <- as.numeric(args[9])

timesR <- seq(0,yearsR, pyearsR)
lt <- length(timesR)
# inititalize summary statistics matrix
susta <- matrix(data=NA, nrow=max.outros, ncol=13)
farray <- numeric(length=max.outros)



load(paste("../data/behav_", pop, ".data", sep=""))
N <- behav[1]
N[2] <- 1-N


if(lt-n.tp*(tpi+1)<0){
  tpmax <- lt-n.tp*tpi
}else{
  tpmax <- n.tp
}

temp.prev <- array(data=NA, dim=c(tpmax,max.outros,3), dimnames=list((tpi*n.tp+1):(tpi*n.tp+tpmax),1:max.outros, c("prevL", "prevH", "prevT")))
temp.inc <- array(data=NA, dim=c(tpmax,max.outros,3), dimnames=list((tpi*n.tp+1):(tpi*n.tp+tpmax),1:max.outros, c("prevL", "prevH", "prevT")))
temp.obsinc <- array(data=NA, dim=c(tpmax,max.outros,3), dimnames=list((tpi*n.tp+1):(tpi*n.tp+tpmax),1:max.outros, c("prevL", "prevH", "prevT")))
temp.pres <- array(data=NA, dim=c(tpmax,max.outros,3), dimnames=list((tpi*n.tp+1):(tpi*n.tp+tpmax),1:max.outros, c("prevL", "prevH", "prevT")))

temp.res.inc <- array(data=NA, dim=c(tpmax,max.outros,3), dimnames=list((tpi*n.tp+1):(tpi*n.tp+tpmax),1:max.outros, c("prevL", "prevH", "prevT")))
temp.res.obsinc <- array(data=NA, dim=c(tpmax,max.outros,3), dimnames=list((tpi*n.tp+1):(tpi*n.tp+tpmax),1:max.outros, c("prevL", "prevH", "prevT")))

sus.prev <- array(data=NA, dim=c(tpmax,6,3), dimnames=list((tpi*n.tp+1):(tpi*n.tp+tpmax),c("median", "mean", "low50", "up50", "low95", "up95"), c("prevL", "prevH", "prevT")))
sus.inc <- array(data=NA, dim=c(tpmax,6,3), dimnames=list((tpi*n.tp+1):(tpi*n.tp+tpmax),c("median", "mean", "low50", "up50", "low95", "up95"), c("prevL", "prevH", "prevT")))
sus.obsinc <- array(data=NA, dim=c(tpmax,6,3), dimnames=list((tpi*n.tp+1):(tpi*n.tp+tpmax),c("median", "mean", "low50", "up50", "low95", "up95"), c("prevL", "prevH", "prevT")))
sus.pres <- array(data=NA, dim=c(tpmax,6,3), dimnames=list((tpi*n.tp+1):(tpi*n.tp+tpmax),c("median", "mean", "low50", "up50", "low95", "up95"), c("prevL", "prevH", "prevT")))
  


for (i in 1:tpmax){
  
  tp <- tpi*n.tp+i
    
  for (j in 0:floor((max.outros-1)/n.sim)){
    load(paste("../data/12_resce_", pop, "_", j, "_", pn, ".data", sep=""))
    load(paste("../data/12_printh_", pop, "_", j, "_", pn, ".data", sep=""))
    
    if(max.outros-(j+1)*(n.sim)<0){
      ceil <- max.outros
    }else{
      ceil <- (j+1)*n.sim
    }
    
    farray[(1+j*n.sim):ceil] <- printh[,11]
    susta[(1+j*n.sim):ceil,] <- t(resce[tp,,scenario,])
  }
  
  
  # calculate prevalences
  temp.prev[i,,1] <- rowSums(susta[,c(4,6,8)])/N[1] #prevL
  temp.prev[i,,2] <- rowSums(susta[,c(5,7,9)])/N[2] #prevH
  temp.prev[i,,3] <- rowSums(susta[,4:9]) #prevT
  
  # calculate incidence
  temp.inc[i,,1] <- rowSums(susta[,c(10,12)])/N[1] #incL
  temp.inc[i,,2] <- rowSums(susta[,c(11,13)])/N[2] #incH
  temp.inc[i,,3] <- rowSums(susta[,10:13]) #incT

  temp.res.inc[i,,3] <- rowSums(susta[,12:13]) #resistant incT
  
  # observed incidence
  temp.obsinc[i,,1] <- (rowSums(susta[,c(10,12)])/N[1])*farray #observed incL
  temp.obsinc[i,,2] <- (rowSums(susta[,c(11,13)])/N[2])*farray #observed incH
  temp.obsinc[i,,3] <- (rowSums(susta[,10:13]))*farray #observed incT
  
  temp.res.obsinc[i,,3] <- (rowSums(susta[,12:13]))*farray #observed incT

  # calculate proportion resistance
  temp.pres[i,,1] <- rowSums(susta[,c(6,8)])/rowSums(susta[,c(4,6,8)]) # proportion resistant in low risk: (W_L+RES_L)/(W_L+RES_L+SEN_L)
  temp.pres[i,,2] <- rowSums(susta[,c(7,9)])/rowSums(susta[,c(5,7,9)]) # proportion resistant in high risk: (W_H+RES_H)/(W_H+RES_H+SEN_H)
  temp.pres[i,,3] <- rowSums(susta[,6:9])/rowSums(susta[,4:9]) # proportion resistant in total pop: (W_L+W_H+RES_L+RES_H)/(W_L+W_H+RES_L+RES_H+SEN_L+SEN_H)
  
  ## calculate summary statistics
  ## timeline: mean, median, 50%up, 50%low, 95%up, 95%low
  # prevalence
  qts.prev <- apply(temp.prev[i,,], 2, function(x) quantile(x, probs=c(0.025, 0.25, 0.75, 0.975)))
  sus.prev[i,1,] <- apply(temp.prev[i,,], 2, median)
  sus.prev[i,2,] <- apply(temp.prev[i,,], 2, mean)
  sus.prev[i,3,] <- qts.prev[2,]
  sus.prev[i,4,] <- qts.prev[3,]
  sus.prev[i,5,] <- qts.prev[1,]
  sus.prev[i,6,] <- qts.prev[4,]
  
  # incidence
  qts.inc <- apply(temp.inc[i,,], 2, function(x) quantile(x, probs=c(0.025, 0.25, 0.75, 0.975)))
  sus.inc[i,1,] <- apply(temp.inc[i,,], 2, median)
  sus.inc[i,2,] <- apply(temp.inc[i,,], 2, mean)
  sus.inc[i,3,] <- qts.inc[2,]
  sus.inc[i,4,] <- qts.inc[3,]
  sus.inc[i,5,] <- qts.inc[1,]
  sus.inc[i,6,] <- qts.inc[4,]
  
  # observed incidence
  qts.obsinc <- apply(temp.obsinc[i,,], 2, function(x) quantile(x, probs=c(0.025, 0.25, 0.75, 0.975)))
  sus.obsinc[i,1,] <- apply(temp.obsinc[i,,], 2, median)
  sus.obsinc[i,2,] <- apply(temp.obsinc[i,,], 2, mean)
  sus.obsinc[i,3,] <- qts.obsinc[2,]
  sus.obsinc[i,4,] <- qts.obsinc[3,]
  sus.obsinc[i,5,] <- qts.obsinc[1,]
  sus.obsinc[i,6,] <- qts.obsinc[4,]
  
  # proportion resistant
  qts.pres <- apply(temp.pres[i,,], 2, function(x) quantile(x, probs=c(0.025, 0.25, 0.75, 0.975)))
  sus.pres[i,1,] <- apply(temp.pres[i,,], 2, median)
  sus.pres[i,2,] <- apply(temp.pres[i,,], 2, mean)
  sus.pres[i,3,] <- qts.pres[2,]
  sus.pres[i,4,] <- qts.pres[3,]
  sus.pres[i,5,] <- qts.pres[1,]
  sus.pres[i,6,] <- qts.pres[4,]
}

tmp.incT <- array(data=NA, dim=c(2,tpmax,max.outros), dimnames=list(c("incT", "obsincT"),(tpi*n.tp+1):(tpi*n.tp+tpmax),1:max.outros))
tmp.incT[1,,] <- temp.inc[,,3]
tmp.incT[2,,] <- temp.obsinc[,,3]

tmp.res.incT <- array(data=NA, dim=c(2,tpmax,max.outros), dimnames=list(c("incT", "obsincT"),(tpi*n.tp+1):(tpi*n.tp+tpmax),1:max.outros))
tmp.res.incT[1,,] <- temp.res.inc[,,3]
tmp.res.incT[2,,] <- temp.res.obsinc[,,3]

save(sus.prev, file=paste("../data/12_sus-prev_", pop, "_", scenario, "_", tpi, "_", pn,".data", sep=""))
save(sus.inc, file=paste("../data/12_sus-inc_", pop, "_", scenario, "_", tpi, "_", pn, ".data", sep=""))
save(sus.obsinc, file=paste("../data/12_sus-obsinc_", pop, "_", scenario, "_", tpi, "_", pn, ".data", sep=""))
save(sus.pres, file=paste("../data/12_sus-pres_", pop, "_", scenario, "_", tpi, "_", pn, ".data", sep=""))
save(tmp.incT, file=paste("../data/12_tmp-incT_", pop, "_", scenario, "_", tpi, "_", pn, ".data", sep=""))
save(tmp.res.incT, file=paste("../data/12_tmp-res-incT_", pop, "_", scenario, "_", tpi, "_", pn, ".data", sep=""))