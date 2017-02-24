# calculate observed cases averted (difference between (observed) cumulative incidence)

args = commandArgs(trailingOnly=T)

pn <- as.numeric(args[1])
pop <- as.character(args[2])

source(paste("pn_", pn, ".R", sep=""))
timesR <- seq(0,yearsR,pyearsR)

interval <- 1/pyearsR
inds <- c()
for(ival in 1:((length(timesR)-1)/interval)){
  inds <- c(inds, (ival*interval+1))
}
tmp.diffincT <- array(data=NA, dim=c(4,length(inds), max.outros))
tmp.diffobsincT <- array(data=NA, dim=c(4,length(inds), max.outros))

tmp.res.diffincT <- array(data=NA, dim=c(4,length(inds), max.outros))
tmp.res.diffobsincT <- array(data=NA, dim=c(4,length(inds), max.outros))

for (scenario in 2:4){
  load(file=paste("../data/12_cumincT_", pop, "_", scenario, "_", pn,".data", sep=""))
  tmp.diffincT[scenario,,] <- cumincT
  load(file=paste("../data/12_cumobsincT_", pop, "_", scenario, "_", pn,".data", sep=""))
  tmp.diffobsincT[scenario,,] <- cumobsincT
  
  load(file=paste("../data/12_res-cumincT_", pop, "_", scenario, "_", pn,".data", sep=""))
  tmp.res.diffincT[scenario,,] <- res.cumincT
  load(file=paste("../data/12_res-cumobsincT_", pop, "_", scenario, "_", pn,".data", sep=""))
  tmp.res.diffobsincT[scenario,,] <- res.cumobsincT
}

naat.casesAverted <- array(data=NA, dim=c(2, length(inds), max.outros))
naat.casesAverted[1,,] <-  tmp.diffincT[3,,]-tmp.diffincT[2,,]
naat.casesAverted[2,,] <-  tmp.diffincT[3,,]-tmp.diffincT[4,,]

naat.obscasesAverted <- array(data=NA, dim=c(2, length(inds), max.outros))
naat.obscasesAverted[1,,] <-  tmp.diffobsincT[3,,]-tmp.diffobsincT[2,,]
naat.obscasesAverted[2,,] <-  tmp.diffobsincT[3,,]-tmp.diffobsincT[4,,]


save(naat.obscasesAverted, file=paste("../data/12_naat-obscasesAverted_", pop, "_", pn,".data", sep=""))
save(naat.casesAverted, file=paste("../data/12_naat-casesAverted_", pop, "_", pn,".data", sep=""))