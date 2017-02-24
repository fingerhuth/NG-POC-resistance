args = commandArgs(trailingOnly=T)
pn <- as.numeric(args[1])

source(paste("pn_", pn, ".R", sep=""))


for (popno in c(1,2)){ # population loop
  if(popno == 1){
    seed <- 312774
    pop <- "msm"
    psi <- psi.msm
    
  }else{
    seed <- 993734
    pop <- "het" 
    psi <- psi.het
  }

  for (m in 1:4){
    for (tpi in 0:((length(timesR)-1)/n.tp)){
      arguments <- paste(tpi, m, max.outros, pop, yearsR, pyearsR, pn, n.tp, n.sim, sep=" ")
      system(paste("bsub -R \"rusage[scratch=600]\" ./s_dd-wrapper.sh ", arguments, sep=""))
    }
  }
}