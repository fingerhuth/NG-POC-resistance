# main script combining 12a (callings 12b), 12c (calling 12d), 12e (calling 12f).

# send simulation batches to cluster nodes

args = commandArgs(trailingOnly=T)

## PASSNUMBER ##
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

  
  for (j in 0:floor((max.outros-1)/n.sim)){ # parallel loop
    arguments <- paste(j, pop, seed, max.outros, years, yearsR, pyearsR, psi, e_0a, eff.e_1b, eff.delta, omega, lambda_zeta, eff.lambda_sigma, xi_ng, eff.xi_r, pn, n.sim, sep=" ")
    system(paste("bsub -W 02:00 -J run.sims-", pop, "-", pn, " Rscript s_run-model_12bb_preexisting.R ", arguments, sep=""))
  } 

}  # end population loop
