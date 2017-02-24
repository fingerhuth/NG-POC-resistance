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
    arguments <- paste(yearsR, pop, m, pyearsR, pn, n.tp, sep=" ")
    system(paste("bsub -J merge-", pop, "-", pn, " Rscript s_run-model_12ff.R ", arguments, sep=""))
  }
}