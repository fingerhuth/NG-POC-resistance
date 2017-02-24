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

  arguments <- paste(pn, pop, sep=" ")
  system(paste("bsub -J merge-", pop, "-", pn, " Rscript s_run-model_12ll_preexisting.R ", arguments, sep=""))
}