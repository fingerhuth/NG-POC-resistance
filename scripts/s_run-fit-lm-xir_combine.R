args <- commandArgs(trailingOnly=T)

pop <- as.character(args[1])

pn <- as.numeric(args[2])

dvec <- c(1/365, 7/365)
psivec <- c(0.3, 0.6, 0.9)
lsvec <- c(0.3, 0.6, 0.9)
xirvec <- c(0, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99)

source(paste("pn_", pn, ".R", sep=""))

fr <- array(data=NA, dim=c(4,length(dvec),length(psivec),length(lsvec), length(xirvec), max.outros))

for (j in 0:(max.outros-1)){
  load(paste("../data/fittedRates_", pop, "_", j, "_", pn, ".data", sep=""))
  fr[,,,,,j+1] <- fittedRates
}

save(fr, file=paste("../data/fr_", pop, "_", pn, ".data", sep=""))