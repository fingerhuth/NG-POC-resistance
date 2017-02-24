args = commandArgs(trailingOnly=T)

j <- as.numeric(args[1]) # simulation batch number
pop <- as.character(args[2])
seed <- as.numeric(args[3])
max.outros <- as.numeric(args[4])

years <- as.numeric(args[5])
yearsR <- as.numeric(args[6])
pyearsR <- as.numeric(args[7])

psi <- as.numeric(args[8])
e_0a <- as.numeric(args[9])
eff.e_1b <- as.numeric(args[10])
eff.delta <- as.numeric(args[11])
omega <- as.numeric(args[12])
lambda_zeta <- as.numeric(args[13])
eff.lambda_sigma <- as.numeric(args[14])
xi_ng <- as.numeric(args[15])
eff.xi_r <- as.numeric(args[16])
pn <- as.numeric(args[17])
n.sim <- as.numeric(args[18])

dvec <- c(1/365, 7/365)
psivec <- c(0.3, 0.6, 0.9)
lsvec <- c(0.3, 0.6, 0.9)
xirvec <- c(0, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99)

# source files
source("f_model.R")
source("f_mixing.R")

# load libraries
library(deSolve)
library(rootSolve, lib.loc="../../lib/")

timesR <- seq(0,yearsR,pyearsR)

# use only parameter sets that worked in spread-model
load(paste("../data/outros_", pop, "-", seed, "-", pop, ".data", sep=""))
if(max.outros-(j+1)*(n.sim)<0){
  theta <- outros[(1+j*(n.sim)):max.outros,7:11]
  fittedRates <- array(data=NA, dim=c(4,length(dvec),length(psivec),length(lsvec), length(xirvec), max.outros-j*n.sim))
}else{
  theta <- outros[(1+j*(n.sim)):(j*(n.sim)+(n.sim)),7:11]
  fittedRates <- array(data=NA, dim=c(4,length(dvec),length(psivec),length(lsvec), length(xirvec), n.sim))
}


noG <- 2

# load an set behavioural parameters for population
load(paste("../data/behav_", pop, ".data", sep=""))
N <- behav[1]
N[2] <- 1-N
pic <- behav[2:3] # partner change or contact rate, previously called c, in SM paper pi

# set parameters for resistance-free equilibirum (CULT/NAAT-based)

# efficacies: ex_y: efficacy of treamtent y (a:1st line, b:alternative) in strain x (0:sens, 1:res)
e_1b <- eff.e_1b

# no resistance mutations for resistance free equilibirum! 
# spontaneous resistance
mu_0a <- 0
mu_a0 <- 0 # back mutations
# treatment induced resistance
mu_ta <- 0

# migration parameters
alpha <- 1/(44-16+1)
gamma <- 1

# waiting parameters
delta <- eff.delta
xi_r <- 0

for (k in 1:n.sim){ # parameter loop
  epsilon <- theta[1]
  betaL <- theta[2]
  betaH <- theta[3]
  D <- theta[4] 
  f <- theta[5]
  
  # initialize populations
  init <- c(N[1]*0.995,N[2]*0.90,N[1]*0.005, N[2]*0.1, 0, 0, 0, 0)
  
  # dependent parameters
  tt <- f/D
  nu <- 1/D - tt
  mu_ta <- 0
  
  
  rho <- mixing(epsilon, noG, pic, N)
  beta <- matrix(c(betaL,sqrt(betaL*betaH),sqrt(betaL*betaH),betaH),nrow=noG,ncol=noG)
  
  
  for (d in 1:length(dvec)){ # delta loop
    for (p in 1:length(psivec)){ # psi loop
      for (l  in 1:length(lsvec)){ # lambda_sigma loop
        
        zeta <- psivec[p]*tt/e_0a
        sigma <- (tt*(1-psivec[p]))/(xi_ng*lsvec[l]*e_0a-dvec[d]*tt*(1-psivec[p]))
        if(sigma <0){
          fittedRates[,d,p,l,x,k] <- -Inf
          print(paste("1-4", d, p, l, k, "skipped", sep=" "))
          next
        }
        
        
        # run model
        parms  <- c(pic=pic, noG=noG, alpha=alpha, nu=nu, gamma=gamma, rho=rho, 
                    zeta=zeta,
                    sigma=sigma,
                    delta=dvec[d],
                    lambda_sigma=lsvec[l],
                    lambda_zeta=lambda_zeta,
                    omega = omega,
                    e_0a=e_0a, 
                    e_1b=e_1b, 
                    xi_ng=xi_ng,
                    xi_r=xi_r,
                    mu_ta=mu_ta,
                    mu_0a=mu_0a,
                    mu_a0=mu_a0
        )
        temp.out <- runsteady(init, c(0,years), model, parms)
        out <- unlist(c(attr(temp.out, "time"), temp.out[1], temp.out[2]))
        
        initR.tmp <- unlist(temp.out[1])
        initR <- c(initR.tmp[1], initR.tmp[2], initR.tmp[3]*(1-10^-3), initR.tmp[4]*(1-10^-3), initR.tmp[3]*10^-3, initR.tmp[4]*10^-3, 0, 0)
        
        for(x in 1:length(xirvec)){
          eff.xi_r <- xirvec[x]
          
          vec.e_1b <- c(0, eff.e_1b, eff.e_1b, eff.e_1b)
          vec.delta <- c(dvec[d], dvec[d], dvec[d], 0)
          vec.xi_r <- c(eff.xi_r, eff.xi_r, 0, eff.xi_r)
          vec.lambda_sigma <- c(lsvec[l], lsvec[l], lsvec[l], 1)
          
          for(m in 1:4){ # scenario loop
            e_1b <- vec.e_1b[m]
            delta <- vec.delta[m]
            xi_r <- vec.xi_r[m]
            lambda_sigma <- vec.lambda_sigma[m]
            
            # mu_ta remains zero!
            
            parms  <- c(pic=pic, noG=noG, alpha=alpha, nu=nu, gamma=gamma, rho=rho, 
                        zeta=zeta,
                        sigma=sigma,
                        delta=delta,
                        lambda_sigma=lambda_sigma,
                        lambda_zeta=lambda_zeta,
                        omega = omega,
                        e_0a=e_0a, 
                        e_1b=e_1b, 
                        xi_ng=xi_ng,
                        xi_r=xi_r,
                        mu_ta=mu_ta,
                        mu_0a=mu_0a,
                        mu_a0=mu_a0
            )
            print(paste(m,d,p,l,x,k))
            res.out <- lsoda(initR, timesR, model, parms)
            
            if(sum(res.out[length(timesR),4:9])<10^-10){ # if infections die out, skip fitting
              fittedRates[m,d,p,l,x,k] <- Inf # to be set to zero in subsequent processing! just to mark how often this happens
            }else{ 
              if(!is.na(which(rowSums(res.out[,4:5])<0)[1])){
                tf <- 1:(which(rowSums(res.out[,4:5])<0)[1]-1)
              }else{
                tf <- 1:length(timesR)
              }
              try(remove(df))
              df <- data.frame(x=res.out[tf,1], y=log(rowSums(res.out[tf,6:9])/rowSums(res.out[tf,4:5])))
              
              if(!is.na(which(df[,2]==Inf)[1])){
                df <- df[-which(df[,2]==Inf),]
              }
              
              
              fit <- lm(y~x, data=df)
              fittedRates[m,d,p,l,x,k] <- fit$coef[2] 
            } # end fitting
            
          } # end xir loop
        } # end scenario loop
      } # end lambda_sigma loop 
    } # end psi loop
  } # end delta loop
} # end parameter loop

save(fittedRates, file=paste("../data/fittedRates_", pop, "_", j, "_", pn, ".data", sep=""))