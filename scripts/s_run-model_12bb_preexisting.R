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


# source files
source("f_model.R")
source("f_mixing.R")

# load libraries
library(deSolve)
library(rootSolve, lib.loc="../../lib/")
# library(rootSolve)

timesR <- seq(0,yearsR,pyearsR)

# use only parameter sets that worked in spread-model
load(paste("../data/outros_", pop, "-", seed, "-", pop, ".data", sep=""))
if(max.outros-(j+1)*(n.sim)<0){
  theta <- outros[(1+j*(n.sim)):max.outros,7:11]
  printh <- matrix(data=NA, nrow=max.outros-j*n.sim, ncol=11)
  resce <- array(data=NA, dim=c(length(timesR), 13, 4, max.outros-j*n.sim))
}else{
  theta <- outros[(1+j*(n.sim)):(j*(n.sim)+(n.sim)),7:11]
  printh <- matrix(data=NA, nrow=n.sim, ncol=11)
  resce <- array(data=NA, dim=c(length(timesR), 13, 4, n.sim))
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

# loss to follow-up parameters: 1=no LTF, 0=all LTF
lambda_sigma <- eff.lambda_sigma
  
# test sensitivities
xi_r <- eff.xi_r

vec.e_1b <- c(0, eff.e_1b, eff.e_1b, eff.e_1b)
vec.delta <- c(eff.delta, eff.delta, eff.delta, 0)
vec.xi_r <- c(eff.xi_r, eff.xi_r, 0, eff.xi_r)
vec.lambda_sigma <- c(eff.lambda_sigma, eff.lambda_sigma, eff.lambda_sigma, 1)

  
for (k in 1:nrow(theta)){ # parameter loop
  epsilon <- theta[k,1]
  betaL <- theta[k,2]
  betaH <- theta[k,3]
  D <- theta[k,4] 
  f <- theta[k,5]
  
  # initialize populations
  init <- c(N[1]*0.995,N[2]*0.90,N[1]*0.005, N[2]*0.1, 0, 0, 0, 0)
  
  # dependent parameters
  tt <- f/D
  zeta <- psi*tt/e_0a
  sigma <- (tt*(1-psi))/(xi_ng*lambda_sigma*e_0a-delta*tt*(1-psi))
  if(sigma < 0){
    next
  }
  mu_ta <- 0
  
  nu <- 1/D - tt
  rho <- mixing(epsilon, noG, pic, N)
  beta <- matrix(c(betaL,sqrt(betaL*betaH),sqrt(betaL*betaH),betaH),nrow=noG,ncol=noG)
  

  # run model
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
  temp.out <- runsteady(init, c(0,years), model, parms)
  out <- unlist(c(attr(temp.out, "time"), temp.out[1], temp.out[2]))
  
  # prevalence & incidence
  printh[k,] <- c(sum(out[c(4,6,8)])/N[1], #prevL
                  sum(out[c(5,7,9)])/N[2], #prevH
                  sum(out[4:9]), #prevT
                  sum(out[c(10,12)])/N[1], #incL
                  sum(out[c(11,13)])/N[2], #incH
                  sum(out[10:13]), #incT
                  theta[k,])
  
  colnames(printh) <- c("prevL", "prevH", "prevT", "incL", "incH", "incT", "epsilon", "betaL", "betaH", "D", "f") # needs change for noG!=2

  initR.tmp <- unlist(temp.out[1])
  initR <- c(initR.tmp[1], initR.tmp[2], initR.tmp[3]*(1-10^-3), initR.tmp[4]*(1-10^-3), initR.tmp[3]*10^-3, initR.tmp[4]*10^-3, 0, 0)
    
  for(m in 2:4){ # scenario loop
    e_1b <- vec.e_1b[m]
    delta <- vec.delta[m]
    xi_r <- vec.xi_r[m]
    lambda_sigma <- vec.lambda_sigma[m]
    
    mu_ta <- 0
    
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
    print(paste(m,k))
#     res <- ode(initR, timesR, model, parms, method="ode45")
    res <- lsoda(initR, timesR, model, parms)
    
    resce[,,m,k] <- res
    
  } # end scenario loop
  
} # end parameter loop

save(printh, file=paste("../data/12_printh_", pop, "_", j, "_", pn, ".data", sep=""))
save(resce, file=paste("../data/12_resce_", pop, "_", j, "_", pn, ".data", sep=""))
