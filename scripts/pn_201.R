# This sets sets omega to 1 to see how large the difference between NAAT and CULT/POC is if there is no waiting for alternative treatment.


# set timeline for simulations (years: maximum years to run resistance-free equilibrium)
years <- 500
yearsR <-  100
pyearsR <- 1/12
timesR <- seq(0, yearsR, pyearsR)

# set scenario parameter vectors
# SM=FAIL, CULT, NAAT, POC
eff.e_1b <- 0.99
eff.delta <- NA
eff.xi_r <- NA
eff.lambda_sigma <- NA

e_0a <- 0.99
omega <- 1/(7/365)
lambda_zeta <- 0.9
xi_ng <- 0.99
xi_ng.bl <- 0.9

n.sim <- 1 # simulation batch size
n.tp <- NA  # extract summary statistics per time point batch size

max.outros <- 1000

psi.msm <- NA
psi.het <- NA

# x <- 5


