# Mixing matrix
mixing <- function(epsilon, noG, c, N) {
  cN <- sum(c*N)
  f <- matrix(nrow=noG,ncol=noG)
  for(i in 1:noG)
    for(j in 1:noG) {
      if(i == j) f[i,j] <- epsilon
      else f[i,j] <- 0
    }
  rho <- matrix(nrow=noG,ncol=noG)
  for(i in 1:noG) for(j in 1:noG) rho[i,j] <- f[i,j] + (1.-epsilon)*c[j]*N[j]/cN
  rho
}