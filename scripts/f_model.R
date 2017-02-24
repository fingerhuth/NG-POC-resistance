# ODE model
model <- function(t, x, parms) {
  with(as.list(parms),{
    dx <- numeric()
    
    # 1: S_low
    # 2: S_high
    # 3: I_Sen_low
    # 4: I_Sen_high
    # 5: I_Res_low
    # 6: I_Res_high
    # 7: W_low
    # 8: W_high

      # prevalences
    dx[1] <- 
      ( 
        - x[1]*pic[1]*sum(rho[1,]*beta[1,]*c(x[3:4], x[5:6], x[7:8])/N) # new infections
        + sum(nu*x[c(3,5,7)]) # spontaneous clearance
        + omega*e_1b*x[7]
        + (1/(1/sigma + delta)*xi_ng*lambda_sigma*e_0a + zeta*e_0a)*(1-mu_ta)*x[3]
        + 1/(1/sigma + delta)*xi_ng*xi_r*lambda_sigma*e_1b*x[5]
        + 1/(1/zeta + delta)*xi_ng*xi_r*e_1b*x[5]
        - alpha*x[1] 
        + sum(alpha*N[1])
        - gamma*x[1]
        + gamma*N[1]*sum(x[1:2])
      )
    dx[2] <- # susceptible
      (       
        - x[2]*pic[2]*sum(rho[2,]*beta[2,]*c(x[3:4], x[5:6], x[7:8])/N) # new infections
        + sum(nu*x[c(4,6,8)]) # spontaneous clearance
        + omega*e_1b*x[8]
        + (1/(1/sigma + delta)*xi_ng*lambda_sigma*e_0a + zeta*e_0a)*(1-mu_ta)*x[4]
        + 1/(1/sigma + delta)*xi_ng*xi_r*lambda_sigma*e_1b*x[6]
        + 1/(1/zeta + delta)*xi_ng*xi_r*e_1b*x[6]
        - alpha*x[2] 
        + sum(alpha*N[2])
        - gamma*x[2]
        + gamma*N[2]*sum(x[1:2])
      )
    dx[3] <- # infected WT
      (
        x[1]*pic[1]*sum(rho[1,]*beta[1,]*x[3:4]/N) 
        - nu*x[3]
        - 1/(1/sigma + delta)*xi_ng*lambda_sigma*e_0a*x[3]
        - zeta*e_0a*x[3]
        - x[3]*mu_0a # treatment-independent mutations leave pop
        + (x[5]+x[7])*mu_a0 # treatment-independent mutations enter pop
        - alpha*x[3]
        - gamma*x[3]
        + gamma*N[1]*sum(x[3:4])
      )
    dx[4] <- # infected WT
      (
        x[2]*pic[2]*sum(rho[2,]*beta[2,]*x[3:4]/N) 
        - nu*x[4]
        - 1/(1/sigma + delta)*xi_ng*lambda_sigma*e_0a*x[4]
        - zeta*e_0a*x[4]
        - x[4]*mu_0a # treatment-independent mutations leave pop
        + (x[6]+x[8])*mu_a0 # treatment-independent mutations enter pop
        - alpha*x[4]
        - gamma*x[4]
        + gamma*N[2]*sum(x[3:4])        
      )
    dx[5] <- # infected A resistant
      (
        x[1]*pic[1]*sum(rho[1,]*beta[1,]*c(x[5:6], x[7:8])/N) 
        + omega*(1-e_1b)*x[7]
        - nu*x[5] 
        + (1/(1/sigma + delta)*xi_ng*lambda_sigma*e_0a + zeta*e_0a)*mu_ta*x[3]
        - 1/(1/sigma + delta)*xi_ng*xi_r*lambda_sigma*e_1b*x[5]
        - 1/(1/zeta + delta)*(xi_ng*xi_r*e_1b + lambda_zeta*(xi_ng*(1-xi_r) + (1-xi_ng)))*x[5]
        + x[3]*mu_0a # treatment-independent mutations enter pop
        - x[5]*mu_a0 # treatment-independent mutations leave pop
        - alpha*x[5]
        - gamma*x[5]
        + gamma*N[1]*sum(x[5:6])
      )
    dx[6] <- # infected A resistant
      (
        x[2]*pic[2]*sum(rho[2,]*beta[2,]*c(x[5:6], x[7:8])/N) 
        + omega*(1-e_1b)*x[8]
        - nu*x[6] 
        + (1/(1/sigma + delta)*xi_ng*lambda_sigma*e_0a + zeta*e_0a)*mu_ta*x[4]
        - 1/(1/sigma + delta)*xi_ng*xi_r*lambda_sigma*e_1b*x[6]
        - 1/(1/zeta + delta)*(xi_ng*xi_r*e_1b + lambda_zeta*(xi_ng*(1-xi_r) + (1-xi_ng)))*x[6]
        + x[4]*mu_0a # treatment-independent mutations enter pop
        - x[6]*mu_a0 # treatment-independent mutations leave pop
        - alpha*x[6]
        - gamma*x[6]
        + gamma*N[2]*sum(x[5:6])
      )
    dx[7] <- # infected A resistant, waiting for right treatment
      (
        1/(1/zeta + delta)*lambda_zeta*(xi_ng*(1-xi_r) + (1-xi_ng))*x[5]
        - nu*x[7] 
        - omega*x[7]
        - x[7]*mu_a0 # treatment-independent mutations leave pop
        - alpha*x[7]
        - gamma*x[7]
        + gamma*N[1]*sum(x[7:8])
      )
    dx[8] <- # infected A resistant, waiting for right treatment
      (
        1/(1/zeta + delta)*lambda_zeta*(xi_ng*(1-xi_r) + (1-xi_ng))*x[6]
        - nu*x[8] 
        - omega*x[8]
        - x[8]*mu_a0 # treatment-independent mutations leave pop
        - alpha*x[8]
        - gamma*x[8]
        + gamma*N[2]*sum(x[7:8])
      )
    list(dx, 
         # incidence; needs manual adjustment if noG changes:
         c(x[1]*pic[1]*sum(rho[1,]*beta[1,]*x[3:4]/N),
           x[2]*pic[2]*sum(rho[2,]*beta[2,]*x[3:4]/N),
           x[1]*pic[1]*sum(rho[1,]*beta[1,]*c(x[5:6], x[7:8])/N),
           x[2]*pic[2]*sum(rho[2,]*beta[2,]*c(x[5:6], x[7:8])/N)
               ))
  })
}