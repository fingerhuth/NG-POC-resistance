setwd("~/PhD/ng_poc/repository/NG-POC-resistance/scripts/")
library(diagram)

pdf("../figures/FigS1_tmp.pdf", width=7, height=7)
openplotmat()
elpos <- coordinates (c(2, 2, 2))

## migration arrows
# from I_sen
curvedarrow(from=elpos[2,]+c(0, 0.025), to=c(1, elpos[2,2]+0.05), curve= 0.05, segment=c(0.37,0.87), lwd=1, arr.pos=0.87, arr.type="triangle", arr.length=0.2)
curvedarrow(from=elpos[2,]+c(0,-0.025), to=c(1, elpos[2,2]-0.05), curve=-0.05, segment=c(0.37,0.87), lwd=1, arr.pos=0.87, arr.type="triangle", arr.length=0.2)

# from I_Res
curvedarrow(from=elpos[6,]+c(0, 0.025), to=c(1, elpos[6,2]+0.05), curve= 0.05, segment=c(0.37,0.87), lwd=1, arr.pos=0.87, arr.type="triangle", arr.length=0.2)
curvedarrow(from=elpos[6,]+c(0,-0.025), to=c(1, elpos[6,2]-0.05), curve=-0.05, segment=c(0.37,0.87), lwd=1, arr.pos=0.87, arr.type="triangle", arr.length=0.2)

# from I_W
curvedarrow(from=elpos[4,]+c(0, 0.025), to=c(1, elpos[4,2]+0.05), curve= 0.05, segment=c(0.37,0.87), lwd=1, arr.pos=0.87, arr.type="triangle", arr.length=0.2)
curvedarrow(from=elpos[4,]+c(0,-0.025), to=c(1, elpos[4,2]-0.05), curve=-0.05, segment=c(0.37,0.87), lwd=1, arr.pos=0.87, arr.type="triangle", arr.length=0.2)

# from S
curvedarrow(from=elpos[3,]+c(0,0.025), to=c(0, elpos[3,2]+0.3), curve=-0.05, segment=c(0.29,0.83), lwd=1, arr.pos=0.83, arr.type="triangle", arr.length=0.2)
curvedarrow(from=elpos[3,]+c(0,-0.025), to=c(0, elpos[3,2]-0.3), curve=0.05, segment=c(0.29,0.83), lwd=1, arr.pos=0.83, arr.type="triangle", arr.length=0.2)

# to S
curvedarrow(from=c(0, elpos[3,2]-0.05), to=elpos[3,]+c(0,-0.025), curve=-0.05, segment=c(0.13,0.6), lwd=1, arr.pos=0.6, arr.type="triangle", arr.length=0.2)
curvedarrow(from=c(0, elpos[3,2]+0.05), to=elpos[3,]+c(0,0.025), curve=0.05, segment=c(0.13,0.6), lwd=1, arr.pos=0.6, arr.type="triangle", arr.length=0.2)

# to I_Sen
curvedarrow(from=c(1, elpos[2,2]+((elpos[2,2]+elpos[4,2])/2-elpos[4,2])), to=elpos[2,], curve=+0.05, segment=c(0.15,0.6), lwd=1, arr.pos=0.6, arr.type="triangle", arr.length=0.2)

# to I_Res
curvedarrow(from=c(1, elpos[6,2]-((elpos[2,2]+elpos[4,2])/2-elpos[4,2])), to=elpos[6,], curve=-0.05, segment=c(0.15,0.6), lwd=1, arr.pos=0.6, arr.type="triangle", arr.length=0.2)

# to I_W
curvedarrow(from=c(1, (elpos[2,2]+elpos[4,2])/2), to=elpos[4,], curve=+0.05, segment=c(0.15,0.6), lwd=1, arr.pos=0.6, arr.type="triangle", arr.length=0.2)

## compartment change arrows
# arrows to/from I_Sen
curvedarrow(from=elpos[3,], to=elpos[2,], curve=-0.05, segment=c(0.25,0.745), lwd=1, arr.pos=0.745, arr.type="triangle", arr.length=0.2)
curvedarrow(from=elpos[2,], to=elpos[3,], curve=0.2, segment=c(0.2,0.825), lwd=1, arr.pos=0.825, arr.type="triangle", arr.length=0.2)
curvedarrow(from=elpos[2,], to=elpos[3,], curve=0.35, segment=c(0.16,0.875), lwd=1, arr.pos=0.875, arr.type="triangle", arr.length=0.2)
curvedarrow(from=elpos[2,], to=elpos[3,], curve=0.5, segment=c(0.13,0.91), lwd=1, arr.pos=0.91, arr.type="triangle", arr.length=0.2)

# arrows from W
curvedarrow(from=elpos[4,]+c(0, 0.005), to=elpos[3,]+c(0, 0.005),  curve=0.05, segment=c(0.26,0.72), lwd=1, arr.pos=0.72, arr.type="triangle", arr.length=0.2)
curvedarrow(from=elpos[4,]+c(0, -0.005), to=elpos[3,]+c(0, -0.005),  curve=-0.05, segment=c(0.26,0.72), lwd=1, arr.pos=0.72, arr.type="triangle", arr.length=0.2)

# arrows to/from I_Res
curvedarrow(from=elpos[3,], to=elpos[6,], curve=0.05, segment=c(0.25, 0.745), lwd=1, arr.pos=0.745, arr.type="triangle", arr.length=0.2)
curvedarrow(from=elpos[6,], to=elpos[3,], curve=-0.2, segment=c(0.2,0.825), lwd=1, arr.pos=0.825, arr.type="triangle", arr.length=0.2)
curvedarrow(from=elpos[6,], to=elpos[3,], curve=-0.35, segment=c(0.16,0.875), lwd=1, arr.pos=0.875, arr.type="triangle", arr.length=0.2)
curvedarrow(from=elpos[6,], to=elpos[3,], curve=-0.5, segment=c(0.13,0.91), lwd=1, arr.pos=0.91, arr.type="triangle", arr.length=0.2)

# arrows between W and I_Res
curvedarrow(from=elpos[4,], to=elpos[6,], curve=0.25, segment=c(0.32,0.67), lwd=1, arr.pos=0.67, arr.type="triangle", arr.length=0.2)
curvedarrow(from=elpos[6,], to=elpos[4,], curve=0.25, segment=c(0.32,0.67), lwd=1, arr.pos=0.67, arr.type="triangle", arr.length=0.2)


# compartments
textrect(elpos[2,], 0.06, 0.04,lab = expression("I"[Sen[i]]), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex = 1.2)
textrect(elpos[3,], 0.06, 0.04,lab = expression("S"[i]), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex = 1.2)
textrect(elpos[4,], 0.06, 0.04,lab = expression("W"[i]), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex = 1.2)
textrect(elpos[6,], 0.06, 0.04,lab = expression("I"[Res[i]]), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex = 1.2)


# infection terms
textempty(c(0.45, (elpos[3,2]+elpos[5,2])/2), cex=0.8, lab=expression(pi[i]*Sigma[j%in%C]*rho[ij]*beta[ij]*frac(I[Res[j]]+W[j], N[j]))) # resistant
textempty(c(0.45, (elpos[3,2]+elpos[2,2])/2), cex=0.8, lab=expression(pi[i]*Sigma[j%in%C]*rho[ij]*beta[ij]*frac(I[Sen[j]], N[j]))) # sensitive

# spontaneous recovery terms
textempty(c(0.433, (elpos[3,2]+elpos[5,2])/2-0.1), cex=0.8, lab=expression( nu )) # resistant
textempty(c(0.433, (elpos[3,2]+elpos[2,2])/2+0.1), cex=0.8, lab=expression(nu)) # sensitive
textempty(c((elpos[3,1]+elpos[4,1])/2, (elpos[3,2]+elpos[4,2])/2+0.03), cex=0.8, lab=expression(nu)) #

# treatment with symptoms terms
textempty(c(0.417, (elpos[3,2]+elpos[5,2])/2-0.19), cex=0.8, lab=expression(frac(1, frac(1, tau[S]) + delta) * xi[G]*xi[R]*eta[2])) # resistant
textempty(c(0.417, (elpos[3,2]+elpos[2,2])/2+0.19), cex=0.8, lab=expression(tau[S]*eta[1])) # sensitive

# treatment without symptoms terms
textempty(c(0.4, (elpos[3,2]+elpos[5,2])/2-0.28), cex=0.8, lab=expression(frac(1, frac(1, tau[A]) + delta)* xi[G]*xi[R]*lambda[A]*eta[2])) # resistant
textempty(c(0.4, (elpos[3,2]+elpos[2,2])/2+0.28), cex=0.8, lab=expression(frac(1, frac(1, tau[A]) + delta)* xi[G]*lambda[A]*eta[1])) # sensitive

# treatment after waiting
textempty(c((elpos[3,1]+elpos[4,1])/2, (elpos[3,2]+elpos[4,2])/2-0.03), cex=0.8, lab=expression(omega*eta[2]))

# unsuccessful treatment after waiting
textempty(c(elpos[4,1]-0.11, (elpos[4,2]+elpos[6,2])/2), cex=0.8, lab=expression(omega*"("*1-eta[2]*")"))

# moving from I[Res] to W
textempty(c(elpos[4,1]+0.11, (elpos[4,2]+elpos[6,2])/2), cex=0.8, lab=expression(frac(1, frac(1, tau[S]) + delta) * lambda[S] * "("*xi[G] * "("*1-xi[R]*")" *"+"* "("*1-xi[G]*")"*")")) 

# migration in
textempty(c(elpos[3,1]-0.15, elpos[3,2]+0.03), cex=0.8, lab=expression(alpha*N[i]))

# migration out
textempty(c(elpos[3,1]-0.153, (elpos[3,2]+elpos[5,2])/2), cex=0.8, lab=expression(alpha))
textempty(c((elpos[4,1]+1)/2, elpos[4,2]-0.025), cex=0.8, lab=expression(alpha))
textempty(c((elpos[2,1]+1)/2, elpos[2,2]-0.025), cex=0.8, lab=expression(alpha))
textempty(c((elpos[6,1]+1)/2, elpos[6,2]-0.025), cex=0.8, lab=expression(alpha))

# redistribution in
textempty(c(elpos[3,1]-0.15, elpos[3,2]-0.03), cex=0.8, lab=expression(gamma*N[i]*Sigma[j%in%C]*S[j]))
textempty(c(elpos[6,1]+0.15, elpos[6,2]-0.12), cex=0.8, lab=expression(gamma*N[i]*Sigma[j%in%C]*I[Res[j]]))
textempty(c(elpos[2,1]+0.15, elpos[2,2]+0.12), cex=0.8, lab=expression(gamma*N[i]*Sigma[j%in%C]*I[Sen[j]]))
textempty(c(elpos[4,1]+0.15, elpos[4,2]+0.12), cex=0.8, lab=expression(gamma*N[i]*Sigma[j%in%C]*W[j]))

# redistribution out
textempty(c(elpos[3,1]-0.153, (elpos[3,2]+elpos[2,2])/2), cex=0.8, lab=expression(gamma))
textempty(c((elpos[4,1]+1)/2, elpos[4,2]+0.025), cex=0.8, lab=expression(gamma))
textempty(c((elpos[6,1]+1)/2, elpos[6,2]+0.025), cex=0.8, lab=expression(gamma))
textempty(c((elpos[2,1]+1)/2, elpos[2,2]+0.025), cex=0.8, lab=expression(gamma))

dev.off()