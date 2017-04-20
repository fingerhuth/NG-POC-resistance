# Generating model flow chart

setwd("~/PhD/ng_poc/repository/NG-POC-resistance/scripts/")

library(diagram)

cols3 <- toupper(c("#d73027",
                   "#fc8d59",
                   "#fee090",
                   "#e0f3f8",
                   "#91bfdb",
                   "#4575b4"))


pdf("../figures/Fig1_tmp.pdf", width=30, height=30)
openplotmat()
elpos <- coordinates(c(1, 1, 2, 4, 4, 8, 8, 8, 8, 8, 1))
elpos[,1] <- elpos[,1]-0.1

ell.radx <- 0.1/2
ell.rady <- 0.035
rect.radx <- 0.1/2
rect.rady <- 0.03
diam.radx <- 0.1/2
diam.rady <- 0.035


dist.outside.no.dot <- 0.071875

dist.outside.no.res <- dist.outside.no.dot*0.75
dist.outside.no.sen <- dist.outside.no.res

length.tree.branch <- 0.045
dist.vertical <- 0.007
dist.horizontal <- 0.007
dist.tree.branch <- length.tree.branch-dist.horizontal

dist.vertical.ng <- 0.02

dist.arrow.x <- 0.0025
dist.arrow.y <- 0.0025

dist.shape.x <- 0.0005
dist.shape.y <- 0.0005

height.no <- 0.035

S.x <- 0.425


arr.length <- 0.35
arr.width <- arr.length

cex.pt <- 1.5

cex.label <- 1.7
cex.box <- 1.7

# resistant figure
elres <- cbind(S.x+1.15*elpos[,1]/2, elpos[,2])
elres[3,2] <- elres[7,2]
elres[20,1] <- elres[12,1]
elres[28,1] <- elres[12,1]
elres[36,1] <- elres[12,1]
elres[44,1] <- elres[12,1]
elres[51,1] <- (elres[34,1]+elres[36,1])/2
elres[24,1] <- elres[23,1] + (elres[24,1]-elres[23,1])/3
elres[32,1] <- elres[24,1]
elres[40,1] <- elres[24,1]
elres[48,1] <- elres[24,1]

# 1st level: I_Res_i (1) with seeking care (2), outside-I_Res to I_Res_i
lines(rbind(elres[1,], elres[2,]))
lines(rbind(c(elres[44,1]+dist.outside.no.dot, elres[1,2]), elres[1,]), lty="dashed")

# 2nd level: seeking care (2) with tree section
lines(rbind(elres[2,], elres[2,]+c(0,-length.tree.branch)))
lines(rbind(elres[2,]+c(0,-length.tree.branch), c(elres[3,1], elres[2,2]-length.tree.branch)))
lines(rbind(elres[2,]+c(0,-length.tree.branch), c(elres[4,1], elres[2,2]-length.tree.branch)))

# 3rd level: tree section with spontaneous recovery (3) and symptoms (4)
lines(rbind(c(elres[4,1], elres[2,2]-length.tree.branch), elres[4,]))
lines(rbind(c(elres[3,1], elres[2,2]-length.tree.branch), c(elres[3,1], elres[7,2])))

# 4th level: symptoms (4) with tree section 
lines(rbind(elres[4,], elres[4,]+c(0,-length.tree.branch)))
lines(rbind(elres[4,]+c(0,-length.tree.branch), c(elres[7,1], elres[4,2]-length.tree.branch)))
lines(rbind(elres[4,]+c(0,-length.tree.branch), c(elres[8,1], elres[4,2]-length.tree.branch)))

# 5th level: tree section with symptomatic (7)/asymptomatic (8) treatment
lines(rbind(c(elres[7,1], elres[4,2]-length.tree.branch), elres[7,]))
lines(rbind(c(elres[8,1], elres[4,2]-length.tree.branch), elres[8,]))

# 6th level: symptomatic (7) to xi_ng (11), asymptomatic (8) to xi_ng (12), bend 0 to spontaneous recovery (3), spontaneous recovery (3) to bend 3
lines(rbind(elres[7,], elres[11,]))
lines(rbind(elres[8,], elres[12,]))
lines(rbind(c(elres[24,1],elres[3,2]), elres[3,]))
lines(rbind(elres[3,], c(elres[3,1],elres[50,2])))

# 7th level: xi_ng (11) to tree section
lines(rbind(elres[11,], elres[11,]+c(0,-length.tree.branch)))
lines(rbind(elres[11,]+c(0,-length.tree.branch), c(elres[25,1], elres[11,2]-length.tree.branch)))
lines(rbind(elres[11,]+c(0,-length.tree.branch), c(elres[18,1], elres[11,2]-length.tree.branch)))

# 8th level: a) tree section to xi_r (18)/ l_s (25)
lines(rbind(c(elres[17,1], elres[11,2]-length.tree.branch), elres[25,]))
lines(rbind(c(elres[18,1], elres[11,2]-length.tree.branch), elres[18,]))

# 8th level: b) xi_ng (12) to xi_r (20), xi_ng (12) to outside-no 1
lines(rbind(elres[12,], elres[20,]))
lines(rbind(elres[12,], elres[12,]+c(dist.outside.no.dot,0)), lty="dashed")

# 9th level: xi_r(18) to meet-up 0 (17), xi_r (18) to efficacious (34), xi_r (20) to l_a (28), xi_r (20) to outside-no 2
lines(rbind(elres[18,], elres[17,]))
lines(rbind(elres[18,], elres[34,]))
lines(rbind(elres[20,], elres[28,]))
lines(rbind(elres[20,], elres[20,]+c(dist.outside.no.dot,0)), lty="dashed")

# 10th level: a) l_s (25) to W_i (24), l_s (25) to meet-up 4 (41)
lines(rbind(elres[25,], elres[24,]))
lines(rbind(elres[25,], elres[41,]), lty="dashed")

# 10th level: b) l_a (28) to efficacious (36), l_a (28) to outside no 3
lines(rbind(elres[28,], elres[36,]))
lines(rbind(elres[28,], elres[28,]+c(dist.outside.no.dot,0)), lty="dashed")

# 10th level: c) W_i (24) to retreatment (32), W_i (24) to bend 0 ()
lines(rbind(elres[24,], elres[32,]))
lines(rbind(elres[24,], c(elres[24,1],elres[3,2])))

# 11th level: a) efficacious (34) to meet-up 1, efficacious (34) to meet-up 2
lines(rbind(elres[34,], c(elres[25,1],elres[34,2])), lty="dashed")
lines(rbind(elres[34,], elres[34,]+c(0,-0.07)))

# 11th level: b) efficacious (36) to bend 1, efficacious (36) to outside-no 4
lines(rbind(elres[36,], elres[36,]+c(0,-0.07)))
lines(rbind(elres[36,], elres[36,]+c(dist.outside.no.dot,0)), lty="dashed")

# 11th level: c) retreatment (32) to efficacious (40)
lines(rbind(elres[32,], elres[40,]))

# 12th level: bend 1 to meet-up 2, meet-up 2 to bend 2 (50)
lines(rbind(elres[34,]+c(0,-0.07), elres[36,]+c(0,-0.07)))
lines(rbind(elres[34,]+c(0,-0.07), elres[50,]))


# 13th level: efficacious (40) to meet-up 3 (48), efficacious (40) to outside-no 5, outside-no 5 to outside-I_Res
lines(rbind(elres[40,], elres[48,]))
lines(rbind(elres[40,], elres[44,]+c(dist.outside.no.dot,0)), lty="dashed")
lines(rbind(elres[44,]+c(dist.outside.no.dot,0), c(elres[44,1]+dist.outside.no.dot, elres[1,2])), lty="dashed")

# 14th level: meet-up 3 (48) to meet-up 5 (49), bent 2 (50) to meet-up 5 (49), bend 3 to meet-up 3
lines(rbind(elres[48,], elres[49,]))
lines(rbind(elres[50,], elres[49,]))
lines(rbind(c(elres[3,1],elres[50,2]), elres[48,]))
lines(rbind(c(elres[3,1], elres[50,2]), c(S.x, elres[45,2])))

## text in shapes
textellipse(elres[1,], radx=ell.radx, rady=ell.rady, lab = expression("individual","with resistant","infection (I"[Res[i]]*")"), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textdiamond(elres[2,], radx=diam.radx, rady=diam.rady, lab = "seeking care?", box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textrect(elres[3,], radx=rect.radx, rady=rect.rady,lab = expression("spontaneous","recovery"), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textdiamond(elres[4,], radx=diam.radx, rady=diam.rady, lab = "symptoms?", box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
# textrect(elres[7,], radx=rect.radx, rady=rect.rady, lab = expression("same-day treatment (rate " *tau[S] *")"," fails; returns after time "*delta), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textrect(elres[7,], radx=rect.radx, rady=rect.rady, lab = expression("same-day treatment","(rate " *tau[S] *") fails;  returns"," after time "*delta), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
# textrect(elres[8,], radx=rect.radx, rady=rect.rady, lab = expression("screening/PN (rate "* tau[A] *"); time "*delta,"after test treatment possible"), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textrect(elres[8,], radx=rect.radx, rady=rect.rady, lab = expression("screening/PN","(rate "* tau[A] *"); treatment","after test possible"," with "*delta*" days delay"), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textdiamond(elres[11,], radx=diam.radx, rady=diam.rady, lab = expression("Gonorrhoea","detected?"), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textdiamond(elres[12,], radx=diam.radx, rady=diam.rady, lab = expression("Gonorrhoea","detected?"), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textdiamond(elres[18,], radx=diam.radx, rady=diam.rady, lab = expression("Resistance","detected?"), box.col = cols3[3], shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textdiamond(elres[20,], radx=diam.radx, rady=diam.rady, lab = expression("Resistance","detected?"), box.col = cols3[3], shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textdiamond(elres[25,], radx=diam.radx, rady=diam.rady, lab = expression("remains","symptomatic?"), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textdiamond(elres[28,], radx=diam.radx, rady=diam.rady, lab = expression(" returns for","treatment?"), box.col = cols3[5], shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textdiamond(elres[34,], radx=diam.radx, rady=diam.rady, lab = expression("treatment","efficacious?"), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textdiamond(elres[36,], radx=diam.radx, rady=diam.rady, lab = expression("treatment","efficacious?"), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textellipse(elres[24,], radx=ell.radx, rady=ell.rady, lab = expression("individual with","resistant infection,","waiting for","retreatment (W"[i]*")"), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textrect(elres[32,], radx=rect.radx, rady=rect.rady, lab = expression("retreatment"), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textdiamond(elres[40,], radx=diam.radx, rady=diam.rady, lab = expression("treatment","efficacious?"), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)


## arrows & labels

# 0st level arrows: I_res
Arrowhead(elres[1,1]+ell.radx+dist.shape.x, elres[1,2], arr.adj=1, angle=180, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# 1st level labels
textplain(mid=c((elres[3,1]+elres[2,1])/2,elres[2,2]-dist.tree.branch), height=0.1, lab="no", cex=cex.label)
textplain(mid=c((elres[4,1]+elres[2,1])/2,elres[2,2]-dist.tree.branch), height=0.1, lab="yes", cex=cex.label)

# 1st level arrows: seeking care (2)
Arrowhead(elres[2,1], elres[2,2]+diam.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# 2nd level labels same-day treatment (7), screening (8)
textplain(mid=c((elres[7,1]+elres[4,1])/2,elres[4,2]-dist.tree.branch), height=0.1, lab="yes", cex=cex.label)
textplain(mid=c((elres[8,1]+elres[4,1])/2,elres[4,2]-dist.tree.branch), height=0.1, lab="no", cex=cex.label)

# 2nd level label: rates same-day treatment (7), screening (8)
textplain(mid=c(elres[7,1]+dist.vertical, (elres[7,2]+elres[11,2])/2), height=0.1, adj=c(0,0.5), lab=expression("rate "*frac(1,1/tau[S] + delta)), cex=cex.label)
textplain(mid=c(elres[8,1]+dist.vertical, (elres[8,2]+elres[12,2])/2), height=0.1, adj=c(0,0.5), lab=expression("rate "*frac(1,1/tau[A] + delta)), cex=cex.label)

# 2nd level arrows: spontaneous recovery (3), symptoms (4)
Arrowhead(elres[3,1], elres[3,2]+rect.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
Arrowhead(elres[4,1], elres[4,2]+diam.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# 3rd level arrows: same-day treatment (7), screening (8)
Arrowhead(elres[7,1], elres[7,2]+rect.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
Arrowhead(elres[8,1], elres[8,2]+rect.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# 3rd level arrwos: spontaneous recovery (3)
Arrowhead(elres[3,1]+rect.radx+dist.shape.x, elres[3,2], arr.adj=1, angle=180, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# 3rd level label: rates spontaneous recovery (3)
textplain(mid=c(elres[3,1]+dist.vertical, (elres[8,2]+elres[12,2])/2), height=0.1, adj=c(0,0.5), lab=expression("rate "*nu), cex=cex.label)

# 3rd level labels
textplain(mid=c(elres[12,1]+dist.outside.no.res,elres[12,2]), height=height.no, adj=c(0.5,0.5), lab=expression("no","("*1-xi[G]*")"), cex=cex.label)
textplain(mid=c(elres[12,1]+dist.vertical, (elres[12,2]+elres[20,2])/2), height=0.1, adj=c(0,0.5), lab=expression("yes ("*xi[G]*")"), cex=cex.label)

# 4th level arrows: xi_ng (11), xi_ng (12)
Arrowhead(elres[11,1], elres[12,2]+diam.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
Arrowhead(elres[12,1], elres[12,2]+diam.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# 4th level labels
textplain(mid=c((elres[11,1]+elres[17,1])/2,elres[11,2]-length.tree.branch), adj=c(0.5,0.5), height=height.no, lab=expression("no","("*1-xi[G]*")"), cex=cex.label)
textplain(mid=c((elres[11,1]+elres[18,1])/2,elres[11,2]-length.tree.branch), adj=c(0.5,0.5), height=height.no, lab=expression("yes","("*xi[G]*")"), cex=cex.label)

# 4th level arrows: outside-no 1
Arrowhead(elres[12,1]+dist.outside.no.dot-dist.arrow.x,elres[12,2], arr.adj=1, angle=0, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
Arrowhead(elres[12,1]+dist.outside.no.dot,elres[12,2]-dist.arrow.y, arr.adj=1, angle=90, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
points(elres[12,1]+dist.outside.no.dot,elres[12,2], pch=21, col="black", bg="white", cex=cex.pt)

# 4th level labels
textplain(mid=c(elres[20,1]+dist.outside.no.res,elres[20,2]), height=height.no, adj=c(0.5,0.5), lab=expression("no","("*1-xi[R]*")"), cex=cex.label)
textplain(mid=c(elres[20,1]+dist.vertical, (elres[20,2]+elres[28,2])/2), height=0.1, adj=c(0,0.5), lab=expression("yes ("*xi[R]*")"), cex=cex.label)

# 5th level arrows: xi_r (18), xi_r (20)
Arrowhead(elres[18,1], elres[18,2]+diam.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
Arrowhead(elres[20,1], elres[20,2]+diam.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# 5th level labels
textplain(mid=c(elres[28,1]+dist.outside.no.res,elres[28,2]), height=height.no, adj=c(0.5,0.5), lab=expression("no","("*1-lambda[A]*")"), cex=cex.label)
textplain(mid=c(elres[28,1]+dist.vertical, (elres[28,2]+elres[36,2])/2), height=0.1, adj=c(0,0.5), lab=expression("yes ("*lambda[A]*")"), cex=cex.label)

# 5th level labels: xi_r (18)
textplain(mid=c((elres[11,1]+elres[17,1])/2, elres[18,2]), height=height.no, adj=c(0.5,0.5), lab=expression("no","("*1-xi[R]*")"), cex=cex.label)
textplain(mid=c(elres[18,1]+dist.vertical, (elres[18,2]+elres[26,2])/2), height=0.1, adj=c(0,0.5), lab=expression("yes ("*xi[R]*")"), cex=cex.label)

# 6th level arrows: outside-no 2
Arrowhead(elres[20,1]+dist.outside.no.dot-dist.arrow.x,elres[20,2], arr.adj=1, angle=0, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
Arrowhead(elres[20,1]+dist.outside.no.dot,elres[20,2]-dist.arrow.y, arr.adj=1, angle=90, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
points(elres[20,1]+dist.outside.no.dot, elres[20,2], pch=21, col="black", bg="white", cex=cex.pt)

# 6th level arrwos: xi_r (20)
Arrowhead(elres[20,1], elres[20,2]+diam.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# 6th level arrows: meet-up 0 
Arrowhead(elres[17,1]+dist.arrow.x,elres[17,2], arr.adj=1, angle=180, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
Arrowhead(elres[17,1],elres[17,2]+dist.arrow.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
points(elres[17,1],elres[17,2], pch=21, col="black", bg="white", cex=cex.pt)

# 6th level labels
textplain(mid=c(elres[36,1]+dist.outside.no.res,elres[36,2]), height=height.no, adj=c(0.5,0.5), lab=expression("no","("*1-eta[2]*")"), cex=cex.label)
textplain(mid=c(elres[36,1]+dist.vertical, (elres[36,2]+elres[44,2])/2), height=0.1, adj=c(0,0.5), lab=expression("yes ("*eta[2]*")"), cex=cex.label)

# 7th level arrows: l_s (25)
Arrowhead(elres[25,1], elres[25,2]+diam.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# 7th level labels: l_s (25)
textplain(mid=c((elres[24,1]+elres[25,1])/2, elres[25,2]), height=height.no, adj=c(0.5,0.5), lab=expression("yes ","("*lambda[S]*")"), cex=cex.label)
textplain(mid=c(elres[25,1]+dist.vertical, (elres[25,2]+elres[33,2])/2), height=0.1, adj=c(0,0.5), lab=expression("no ("*1-lambda[S]*")"), cex=cex.label)

# 7th level arrows: l_a (28)
Arrowhead(elres[28,1], elres[28,2]+diam.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# 7th level arrows: W_i (24)
Arrowhead(elres[24,1]+ell.radx+dist.shape.x, elres[24,2], arr.adj=1, angle=180, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# 7th level arrows: outside-no 3
Arrowhead(elres[28,1]+dist.outside.no.dot-dist.arrow.x,elres[28,2], arr.adj=1, angle=0, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
Arrowhead(elres[28,1]+dist.outside.no.dot,elres[28,2]-dist.arrow.y, arr.adj=1, angle=90, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
points(elres[28,1]+dist.outside.no.dot,elres[28,2], pch=21, col="black", bg="white", cex=cex.pt)

# 8th level arrows: meet-up 1 
Arrowhead(elres[25,1]+dist.arrow.x,elres[34,2], arr.adj=1, angle=180, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
Arrowhead(elres[25,1],elres[34,2]+dist.arrow.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
points(elres[25,1],elres[34,2], pch=21, col="black", bg="white", cex=cex.pt)

# 8th level arrows: retreatment (32)
Arrowhead(elres[32,1], elres[32,2]+rect.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# 8th level label: retreatment (32)
textplain(mid=c(elres[32,1]+dist.vertical, (elres[32,2]+elres[40,2])/2), height=0.1, adj=c(0,0.5), lab=expression("rate "*omega), cex=cex.label)

# 8th level arrows: efficacious (34)
Arrowhead(elres[34,1], elres[34,2]+diam.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# 8th level arrows: efficacious (36)
Arrowhead(elres[36,1], elres[36,2]+diam.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# 8th level labels: efficacious (34)
textplain(mid=c((elres[11,1]+elres[17,1])/2, elres[34,2]), height=height.no, adj=c(0.5,0.5), lab=expression("no","("*1-eta[2]*")"), cex=cex.label)
textplain(mid=c(elres[34,1]+dist.vertical, (elres[36,2]+elres[44,2])/2), height=0.1, adj=c(0,0.5), lab=expression("yes ("*eta[2]*")"), cex=cex.label)

# 7th level arrows: outside-no 4
Arrowhead(elres[36,1]+dist.outside.no.dot-dist.arrow.x,elres[36,2], arr.adj=1, angle=0, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
Arrowhead(elres[36,1]+dist.outside.no.dot,elres[36,2]-dist.arrow.y, arr.adj=1, angle=90, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
points(elres[36,1]+dist.outside.no.dot,elres[36,2], pch=21, col="black", bg="white", cex=cex.pt)

# 9th level arrows: meet-up 2 (elres[34,]+c(0,-0.07)) 
Arrowhead(elres[34,1]+dist.arrow.x,elres[34,2]-0.07, arr.adj=1, angle=180, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
Arrowhead(elres[34,1],elres[34,2]-0.07+dist.arrow.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
points(elres[34,1],elres[34,2]-0.07, pch=21, col="black", bg="white", cex=cex.pt)

# 10th level arrows: meet-up 4 (41)
Arrowhead(elres[41,1]-dist.arrow.x,elres[41,2], arr.adj=1, angle=0, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
Arrowhead(elres[41,1],elres[41,2]+dist.arrow.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
points(elres[41,1],elres[41,2], pch=21, col="black", bg="white", cex=cex.pt)

# 10th level arrows: efficacious (40)
Arrowhead(elres[40,1], elres[40,2]+diam.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# 10th level labels: efficacious (40)
textplain(mid=c((elres[24,1]+elres[25,1])/2, elres[40,2]), height=height.no, adj=c(0.5,0.5), lab=expression("no ","("*1 - eta[2]*")"), cex=cex.label)
textplain(mid=c(elres[40,1]+dist.vertical, (elres[40,2]+elres[48,2])/2), height=0.1, adj=c(0,0.5), lab=expression("yes ("*eta[2]*")"), cex=cex.label)

# 11th level arrows:  meet-up 3 (48)
Arrowhead(elres[48,1]+dist.arrow.x,elres[48,2], arr.adj=1, angle=180, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
Arrowhead(elres[48,1],elres[48,2]+dist.arrow.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
points(elres[48,1],elres[48,2], pch=21, col="black", bg="white", cex=cex.pt)

# 11th level arrows: meet-up 5 (c(elres[3,1],elres[50,2]))
Arrowhead(elres[3,1]+dist.arrow.x,elres[50,2], arr.adj=1, angle=180, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
Arrowhead(elres[3,1],elres[50,2]+dist.arrow.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
points(elres[3,1],elres[50,2], pch=21, col="black", bg="white", cex=cex.pt)

# 12th level arrows: S_i (53)
Arrowhead(S.x+dist.arrow.x, elres[45,2], arr.adj=1, angle=180, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")



elpos <- coordinates(c(1, 1, 2, 4, 4, 4, 4, 4, 4, 1, 1))
elpos[,1] <- elpos[,1]-0.1

# sensitive figure
elsen <- cbind(0.08625-0.0134375+1.15*elpos[,1]/2, elpos[,2])
elsen[4,2] <- elsen[7,2]
elsen[4,1] <- (elsen[4,1]+elsen[6,1])*0.5
elsen[22,1] <- elsen[4,1]
elsen[1,1] <- (elsen[4,1]+elsen[3,1])/2
elsen[2,1] <- elsen[1,1]

# 1st level: I_Sen_i (1) with seeking care (2), outside-I_Sen to I_Sen_i
lines(rbind(elsen[1,], elsen[2,]))
lines(rbind(c(elsen[17,1]-dist.outside.no.dot,elsen[1,2]), elsen[1,]), lty="dashed")

# 2nd level: seeking care (2) with tree section
lines(rbind(elsen[2,], elsen[2,]+c(0,-length.tree.branch)))
lines(rbind(elsen[2,]+c(0,-length.tree.branch), c(elsen[3,1], elsen[2,2]-length.tree.branch)))
lines(rbind(elsen[2,]+c(0,-length.tree.branch), c(elsen[4,1], elsen[2,2]-length.tree.branch)))

# 3rd level: tree section with spontaneous recovery (4) and symptoms (3)
lines(rbind(c(elsen[4,1], elsen[2,2]-length.tree.branch), elsen[4,]))
lines(rbind(c(elsen[3,1], elsen[2,2]-length.tree.branch), elsen[3,]))

# 4th level: symptoms (3) with tree section 
lines(rbind(elsen[3,], elsen[3,]+c(0,-length.tree.branch)))
lines(rbind(elsen[3,]+c(0,-length.tree.branch), c(elsen[5,1], elsen[3,2]-length.tree.branch)))
lines(rbind(elsen[3,]+c(0,-length.tree.branch), c(elsen[6,1], elsen[3,2]-length.tree.branch)))

# 5th level: tree section with symptomatic (6)/asymptomatic (5) treatment
lines(rbind(c(elsen[6,1], elsen[3,2]-length.tree.branch), elsen[6,]))
lines(rbind(c(elsen[5,1], elsen[3,2]-length.tree.branch), elsen[5,]))

# 6th level: symptomatic (6) to bend 1 (18), asymptomatic (5) to xi_ng (9), spontaneous recovery (4) to bend 2 (c(elsen[4,1], elsen[29,2]))
lines(rbind(elsen[6,], elsen[18,]))
lines(rbind(elsen[5,], elsen[9,]))
lines(rbind(elsen[4,], c(elsen[4,1], elsen[29,2])))

# 7th level: b) xi_ng (9) to l_a (13), xi_ng (9) to outside-no 1
lines(rbind(elsen[9,], elsen[13,]))
lines(rbind(elsen[9,], elsen[9,]+c(-dist.outside.no.dot,0)), lty="dashed")

# 8th level: l_a (13) to n_1 (17), l_a to outside-no 2
lines(rbind(elsen[13,], elsen[17,]))
lines(rbind(elsen[13,], elsen[13,]+c(-dist.outside.no.dot,0)), lty="dashed")

# 9th level: bend 1 (18) to n_1 (17), n_1 (17) to meet-up 1 c(21), n_1 (17) to outside-no 3 (17+c(-dist.outside.no.dot,0)), outside-no 3 to outside-I_Sen 
lines(rbind(elsen[18,], elsen[17,]))
lines(rbind(elsen[17,], elsen[21,]))
lines(rbind(elsen[17,], elsen[17,]+c(-dist.outside.no.dot,0)), lty="dashed")
lines(rbind(elsen[17,]+c(-dist.outside.no.dot,0), c(elsen[17,1]-dist.outside.no.dot,elsen[1,2])), lty="dashed")

# 10th level: bend 2 (22) to meet-up 1 (c(elsen[21,1], elres[49,2])), meet-up 1 (24) to S_1 (28)
lines(rbind(c(elsen[21,1], elres[49,2]), elsen[21,]))
lines(rbind(c(elsen[21,1], elres[49,2]), c(S.x, elres[45,2])))

# 11th level: S-junction to S
lines(rbind(c(S.x, elres[45,2]), c(S.x, elres[53,2])))

## text in shapes
textellipse(elsen[1,], radx=ell.radx, rady=ell.rady, lab = expression("individual","with sensitive","infection (I"[Sen[i]]*")"), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textdiamond(elsen[2,], radx=diam.radx, rady=diam.rady, lab = "seeking care?", box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textrect(elsen[4,], radx=rect.radx, rady=rect.rady,lab = expression("spontaneous","recovery"), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textdiamond(elsen[3,], radx=diam.radx, rady=diam.rady, lab = "symptoms?", box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textrect(elsen[6,], radx=rect.radx, rady=rect.rady, lab = expression("same-day","treatment"), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textrect(elsen[5,], radx=rect.radx, rady=rect.rady, lab = expression("screening/PN","(rate "* tau[A] *"); treatment","after test possible"," with "*delta*" days delay"), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textdiamond(elsen[9,], radx=diam.radx, rady=diam.rady, lab = expression("Gonorrhoea","detected?"), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textdiamond(elsen[13,], radx=diam.radx, rady=diam.rady, lab = expression(" returns for","treatment?"), box.col = cols3[5], shadow.col = "none", shadow.size = 0.0, cex=cex.box)
textdiamond(elsen[17,], radx=diam.radx, rady=diam.rady, lab = expression("treatment","efficacious?"), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)



## arrows & labels

# 0st level arrows: I_Sen
Arrowhead(elsen[1,1]-ell.radx-dist.shape.x, elsen[1,2], arr.adj=1, angle=0, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# 1st level labels
textplain(mid=c((elsen[3,1]+elsen[2,1])/2,elsen[2,2]-dist.tree.branch), height=0.1, lab="yes", cex=cex.label)
textplain(mid=c((elsen[4,1]+elsen[2,1])/2,elsen[2,2]-dist.tree.branch), height=0.1, lab="no", cex=cex.label)

# 1st level arrows: seeking care (2)
Arrowhead(elsen[2,1], elsen[2,2]+diam.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# 2nd level labels same-day treatment (6), screening (5)
textplain(mid=c((elsen[6,1]+elsen[3,1])/2,elsen[3,2]-dist.tree.branch), height=0.1, lab="yes", cex=cex.label)
textplain(mid=c((elsen[5,1]+elsen[3,1])/2,elsen[3,2]-dist.tree.branch), height=0.1, lab="no", cex=cex.label)

# 2nd level label: rates same-day treatment (6), screening (5)
textplain(mid=c(elsen[6,1]+dist.vertical, (elsen[6,2]+elsen[10,2])/2), height=0.1, adj=c(0,0.5), lab=expression("rate "*tau[S]), cex=cex.label)
textplain(mid=c(elsen[5,1]+dist.vertical, (elsen[5,2]+elsen[9,2])/2), height=0.1, adj=c(0,0.5), lab=expression("rate "*frac(1,1/tau[A] + delta)), cex=cex.label)

# 2nd level arrows: spontaneous recovery (3), symptoms (4)
Arrowhead(elsen[4,1], elsen[4,2]+rect.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
Arrowhead(elsen[3,1], elsen[3,2]+diam.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# 3rd level arrows: same-day treatment (6), screening (5)
Arrowhead(elsen[6,1], elsen[6,2]+rect.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
Arrowhead(elsen[5,1], elsen[5,2]+rect.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# 3rd level label: rates spontaneous recovery (4)
textplain(mid=c(elsen[4,1]+dist.vertical, (elsen[8,2]+elsen[12,2])/2), height=0.1, adj=c(0,0.5), lab=expression("rate "*nu), cex=cex.label)

# 3rd level labels
textplain(mid=c(elsen[9,1]-dist.outside.no.sen,elsen[9,2]), height=height.no, adj=c(0.5,0.5), lab=expression("no","("*1-xi[G]*")"), cex=cex.label)
textplain(mid=c(elsen[9,1]+dist.vertical, (elsen[9,2]+elsen[13,2])/2), height=0.1, adj=c(0,0.5), lab=expression("yes ("*xi[G]*")"), cex=cex.label)

# 4th level arrows: xi_ng (9)
Arrowhead(elsen[9,1], elsen[9,2]+diam.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# 4th level arrows: outside-no 1
Arrowhead(elsen[9,1]-dist.outside.no.dot+dist.arrow.x,elsen[9,2], arr.adj=1, angle=180, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
Arrowhead(elsen[9,1]-dist.outside.no.dot,elsen[9,2]-dist.arrow.y, arr.adj=1, angle=90, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
points(elsen[9,1]-dist.outside.no.dot,elsen[9,2], pch=21, col="black", bg="white", cex=cex.pt)

# 4th level labels: l_a
textplain(mid=c(elsen[13,1]-dist.outside.no.sen,elsen[13,2]), height=height.no, adj=c(0.5,0.5), lab=expression("no","("*1-lambda[A]*")"), cex=cex.label)
textplain(mid=c(elsen[13,1]+dist.vertical, (elsen[13,2]+elsen[17,2])/2), height=0.1, adj=c(0,0.5), lab=expression("yes ("*lambda[A]*")"), cex=cex.label)

# 5th level arrows: l_a
Arrowhead(elsen[13,1], elsen[13,2]+diam.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# 6th level arrows: outside-no 2
Arrowhead(elsen[13,1]-dist.outside.no.dot+dist.arrow.x,elsen[13,2], arr.adj=1, angle=180, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
Arrowhead(elsen[13,1]-dist.outside.no.dot,elsen[13,2]-dist.arrow.y, arr.adj=1, angle=90, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
points(elsen[13,1]-dist.outside.no.dot,elsen[13,2], pch=21, col="black", bg="white", cex=cex.pt)

# 6th level arrwos: efficacious (17), bend 1 () to efficacious (20)
Arrowhead(elsen[17,1], elsen[17,2]+diam.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
Arrowhead(elsen[17,1]+diam.radx+dist.shape.x, elsen[17,2], arr.adj=1, angle=180, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# 6th level labels: eta_1
textplain(mid=c(elsen[17,1]-dist.outside.no.sen,elsen[17,2]), height=height.no, adj=c(0.5,0.5), lab=expression("no","("*1-eta[1]*")"), cex=cex.label)
textplain(mid=c(elsen[17,1]+dist.vertical, (elsen[17,2]+elsen[21,2])/2), height=0.1, adj=c(0,0.5), lab=expression("yes ("*eta[1]*")"), cex=cex.label)


# 7th level arrows: meet-up 1
Arrowhead(elsen[4,1]-dist.arrow.x,elres[49,2], arr.adj=1, angle=0, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
Arrowhead(elsen[4,1],elres[49,2]+dist.arrow.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")
points(elsen[4,1],elres[49,2], pch=21, col="black", bg="white", cex=cex.pt)
Arrowhead(S.x-dist.arrow.x, elres[45,2], arr.adj=1, angle=0, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")

# point for S-junction
points(S.x, elres[45,2], pch=21, col="black", bg="white", cex=cex.pt)
Arrowhead(S.x, elres[53,2]+ell.rady+dist.shape.y, arr.adj=1, angle=270, arr.length=arr.length, arr.width=arr.width, arr.type="triangle")


# Additional text
textellipse(c(S.x, elres[53,2]), radx=ell.radx, rady=ell.rady, lab = expression("susceptible","individual (S"[i]*")"), box.col = "white", shadow.col = "none", shadow.size = 0.0, cex=cex.box)
dev.off()