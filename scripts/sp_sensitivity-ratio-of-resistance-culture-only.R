setwd("~/PhD/ng_poc/repository/scripts/")

library(reshape2)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(gtable)
library(cowplot)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# load sensitivity analysis

dvec <- c(1/365, 7/365)
psivec <- c(0.3, 0.6, 0.9)
lsvec <- c(0.3, 0.6, 0.9)
xirvec <- c(0, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99)

d <- 2
df <- c()
pn <- 201
for(pn in c(201, 202)){
  for(pop in c("msm", "het")){
    
    
    if(pop=="msm"){
      popname <- "MSM"
    }else{
      popname <- "HMW"
    }
    
    if(pn==202){  
      load(paste("../data/FigS14/fr_", pop, "_", pn, ".data", sep=""))
      
      ed8 <- fr
      ed8[ed8==Inf] <- NA
      ed8[ed8==-Inf] <- NA
      
      a1 <- array(dim=dim(ed8[4,d,,,,]))
      for(i in 1:8){
        a1[,,i,] <- ed8[2,d,,,8,]
      }
      ned8 <- (ed8[4,d,,,,])/a1
      med8 <- apply(ned8, c(1,2,3), function(x) median(x))
      med8 <- melt(med8)
      led8 <- cbind(med8, popname, "CULT")
      names(led8) <- c( "psi", "lambda_sigma", "xi_r", "rrd", "pop", "test") # rrd = relative rate difference
      
      df <- rbind(df, led8)
      
    }else if(pn==201){
      load(paste("../data/FigS14/fr_", pop, "_", pn, ".data", sep=""))
      
      ed7 <- fr
      ed7[ed7==Inf] <- NA
      ed7[ed7==-Inf] <- NA
      
      a5 <- array(dim=dim(ed7[4,d,,,,]))
      for(i in 1:8){
        a5[,,i,] <- ed7[3,d,,,7,]
      }
      ned7 <- (ed7[4,d,,,,])/a5
      med7 <- apply(ned7, c(1,2,3), function(x) median(x))
      med7 <- melt(med7)
      led7 <- cbind(med7, popname, "NAAT")
      names(led7) <- c( "psi", "lambda_sigma", "xi_r", "rrd", "pop", "test") # rrd = relative rate difference
      
      df <- rbind(df, led7)
    }
    
  }
}

df[,1] <- as.factor(df[,1])
levels(df[,1]) <- paste("fraction treated that is symptomatic: ", as.character(psivec*100), " %", sep="")

df[,2] <- as.factor(df[,2])
levels(df[,2]) <- paste("fraction asymptomatic patients followed up: ", as.character(lsvec*100), " %", sep="")

for(i in 1:length(xirvec)){
  df[which(df[,3]==i),3] <- xirvec[i]*100
}


df_sens <- df
a1_sens <- a1
a5_sens <- a5



# load original

dvec <- c(1/365, 7/365)
psivec <- c(0.3, 0.6, 0.9)
lsvec <- c(0.3, 0.6, 0.9)
xirvec <- c(0, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99)


df <- c()
df9 <- c()
df10 <- c()

pn <- 197
for(pop in c("msm", "het")){
  
  
  if(pop=="msm"){
    popname <- "MSM"
  }else{
    popname <- "HMW"
  }
  
  load(paste("../data/Fig4+5+S4/fr_", pop, "_", pn, ".data", sep=""))
  
  for(d in 2){
    
    
    
    ed8 <- fr
    ed8[ed8==Inf] <- NA
    ed8[ed8==-Inf] <- NA
    
    a1 <- array(dim=dim(ed8[4,d,,,,]))
    for(i in 1:8){
      a1[,,i,] <- ed8[2,d,,,8,]
    }
    ned8 <- (ed8[4,d,,,,])/a1
    med8 <- apply(ned8, c(1,2,3), function(x) median(x))
    med8 <- melt(med8)
    led8 <- cbind(med8, popname, "CULT")
    names(led8) <- c( "psi", "lambda_sigma", "xi_r", "rrd", "pop", "test") # rrd = relative rate difference
    
    
    ed7 <- fr
    ed7[ed7==Inf] <- NA
    ed7[ed7==-Inf] <- NA
    
    a5 <- array(dim=dim(ed7[4,d,,,,]))
    for(i in 1:8){
      a5[,,i,] <- ed7[3,d,,,7,]
    }
    ned7 <- (ed7[4,d,,,,])/a5
    med7 <- apply(ned7, c(1,2,3), function(x) median(x))
    med7 <- melt(med7)
    led7 <- cbind(med7, popname, "NAAT")
    names(led7) <- c( "psi", "lambda_sigma", "xi_r", "rrd", "pop", "test") # rrd = relative rate difference
    
    
    df <- rbind(df, led8, led7)
    
  }
}



df[,1] <- as.factor(df[,1])
levels(df[,1]) <- paste("fraction treated that is symptomatic: ", as.character(psivec*100), " %", sep="")

df[,2] <- as.factor(df[,2])
levels(df[,2]) <- paste("fraction asymptomatic patients followed up: ", as.character(lsvec*100), " %", sep="")

for(i in 1:length(xirvec)){
  df[which(df[,3]==i),3] <- xirvec[i]*100
}




df2 <- rbind(cbind(df_sens, sens="y"),
             cbind(df, sens="n"))


pcult <- ggplot(df2[which(df2$test=="CULT"),], aes(x=xi_r, y=rrd))+
  geom_ribbon(aes(ymin=min(rrd, na.rm=T), ymax=1), fill = cbPalette[5], alpha=0.2)+
  theme_hc()+
  geom_point(aes(col=psi, shape=sens), size=2.5)+
  # geom_line(aes(col=psi, linetype=lambda_sigma), size=0.7)+
  xlab(expression("POC test sensitivity to detect resistance ("*xi["R, POC"]*" in %)"))+
  ylab("ratio of resistance spread between POC and culture")+
  scale_y_log10(breaks=c(1,10,100))+
  theme(strip.background = element_rect(fill = "white"))

pdf("../figures/TMP_cultbaseline-cult.pdf", width=14)
pcult + facet_wrap(lambda_sigma ~ pop)+
  theme(panel.spacing = unit(2, "cm"))
dev.off()

pnaat <- ggplot(df2[which(df2$test=="NAAT"),], aes(x=xi_r, y=rrd))+
  geom_ribbon(aes(ymin=min(rrd, na.rm=T), ymax=1), fill = cbPalette[5], alpha=0.2)+
  theme_hc()+
  geom_point(aes(col=psi, shape=sens), size=2.5)+
  # geom_line(aes(col=psi, linetype=lambda_sigma), size=0.7)+
  xlab(expression("POC test sensitivity to detect resistance ("*xi["R, POC"]*" in %)"))+
  ylab("ratio of resistance spread between POC and culture")+
  scale_y_log10(breaks=c(1,10,100))+
  theme(strip.background = element_rect(fill = "white"))

pdf("../figures/TMP_cultbaseline-naat.pdf", width=14)
pnaat + facet_wrap(lambda_sigma ~ pop)+
  theme(panel.spacing = unit(2, "cm"))
dev.off()

# 
# all(df2[which(df2$test=="NAAT" &df2$sens=="y"),-c(4,7)]==df2[which(df2$test=="NAAT" &df2$sens=="n"),-c(4,7)])
# hist(df2[which(df2$test=="NAAT" &df2$sens=="y"),4]-df2[which(df2$test=="NAAT" &df2$sens=="n"),4])
# 
# max(abs(df2[which(df2$test=="NAAT" &df2$sens=="y"),4]-df2[which(df2$test=="NAAT" &df2$sens=="n"),4]), na.rm=T)
# 
# df2[which(df2[which(df2$test=="NAAT" &df2$sens=="y"),4]-df2[which(df2$test=="NAAT" &df2$sens=="n"),4]==
# max(df2[which(df2$test=="NAAT" &df2$sens=="y"),4]-df2[which(df2$test=="NAAT" &df2$sens=="n"),4], na.rm=T)),]
# 
# 
# all(df2[which(df2$test=="CULT" &df2$sens=="y"),-c(4,7)]==df2[which(df2$test=="CULT" &df2$sens=="n"),-c(4,7)])
# abs(df2[which(df2$test=="CULT" &df2$sens=="y"),4]-df2[which(df2$test=="CULT" &df2$sens=="n"),4])
# max(abs(df2[which(df2$test=="CULT" &df2$sens=="y"),4]-df2[which(df2$test=="CULT" &df2$sens=="n"),4]), na.rm=T)
# 
# hist(a5 - a5_sens)
# hist(a1 - a1_sens)
# 
# min(a5 - a5_sens)
# min(a1 - a1_sens)
# max(a5 - a5_sens)
# max(a1 - a1_sens)
# 
