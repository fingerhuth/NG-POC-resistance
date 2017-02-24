setwd("~/PhD/ng_poc/repository/scripts/")

library(reshape2)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(gtable)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

dvec <- c(1/365, 7/365)
psivec <- c(0.3, 0.6, 0.9)
lsvec <- c(0.3, 0.6, 0.9)
xirvec <- c(0, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99)


df <- c()
pn <- 197
source(paste("pn_", pn, ".R", sep=""))

for(pop in c("msm", "het")){
  if(pop=="msm"){
    popname <- "MSM"
  }else{
    popname <- "HMW"
  }
  load(paste("../data/Fig4+5+S2/fr_", pop, "_", pn, ".data", sep=""))
  
  erad <- array(dim=c(length(psivec), length(lsvec), length(xirvec)))
  for (m in 3){
    for(d in 2){
      for(p in 1:length(psivec)){
        for (l in 1:length(lsvec)){
          for (x in 1:length(xirvec)){
            erad[p, l, x] <- length(which(fr[4,d,p,l,x,]==Inf))/max.outros*100
          }
        }
      }
    }
  }
  
  merr <- melt(erad)
  merr <- cbind(merr, popname)
  names(merr) <- c("psi", "lambda_sigma", "xi_r", "erad", "pop")
  df <- rbind(df, merr)
}


df[,1] <- as.factor(df[,1])
levels(df[,1]) <- paste("fraction treated that is symptomatic: ", as.character(psivec*100), " %", sep="")

df[,2] <- as.factor(df[,2])
levels(df[,2]) <- paste("fraction asymptomatic patients followed up: ", as.character(lsvec*100), " %", sep="")

for(i in 1:length(xirvec)){
  df[which(df[,3]==i),3] <- xirvec[i]*100
}

perad <- ggplot(df, aes(x=xi_r, y=erad))+
  theme_hc()+
  geom_point(aes(col=psi, shape=psi))+
  geom_line(aes(col=psi, linetype=lambda_sigma))+
  xlab(expression("POC test sensitivity to detect resistance ("*xi["R, POC"]*" in %)"))+
  ylab("proportion simulations with eradication (in %)")+
  scale_colour_manual(values=cbPalette[c(1,3,8)], name  =expression("fraction of successfully treated individuals who were symptomatic at baseline ("*psi*" in %)"),
                      labels=c(as.character(psivec*100)))+
  scale_shape_discrete(name  =expression("fraction of successfully treated individuals who were symptomatic at baseline ("*psi*" in %)"),
                       labels=c(as.character(psivec*100)))+
  scale_linetype_discrete(name  =expression("fraction of asymptomatic individuals who return for treatment at baseline ("*lambda["A, baseline"]*" in %)"),
                          labels=c(as.character(lsvec*100)))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(legend.box="vertical")+
  theme(
    panel.spacing = (unit(0.5, "cm")),
    panel.grid.major.y=element_line(colour="gray"),
    strip.background=element_rect(colour="white", fill="white"),
    axis.ticks=element_blank(),
    axis.title.y=element_text(margin=margin(0,10,0,0)),
    axis.title.x=element_text(margin=margin(10,0,30,0)),
    axis.text=element_text(colour="black")
  )

pdf("../figures/FigS2_tmp.pdf",colormodel="cmyk", width=15, height=8)
perad + facet_wrap(~ pop)+
  theme(panel.spacing = unit(2, "cm"))+
  theme(axis.title.y=element_text(vjust=1))+
  theme(axis.title.x=element_text(vjust=-1))+
  theme(text=element_text(size=18))
dev.off()