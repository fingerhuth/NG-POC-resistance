setwd("~/PhD/ng_poc/repository/NG-POC-resistance/scripts/")

# load libraries
library(ggplot2)
library(ggthemes)
library(grid)
library(gridExtra)
library(cowplot)

theme_set(theme_bw())

# sensitivity analyses 
# 1 xi_NG: pn 131-139
# 2 xi_R: pn: 140-148
# 3 lambda_sigma: pn 149-157
# 4 lambda_zeta: pn  158-166
# 5 psi: pn 167-175
# 6 delta: pn 176-181
# 7 omega: pn 182-187

fig.no <- 4
for (para in 1:7){
  fig.no <- fig.no + 1
  
  outcome.mean <- array(dim=c(2,2,10))
  # load correct data for parameter (necessary because each value of parameter has own pn)
  if(para==1){
    xlab <- expression("test sensitivity to detect gonorrhoea ("*xi[G]*" in %)")
    
    df <- data.frame()
    for (pop in c("msm", "het")){
      for (pn in c(129, 131:139)){
        source(paste("pn_", pn, ".R", sep=""))
        load(paste("../data/FigS5-11/12_naat-obscasesAverted_", pop, "_", pn,".data", sep=""))
        df <- as.data.frame(rbind(df, cbind(naat.obscasesAverted[2,5,]*100000, "POC+R", pop, xi_ng*100)))
        df <- as.data.frame(rbind(df, cbind(naat.obscasesAverted[1,5,]*100000, "culture", pop, xi_ng*100)))
      }
    }
    
  }else if(para==2){
    outcome.mean <- array(dim=c(2,2,11))
    
    xlab <- expression("test sensitivity to detect resistance against the first-line antibiotic ("*xi["R, POC"]*" or "*xi["R, culture"]*" in %)")
    
    df <- data.frame()
    for (pop in c("msm", "het")){
      for (pn in c(129, 140:148, 130)){
        source(paste("pn_", pn, ".R", sep=""))
        load(paste("../data/FigS5-11/12_naat-obscasesAverted_", pop, "_", pn,".data", sep=""))
        df <- as.data.frame(rbind(df, cbind(naat.obscasesAverted[2,5,]*100000, "POC", pop, eff.xi_r*100)))
        df <- as.data.frame(rbind(df, cbind(naat.obscasesAverted[1,5,]*100000, "culture", pop, eff.xi_r*100)))
      }
    }
    
  }else if(para==3){
    xlab <- expression("fraction of asymptomatic individuals who return for treatment at baseline ("*lambda["A, baseline"]*" in %)")
    
    df <- data.frame()
    for (pop in c("msm", "het")){
      for (pn in c(149, 129, 150:157)){
        source(paste("pn_", pn, ".R", sep=""))
        load(paste("../data/FigS5-11/12_naat-obscasesAverted_", pop, "_", pn,".data", sep=""))
        df <- as.data.frame(rbind(df, cbind(naat.obscasesAverted[2,5,]*100000, "POC+R", pop, eff.lambda_sigma*100)))
        df <- as.data.frame(rbind(df, cbind(naat.obscasesAverted[1,5,]*100000, "culture", pop, eff.lambda_sigma*100)))
      }
    }
    
  }else if(para==4){
    xlab <- expression("fraction of symptomatic individuals who remain symptomatic after failed treatment ("*lambda[S]*" in %)")
    
    df <- data.frame()
    for (pop in c("msm", "het")){
      for (pn in c(158, 129, 159:166)){
        source(paste("pn_", pn, ".R", sep=""))
        load(paste("../data/FigS5-11/12_naat-obscasesAverted_", pop, "_", pn,".data", sep=""))
        df <- as.data.frame(rbind(df, cbind(naat.obscasesAverted[2,5,]*100000, "POC+R", pop, lambda_zeta*100)))
        df <- as.data.frame(rbind(df, cbind(naat.obscasesAverted[1,5,]*100000, "culture", pop, lambda_zeta*100)))
      }
    }
    
  }else if(para==5){
    xlab <- expression("fraction of successfully treated individuals who were symptomatic at baseline ("*psi*" in %)")
    
    df <- data.frame()
    for (pop in c("msm", "het")){
      for (pn in c(167:170, 129, 171:175)){
        source(paste("pn_", pn, ".R", sep=""))
        load(paste("../data/FigS5-11/12_naat-obscasesAverted_", pop, "_", pn,".data", sep=""))
        df <- as.data.frame(rbind(df, cbind(naat.obscasesAverted[2,5,]*100000, "POC+R", pop, psi.het*100)))
        df <- as.data.frame(rbind(df, cbind(naat.obscasesAverted[1,5,]*100000, "culture", pop, psi.het*100)))
      }
    }
    
  }else if(para==6){
    outcome.mean <- array(dim=c(2,2,7))
    
    xlab <- expression("average time after test individuals return for treatment at baseline ("*delta["baseline"]*" in days)")
    
    df <- data.frame()
    for (pop in c("msm", "het")){
      for (pn in c(176:177, 129, 178:181)){
        source(paste("pn_", pn, ".R", sep=""))
        load(paste("../data/FigS5-11/12_naat-obscasesAverted_", pop, "_", pn,".data", sep=""))
        df <- as.data.frame(rbind(df, cbind(naat.obscasesAverted[2,5,]*100000, "POC+R", pop, round(365*eff.delta))))
        df <- as.data.frame(rbind(df, cbind(naat.obscasesAverted[1,5,]*100000, "culture", pop, round(365*eff.delta))))
      }
    }
    
  }else if(para==7){
    outcome.mean <- array(dim=c(2,2,7))
    
    xlab <- expression("average time individuals with resistant gonorrhoea wait for re-treatment ("*1/omega*" in days)")
    
    df <- data.frame()
    for (pop in c("msm", "het")){
      for (pn in c(182:183, 129, 184:187)){
        source(paste("pn_", pn, ".R", sep=""))
        load(paste("../data/FigS5-11/12_naat-obscasesAverted_", pop, "_", pn,".data", sep=""))
        df <- as.data.frame(rbind(df, cbind(naat.obscasesAverted[2,5,]*100000, "POC+R", pop, round(365/omega))))
        df <- as.data.frame(rbind(df, cbind(naat.obscasesAverted[1,5,]*100000, "culture", pop, round(365/omega))))
      }
    }
  }
  
  
  
  names(df) <- c("outcome", "scenario", "pop", "value")
  df$outcome <- as.numeric(as.character(df$outcome))
  levels(df$value) <- paste(as.numeric(levels(df$value)),  sep="")
  
  
  pl <- list()
  ywhiskers <- rbind(c(Inf, -Inf), c(Inf, -Inf))
  
  
  for (pop in c("msm", "het")){
    if(pop=="msm"){
      col.cult <- "dodgerblue2"
      col.poc <- "dodgerblue4"
      col.out <- "dodgerblue3"
      bg.col <- "white"
      title <- "MSM"
      i <- 1
      scale1 <- scale_colour_manual(guide_legend(title=""), values=c("dodgerblue4", "dodgerblue2")) # set colours and label names in legend
      scale2 <- scale_fill_manual(guide_legend(title=""), values=c("dodgerblue4", "dodgerblue2")) # set fill (colours) and label names in legend
    }else if(pop=="het"){
      col.cult <- "darkolivegreen2"
      col.poc <- "darkolivegreen4"
      col.out <- "darkolivegreen3"
      bg.col <- "white"
      title <- "HMW"
      i <- 2
      scale1 <- scale_colour_manual(guide_legend(title=""), values=c("darkolivegreen4", "darkolivegreen2")) # set colours and label names in legend
      scale2 <- scale_fill_manual(guide_legend(title=""), values=c("darkolivegreen4", "darkolivegreen2")) # set fill (colours) and label names in legend
    }else{
      print("Problem with loop.")
    }
    
    ind <- which(df$pop==pop)
    bp <- ggplot(df[ind,], mapping=aes_string(y = "outcome", x = "scenario"))+
      geom_boxplot(outlier.shape=NA, aes_string(colour="scenario", fill="scenario")) +
      stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })+
      theme_hc() +
      theme(
        panel.spacing = (unit(0.3, "cm")),
        panel.grid.major.y=element_line(colour="gray"),
        strip.background=element_rect(colour="white", fill="white"),
        axis.ticks=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(0,5,0,5)),
        axis.text.y=element_text(colour="black"),
        legend.position="bottom",
        legend.margin=margin(0,0,20,0, "pt")
      )+
      scale1+
      scale2+
      labs(x=xlab, y="observed cases averted after 5 years\ncompared with NAAT (per 100 000 persons)", title=title)
    
    # scale y-axis to exclude outliers (http://stackoverflow.com/questions/5677885/ignore-outliers-in-ggplot2-boxplot)
    # compute lower and upper whiskers
    isi <- 0
    for (is in levels(df[ind,]$scenario)){
      isel <- which(df$pop==pop & df$scenario==is)
      isi <- isi + 1
      ipi <- 1
      for(ir in levels(df[ind,]$value)){
        j <- which(df[isel,]$value==ir)
        
        outcome.mean[i, isi, ipi] <- mean(df[isel,][j,]$outcome)
        ipi <- ipi+1
        
        whisk.temp <- boxplot.stats(df[isel,][j,]$outcome)$stats[c(1, 5)]
        if(whisk.temp[1]<ywhiskers[i,1]) ywhiskers[i,1] <- whisk.temp[1]
        if(whisk.temp[2]>ywhiskers[i,2]) ywhiskers[i,2] <- whisk.temp[2]
      }
    }
    
    print(paste(pop, para ,all(outcome.mean[i,1,]>outcome.mean[i,2,])))
    
    yvec <- c()
    if(ywhiskers[i,1]>0){
      yvec[1] <- ywhiskers[i,1]*0.95
    }else{
      yvec[1] <- ywhiskers[i,1]*1.05
    }
    if(ywhiskers[i,2]>0){
      yvec[2] <- ywhiskers[i,2]*1.05
    }else{
      yvec[2] <- ywhiskers[i,2]*0.95
    }

    pl[[i]] <- bp + facet_wrap(~ value, nrow=1, strip.position="bottom") + 
      theme(strip.text.x=element_text(colour="black"),
            plot.title = element_text(hjust = 0.5)) + 
      coord_cartesian(ylim = yvec)
  }
  
  try(print(plot_grid(pl[[1]], pl[[2]], labels=c("A", "B"), ncol = 1, nrow = 2)))
  ggsave(paste("../figures/FigS", fig.no, "_tmp.pdf", sep=""), colormodel="cmyk", width=10, height=10)
  
}
