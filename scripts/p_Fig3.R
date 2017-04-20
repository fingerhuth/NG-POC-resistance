setwd("~/PhD/ng_poc/repository/NG-POC-resistance/scripts/")

# lambda_sigma = lambda_A
# lambda_zeta = lambda_S

library(ggplot2)
  library(ggthemes)
  library(grid)
  library(gridExtra)
  library(cowplot)
  # make single data frame AND multiply by 100 000 (for averted cases per 100 000)
  
  theme_set(theme_bw())
  
  obscasesAverted <- data.frame()
  for (pop in c("msm", "het")){
    for (pn in 120:128){
      source(paste("pn_", pn, ".R", sep=""))
      psi <- psi.het
      load(paste("../data/Fig3/12_naat-obscasesAverted_", pop, "_", pn,".data", sep=""))
      obscasesAverted <- as.data.frame(rbind(obscasesAverted, cbind(naat.obscasesAverted[2,5,]*100000, "POC+R  ", pop, eff.lambda_sigma, psi)))
    }
  }
  for (pop in c("msm", "het")){
    for (pn in 188:196){
      source(paste("pn_", pn, ".R", sep=""))
      psi <- psi.het
      load(paste("../data/Fig3/12_naat-obscasesAverted_", pop, "_", pn,".data", sep=""))
      obscasesAverted <- as.data.frame(rbind(obscasesAverted, cbind(naat.obscasesAverted[2,5,]*100000, " POC-R  ", pop, eff.lambda_sigma, psi)))
    }
  }
  for (pop in c("msm", "het")){
    for (pn in 120:128){
      source(paste("pn_", pn, ".R", sep=""))
      psi <- psi.het
      load(paste("../data/Fig3/12_naat-obscasesAverted_", pop, "_", pn,".data", sep=""))
      obscasesAverted <- as.data.frame(rbind(obscasesAverted, cbind(naat.obscasesAverted[1,5,]*100000, "culture", pop, eff.lambda_sigma, psi)))
    }
  }
  
  
  names(obscasesAverted) <- c("outcome", "scenario", "pop", "lambda_sigma", "psi")
  obscasesAverted$outcome <- as.numeric(as.character(obscasesAverted$outcome))
  
  (lev.ls <- as.numeric(levels(obscasesAverted$lambda_sigma))*100)
  (lev.p <- as.numeric(levels(obscasesAverted$psi))*100)
  
  levels(obscasesAverted$lambda_sigma) <- c(expression(lambda["A, baseline"]*" = "*30*"%"),
                                            expression(lambda["A, baseline"]*" = "*60*"%"),
                                            expression(lambda["A, baseline"]*" = "*90*"%"))
  levels(obscasesAverted$psi) <- c(expression(psi*" = "*30*"%"),
                                   expression(psi*" = "*60*"%"),
                                   expression(psi*" = "*90*"%"))
  
  
  
  pl <- list()
  ywhiskers <- rbind(c(Inf, -Inf),c(Inf, -Inf))
  outcome.stats <- array(dim=c(2,3,3,3,5))
  outcome.stats2 <- array(dim=c(2,3,3,3,3))
  outcome.median <- array(dim=c(2,3,3,3))
  outcome.median2 <- array(dim=c(2,3,3,3))
  outcome.mean <- array(dim=c(2,3,3,3))
  
  for (pop in c("msm", "het")){
    if(pop=="msm"){
      bg.col <- "white"
      title <- "MSM"
      i <- 1
      scale1 <- scale_colour_manual(guide_legend(title=""), values=c("dodgerblue4", "dodgerblue3", "deepskyblue")) # set colours and label names in legend
      scale2 <- scale_fill_manual(guide_legend(title=""), values=c("dodgerblue4", "dodgerblue3", "deepskyblue")) # set fill (colours) and label names in legend
    }else if(pop=="het"){
      bg.col <- "white"
      title <- "HMW"
      i <- 2
      scale1 <- scale_colour_manual(guide_legend(title=""), values=c("darkolivegreen4", "darkolivegreen3", "darkolivegreen2")) # set colours and label names in legend
      scale2 <- scale_fill_manual(guide_legend(title=""), values=c("darkolivegreen4", "darkolivegreen3", "darkolivegreen2")) # set fill (colours) and label names in legend
    }else{
      print("Problem with loop.")
    }
    ind <- which(obscasesAverted$pop==pop)
    bp <- ggplot(obscasesAverted[ind,], mapping=aes_string(y = "outcome", x = "scenario"))+
      geom_boxplot(outlier.shape=NA, aes_string(colour="scenario", fill="scenario")) +
      stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })+
      theme_hc() +
      theme(
        text= element_text(size=13),
        panel.spacing = (unit(1, "cm")),
        panel.grid.major.y=element_line(colour="gray"),
        strip.background=element_rect(colour="white", fill="white"),
        axis.ticks=element_blank(),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        strip.text=element_text(size = 12),
        legend.position="none"
      )+
      scale1+
      scale2+
      labs(x="", y="observed cases averted after 5 years\ncompared with NAAT (per 100 000 persons)", title=title)+
      theme(
        panel.spacing = (unit(0.5, "cm")),
        panel.grid.major.y=element_line(colour="gray"),
        strip.background=element_rect(colour="white", fill="white"),
        axis.ticks=element_blank(),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        plot.title = element_text(hjust = 0.5),
        axis.text=element_text(colour="black")
      )
    
    pl[[i]] <- bp + facet_grid(psi ~ lambda_sigma, labeller = label_parsed)
  
    
    # scale y-axis to exclude outliers (http://stackoverflow.com/questions/5677885/ignore-outliers-in-ggplot2-boxplot)
    # compute lower and upper whiskers
    isi <- 0
    for (is in levels(obscasesAverted[ind,]$scenario)){
      isel <- which(obscasesAverted$pop==pop & obscasesAverted$scenario==is)
      isi <- isi + 1
      ipi <- 0
      for(ip in levels(obscasesAverted[ind,]$psi)){
        ipi <- ipi + 1
        ili <- 0
        for(il in levels(obscasesAverted[ind,]$lambda_sigma)){
          ili <- ili + 1
          j <- which(obscasesAverted[isel,]$psi==ip & obscasesAverted[isel,]$lambda_sigma==il)
          outcome.stats[i, isi, ipi, ili,] <- boxplot.stats(obscasesAverted[isel,][j,]$outcome)$stats
          outcome.stats2[i, isi, ipi, ili,] <- quantile((obscasesAverted[isel,][j,]$outcome), c(0.25, 0.5, 0.75))
          outcome.median[i, isi, ipi, ili] <- boxplot.stats(obscasesAverted[isel,][j,]$outcome)$stats[3]
          outcome.median2[i, isi, ipi, ili] <- median(obscasesAverted[isel,][j,]$outcome)
          outcome.mean[i, isi, ipi, ili] <- mean(obscasesAverted[isel,][j,]$outcome)
          whisk.temp <- boxplot.stats(obscasesAverted[isel,][j,]$outcome)$stats[c(1, 5)]
          if(whisk.temp[1]<ywhiskers[i,1]) ywhiskers[i,1] <- whisk.temp[1]
          if(whisk.temp[2]>ywhiskers[i,2]) ywhiskers[i,2] <- whisk.temp[2]
        }
      }
    }
    # scale y limits based on whiskers (for two non-negative ywhisker values)
    pl[[i]] <- pl[[i]] + coord_cartesian(ylim = ywhiskers[i,]*c(0.95, 1.05))
  }
  
  print(plot_grid(pl[[1]], pl[[2]], ncol = 2, nrow = 1))
  ggsave(paste("../figures/Fig3_tmp.pdf", sep=""), colormodel="cmyk", width=14, height=7)
  
  
  all(outcome.median==outcome.median2)

  # i, isi, ipi, ili
  # pop, scenario, psi, lambda_a
  # msm/het (1/2), POC+R/POC-R/culture, 30%/60%/90%, 30%/60%/90%,
  # baseline: [,,2,3]
  round(outcome.median[1,1,2,3]) # msm POC+R
  round(outcome.median[1,2,2,3]) # msm POC-R
  round(outcome.median[1,3,2,3]) # msm culture
  
  round(outcome.median[2,1,2,3]) # het POC+R
  round(outcome.median[2,2,2,3]) # het POC-R
  round(outcome.median[2,3,2,3]) # het culture
  
  
  # i, isi, ipi, ili
  # pop, scenario, psi, lambda_a
  # msm/het (1/2), POC+R/POC-R/culture, 30%/60%/90%, 30%/60%/90%,
  # baseline: [,,2,3]
  round(outcome.stats2[1,1,2,3,]) # msm POC+R
  round(outcome.stats2[1,2,2,3,]) # msm POC-R
  round(outcome.stats2[1,3,2,3,]) # msm culture
  
  round(outcome.stats2[2,1,2,3,]) # het POC+R
  round(outcome.stats2[2,2,2,3,]) # het POC-R
  round(outcome.stats2[2,3,2,3,]) # het culture
