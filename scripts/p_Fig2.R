setwd("~/PhD/ng_poc/repository/NG-POC-resistance/scripts/")

library(ggthemes)
library(ggplot2)
library(grid)
library(gridExtra)

pn <- 129
source(paste("pn_", pn, ".R", sep=""))

i <- 0
for (pop in c("het", "msm")){
  for (scenario in 2:4){
    load(paste("../data/Fig2/12_tss-presT_", pop, "_", scenario, "_", pn,".data", sep=""))
    colnames(tss.presT) <- c("median", "mean", "low50", "up50", "low95", "up95") # lower and upper bound are mixed up

    if (i==0){
      df.tl <- cbind(times=timesR, as.data.frame(tss.presT), group=pop, measurement="pres", scenario=scenario, pn=pn)
      colnames(df.tl) <- c("times", "median", "mean", "low50", "up50", "low95", "up95", "group", "measurement", "scenario", "pn")
      i <- 1
    }else{
      df.tl.temp <- cbind(times=timesR, as.data.frame(tss.presT), group=pop, measurement="pres", scenario=scenario, pn=pn)
      colnames(df.tl.temp) <- c("times", "median", "mean", "low50", "up50", "low95", "up95", "group", "measurement", "scenario", "pn")
      df.tl <- rbind(df.tl, df.tl.temp)
    }
  }             
}

pn <- 130


i <- 0
for (pop in c("het", "msm")){
  for (scenario in 4){
    load(paste("../data/Fig2/12_tss-presT_", pop, "_", scenario, "_", pn,".data", sep=""))
    colnames(tss.presT) <- c("median", "mean", "low50", "up50", "low95", "up95")
    
    if (i==0){
      df.tl2 <- cbind(times=timesR, as.data.frame(tss.presT), group=pop, measurement="pres", scenario=5, pn=pn)
      colnames(df.tl2) <- c("times", "median", "mean", "low50", "up50", "low95", "up95", "group", "measurement", "scenario", "pn")
      i <- 1
    }else{
      df.tl.temp2 <- cbind(times=timesR, as.data.frame(tss.presT), group=pop, measurement="pres", scenario=5, pn=pn)
      colnames(df.tl.temp2) <- c("times", "median", "mean", "low50", "up50", "low95", "up95", "group", "measurement", "scenario", "pn")
      df.tl2 <- rbind(df.tl2, df.tl.temp2)
    }
  }             
}

df.tl <- rbind(df.tl, df.tl2)

col.msm <- "dodgerblue3"
col.het <- "darkolivegreen3"
bg.col <- "white"



# plot timelines
timeline <- function(df.tl, maxtime, title, xlab, ylab, col.het, col.msm){
  df.tl <- df.tl[which(df.tl[,1]<=maxtime),]
  ggplot(df.tl, aes(x=times)) +
    geom_line(aes(x=times, y=0.05*100), linetype="dashed", colour="grey", size=0.75) +
    theme_hc() + 
    theme(text = element_text(
      size = 12,
      colour="black"),
      plot.margin = unit(c(0,0,0,0), "pt"),
      panel.background = element_rect(colour=bg.col, fill=bg.col),
      plot.background = element_rect(colour=bg.col, fill=bg.col),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour="darkgrey"),
      strip.background = element_rect(colour=bg.col, fill=bg.col),
      legend.position ="bottom",
      legend.direction = "horizontal",
      legend.background = element_rect(colour=bg.col, fill=bg.col),
      axis.text = element_text(colour="black")) +
    geom_line(aes(y=median*100, colour=group), alpha=1, size=1) + # plot median
    geom_ribbon(aes(ymin=low50*100, ymax=up50*100, fill=group), alpha=0.45) + # plot 50% area
    geom_ribbon(aes(ymin=low95*100, ymax=up95*100,fill=group), alpha=0.35) + # plot 95% area
    scale_colour_manual(values=c(col.het,col.msm), labels=c("HMW", "MSM"), guide=guide_legend(title="", reverse=T)) + # set colours in legend
    scale_fill_manual(values=c(col.het,col.msm), labels=c("HMW", "MSM"), guide=guide_legend(title="", reverse=T)) + # set fill (colours) in legend
    labs(title=title,
         x=xlab, 
         y=ylab)+ylim(0,100)+
    theme(
      panel.spacing = (unit(0.5, "cm")),
      panel.grid.major.y=element_line(colour="gray"),
      strip.background=element_rect(colour="white", fill="white"),
      axis.ticks=element_blank(),
      axis.title.y=element_text(margin=margin(0,0,0,0)),
      axis.title.x=element_text(margin=margin(10,0,30,0)),
      axis.text=element_text(colour="black")
    )
  
}

pres.all <- which(df.tl$measurement=="pres" & df.tl$scenario!=1)
dfstemp <- df.tl[pres.all,]
dfstemp$scenario <- factor(dfstemp$scenario, levels=c(2,3,5,4))
levels(dfstemp$scenario) <- c("culture", "NAAT", "POC-R", "POC+R")
temp.ppres <- timeline(dfstemp, maxtime=30, title="",
                       xlab=expression("years after resistant gonorrhoea introduced"),
                       ylab=expression(atop("proportion resistant gonorrhoea (in %)")),
                       col.het, col.msm)

pdf(paste("../figures/Fig2_tmp.pdf", sep=""), width=10, height=10, colormodel = "cmyk")
temp.ppres + facet_wrap(~scenario, ncol=1)
dev.off()


# printing proportion resistant after 30 years

df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==2),c(2, 8, 9, 10)]
# Culture MSM
c(round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==2 & df.tl$group=="msm"),"median"]*100, 2),
  round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==2 & df.tl$group=="msm"),"low50"]*100, 2),
  round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==2 & df.tl$group=="msm"),"up50"]*100, 2))
# Culture HMW
c(round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==2 & df.tl$group=="het"),"median"]*100, 2),
  round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==2 & df.tl$group=="het"),"low50"]*100, 2),
  round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==2 & df.tl$group=="het"),"up50"]*100, 2))

df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==4),c(2, 8, 9, 10)]
# POC+R MSM
c(round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==4 & df.tl$group=="msm"),"median"]*100, 2),
  round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==4 & df.tl$group=="msm"),"low50"]*100, 2),
  round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==4 & df.tl$group=="msm"),"up50"]*100, 2))
# POC+R HMW
c(round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==4 & df.tl$group=="het"),"median"]*100, 2),
  round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==4 & df.tl$group=="het"),"low50"]*100, 2),
  round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==4 & df.tl$group=="het"),"up50"]*100, 2))

df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==3),c(2, 8, 9, 10)]
# NAAT MSM
c(round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==3 & df.tl$group=="msm"),"median"]*100, 2),
  round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==3 & df.tl$group=="msm"),"low50"]*100, 2),
  round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==3 & df.tl$group=="msm"),"up50"]*100, 2))
# NAAT HMW
c(round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==3 & df.tl$group=="het"),"median"]*100, 2),
  round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==3 & df.tl$group=="het"),"low50"]*100, 2),
  round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==3 & df.tl$group=="het"),"up50"]*100, 2))

df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==5),c(2, 8, 9, 10)]
# POC-R MSM
c(round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==5 & df.tl$group=="msm"),"median"]*100, 2),
  round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==5 & df.tl$group=="msm"),"low50"]*100, 2),
  round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==5 & df.tl$group=="msm"),"up50"]*100, 2))
# POC-R HMW
c(round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==5 & df.tl$group=="het"),"median"]*100, 2),
  round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==5 & df.tl$group=="het"),"low50"]*100, 2),
  round(df.tl[which(df.tl$times==30 & df.tl$measurement=="pres" & df.tl$scenario==5 & df.tl$group=="het"),"up50"]*100, 2))

# printing time until 5% first exceeded
# POC-R MSM
c(round(df.tl[which(df.tl$median>0.05 & df.tl$measurement=="pres" & df.tl$scenario==5 &df.tl $ group=="msm")[1],1],2),
  round(df.tl[which(df.tl$up50>0.05 & df.tl$measurement=="pres" & df.tl$scenario==5 &df.tl $ group=="msm")[1],1],2),
  round(df.tl[which(df.tl$low50>0.05 & df.tl$measurement=="pres" & df.tl$scenario==5 &df.tl $ group=="msm")[1],1],2))
# POC-R HMW
c(round(df.tl[which(df.tl$median>0.05 & df.tl$measurement=="pres" & df.tl$scenario==5 &df.tl $ group=="het")[1],1],2),
  round(df.tl[which(df.tl$up50>0.05 & df.tl$measurement=="pres" & df.tl$scenario==5 &df.tl $ group=="het")[1],1],2),
  round(df.tl[which(df.tl$low50>0.05 & df.tl$measurement=="pres" & df.tl$scenario==5 &df.tl $ group=="het")[1],1],2))
# NAAT MSM
c(round(df.tl[which(df.tl$median>0.05 & df.tl$measurement=="pres" & df.tl$scenario==3 &df.tl $ group=="msm")[1],1],2),
  round(df.tl[which(df.tl$up50>0.05 & df.tl$measurement=="pres" & df.tl$scenario==3 &df.tl $ group=="msm")[1],1],2),
  round(df.tl[which(df.tl$low50>0.05 & df.tl$measurement=="pres" & df.tl$scenario==3 &df.tl $ group=="msm")[1],1],2))
# NAAT HMW
c(round(df.tl[which(df.tl$median>0.05 & df.tl$measurement=="pres" & df.tl$scenario==3 &df.tl $ group=="het")[1],1],2),
  round(df.tl[which(df.tl$up50>0.05 & df.tl$measurement=="pres" & df.tl$scenario==3 &df.tl $ group=="het")[1],1],2),
  round(df.tl[which(df.tl$low50>0.05 & df.tl$measurement=="pres" & df.tl$scenario==3 &df.tl $ group=="het")[1],1],2))
# culture MSM
c(round(df.tl[which(df.tl$median>0.05 & df.tl$measurement=="pres" & df.tl$scenario==2 &df.tl $ group=="msm")[1],1],2),
  round(df.tl[which(df.tl$up50>0.05 & df.tl$measurement=="pres" & df.tl$scenario==2 &df.tl $ group=="msm")[1],1],2),
  round(df.tl[which(df.tl$low50>0.05 & df.tl$measurement=="pres" & df.tl$scenario==2 &df.tl $ group=="msm")[1],1],2))

