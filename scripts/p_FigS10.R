setwd("~/PhD/ng_poc/repository/scripts/")

library(ggthemes)
library(ggplot2)
library(grid)
library(gridExtra)


sn <- c(3, 4, 2, 5)
j <- 0
df <- data.frame()
for (pn in c(198,199,200)){
  if(pn==198){
    sl <- c(3,4)
  }else if(pn==199){
    sl <- 2
  }else if(pn==200){
    sl <- 4
  }
  
  source(paste("pn_", pn, ".R", sep=""))
  
  
  
  i <- 0

    for (scenario in sl){
      j <- j + 1 
      for (pop in c("het", "msm")){
      
      load(paste("../data/FigS10/12_tss-presT_", pop, "_", scenario, "_", pn,".data", sep=""))
      colnames(tss.presT) <- c("median", "mean", "low50", "up50", "low95", "up95") # lower and upper bound are mixed up
      tmp <- cbind(times=timesR, as.data.frame(tss.presT), group=pop, measurement="pres", scenario=sn[j], pn=pn)
      colnames(tmp) <- c("times", "median", "mean", "low50", "up50", "low95", "up95", "group", "measurement", "scenario", "pn")
      df <- rbind(df, tmp)
      
    }             
  }
}
  
  
  


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

pres.all <- which(df$measurement=="pres" & df!=1)
dfstemp <- df[pres.all,]
dfstemp$scenario <- factor(dfstemp$scenario, levels=c(2,3,5,4))
levels(dfstemp$scenario) <- c("culture", "NAAT", "POC-R", "POC+R")
temp.ppres <- timeline(dfstemp, maxtime=30, title="",
                       xlab=expression("years after resistant gonorrhoea introduced"),
                       ylab=expression(atop("proportion resistant gonorrhoea (in %)")),
                       col.het, col.msm)

pdf(paste("../figures/FigS10_tmp.pdf", sep=""), width=10, height=10, colormodel = "cmyk")
temp.ppres + facet_wrap(~scenario, ncol=1)
dev.off()




# printing proportion resistant after 30 years
df[which(df$times==30 & df$measurement=="pres" & df$scenario==2),c(2, 8, 9, 10)]
# Culture MSM
c(round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==2 & df$group=="msm"),"median"]*100, 2),
  round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==2 & df$group=="msm"),"low50"]*100, 2),
  round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==2 & df$group=="msm"),"up50"]*100, 2))
# Culture HMW
c(round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==2 & df$group=="het"),"median"]*100, 2),
  round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==2 & df$group=="het"),"low50"]*100, 2),
  round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==2 & df$group=="het"),"up50"]*100, 2))

df[which(df$times==30 & df$measurement=="pres" & df$scenario==4),c(2, 8, 9, 10)]
# POC+R MSM
c(round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==4 & df$group=="msm"),"median"]*100, 2),
  round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==4 & df$group=="msm"),"low50"]*100, 2),
  round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==4 & df$group=="msm"),"up50"]*100, 2))
# POC+R HMW
c(round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==4 & df$group=="het"),"median"]*100, 2),
  round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==4 & df$group=="het"),"low50"]*100, 2),
  round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==4 & df$group=="het"),"up50"]*100, 2))

df[which(df$times==30 & df$measurement=="pres" & df$scenario==3),c(2, 8, 9, 10)]
# NAAT MSM
c(round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==3 & df$group=="msm"),"median"]*100, 2),
  round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==3 & df$group=="msm"),"low50"]*100, 2),
  round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==3 & df$group=="msm"),"up50"]*100, 2))
# NAAT HMW
c(round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==3 & df$group=="het"),"median"]*100, 2),
  round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==3 & df$group=="het"),"low50"]*100, 2),
  round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==3 & df$group=="het"),"up50"]*100, 2))

df[which(df$times==30 & df$measurement=="pres" & df$scenario==5),c(2, 8, 9, 10)]
# POC-R MSM
c(round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==5 & df$group=="msm"),"median"]*100, 2),
  round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==5 & df$group=="msm"),"low50"]*100, 2),
  round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==5 & df$group=="msm"),"up50"]*100, 2))
# POC-R HMW
c(round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==5 & df$group=="het"),"median"]*100, 2),
  round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==5 & df$group=="het"),"low50"]*100, 2),
  round(df[which(df$times==30 & df$measurement=="pres" & df$scenario==5 & df$group=="het"),"up50"]*100, 2))

# printing time until 5% first exceeded
# culture MSM
c(round(df[which(df$median>0.05 & df$measurement=="pres" & df$scenario==2 &df$group=="msm")[1],1],2),
  round(df[which(df$up50>0.05 & df$measurement=="pres" & df$scenario==2 &df$group=="msm")[1],1],2),
  round(df[which(df$low50>0.05 & df$measurement=="pres" & df$scenario==2 &df$group=="msm")[1],1],2))
# culture HMW
c(round(df[which(df$median>0.05 & df$measurement=="pres" & df$scenario==2 &df$group=="het")[1],1],2),
  round(df[which(df$up50>0.05 & df$measurement=="pres" & df$scenario==2 &df$group=="het")[1],1],2),
  round(df[which(df$low50>0.05 & df$measurement=="pres" & df$scenario==2 &df$group=="het")[1],1],2))
# POC-R MSM
c(round(df[which(df$median>0.05 & df$measurement=="pres" & df$scenario==5 &df$group=="msm")[1],1],2),
  round(df[which(df$up50>0.05 & df$measurement=="pres" & df$scenario==5 &df$group=="msm")[1],1],2),
  round(df[which(df$low50>0.05 & df$measurement=="pres" & df$scenario==5 &df$group=="msm")[1],1],2))
# POC-R HMW
c(round(df[which(df$median>0.05 & df$measurement=="pres" & df$scenario==5 &df$group=="het")[1],1],2),
  round(df[which(df$up50>0.05 & df$measurement=="pres" & df$scenario==5 &df$group=="het")[1],1],2),
  round(df[which(df$low50>0.05 & df$measurement=="pres" & df$scenario==5 &df$group=="het")[1],1],2))
# NAAT MSM
c(round(df[which(df$median>0.05 & df$measurement=="pres" & df$scenario==3 &df$group=="msm")[1],1],2),
  round(df[which(df$up50>0.05 & df$measurement=="pres" & df$scenario==3 &df$group=="msm")[1],1],2),
  round(df[which(df$low50>0.05 & df$measurement=="pres" & df$scenario==3 &df$group=="msm")[1],1],2))
# NAAT HMW
c(round(df[which(df$median>0.05 & df$measurement=="pres" & df$scenario==3 &df$group=="het")[1],1],2),
  round(df[which(df$up50>0.05 & df$measurement=="pres" & df$scenario==3 &df$group=="het")[1],1],2),
  round(df[which(df$low50>0.05 & df$measurement=="pres" & df$scenario==3 &df$group=="het")[1],1],2))

