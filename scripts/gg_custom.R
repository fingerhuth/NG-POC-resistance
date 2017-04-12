# custom

library(ggthemes)
theme_set(theme_bw())

custom <- 
  theme_hc() + 
  theme(
    text = element_text(size = 8, colour="black"),
    line = element_line(size=0.2),
    axis.text=element_text(colour="black"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(colour="gray"),
    axis.title.y=element_text(margin=margin(0,0,0,0)),
    axis.title.x=element_text(margin=margin(5,0,0,0)),
    legend.position ="bottom",
    legend.direction = "horizontal",
    legend.background = element_rect(colour="white", fill="white"),
    legend.key.height = unit(8, "pt"),
    legend.key.width = unit(8, "pt"),
    legend.margin= margin(unit(c(0,0,0,0), "pt")),
    panel.spacing = (unit(0.5, "cm")),
    panel.background = element_rect(colour="white", fill="white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour="gray"),
    plot.margin = unit(c(1,1,1,1), "pt"),
    plot.title=element_text(hjust=0.5),
    plot.background = element_rect(colour="white", fill="white"),
    strip.background=element_rect(colour="white", fill="white")
  )