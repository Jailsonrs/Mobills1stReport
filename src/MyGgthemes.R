tema1 <- theme(
  panel.background = element_rect(fill="grey90"),
  axis.text.x=element_text(angle=0,hjust=1,face=6,size=5),
  legend.position= "none",
  axis.title.y = element_blank(),
  axis.ticks=element_blank(),
  axis.text.y=element_text(colour=c(rep("black",17),"red",rep("black",10))),
  panel.grid.minor.y=element_blank(),
  panel.grid.minor.x=element_blank(),
  panel.grid.major.x=element_line(linetype=2,size=0.3,colour="grey49"),
  panel.grid.major.y=element_line(linetype=2,size=0.3,colour="grey49"))

tema2 <- theme(
  panel.background = element_rect(fill="snow"),
  axis.text.x=element_text(angle=90,hjust=1,face=2,size=9,colour="grey30"),
  axis.text.y=element_text(colour=c(rep("slategray4",17),"red",rep("slategray4",10)),face=2,size=10),
  legend.position= "right",
  axis.ticks.y=element_line(colour="royalblue",size=2),
  axis.ticks.length=unit(.1,"cm"),
  panel.grid.minor.y=element_blank(),
  panel.grid.minor.x=element_blank(),
  text=element_text(family="Georgia"),
  panel.grid.major.x=element_line(linetype=2,size=0.3,colour="white"),
  panel.grid.major.y=element_line(linetype=1,size=0.3,colour="lightyellow2"),
  plot.title = element_text(size = 20),
  plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
  plot.caption = element_text(size = 8, margin = margin(t = 10), color = "slategray4", hjust = 0)
)
