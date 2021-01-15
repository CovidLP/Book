## load ggplot2 library
library(ggplot2)

## suppress warnings
options(warn = -1)

#### load data ####
plotData = readRDS("data/Figure_8_1.rds")

#### create plotting List ####
p.list = lapply(levels(plotData$Country.Region), function(i) {
  ggplot(subset(plotData,Country.Region==i), aes(x=date,y=yy)) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "lightgray"))+
    geom_line()+geom_point()+
    geom_line(aes(x=date,y=fit),linetype='dashed') + facet_wrap(~Country.Region,scales = "free_y")+
    ylab("deaths")
})

#### plot Figure 8.1 ####
gridExtra::grid.arrange(p.list[[1]],p.list[[2]],p.list[[6]],p.list[[4]])
