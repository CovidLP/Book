library(PandemicLP)
library(dplyr)
library(ggplot2)
Sys.setlocale("LC_TIME", "C") # datas em ingl?s
#gr = (sqrt(5)+1)/2
height = 18;width = 35

#### Figure 5.5 ####
us_5.5 = load_covid("US",last_date = "2020-07-10")
saveRDS(us_5.5,"../Livro/supplementary/part2/cap5/data/505_us.rds")
# us_5.5 = readRDS("data/505_us.rds") # Run this to load the data from a file

g = us$data %>%
  ggplot(aes(date,new_cases))+
  geom_line(colour="black",size=1)+theme_bw()+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "4 weeks")+th+
  ylab("New cases per day")
g
# ggsave("US.pdf",plot=g,device="pdf",height = height,
#        width = width)