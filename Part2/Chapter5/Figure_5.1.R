# rm(list=ls())



### packages
library(dplyr)
library(ggplot2)
library(smooth)
library(reshape2)




### Objects
t0 = as.Date("2020-03-01")
t = as.Date("2020-07-31")
obs_states_ma  <- data.frame(date = seq(t0,t,by="day"))
ma_order = 9
states = c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA",
           "PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")



### The data to generate plot in (3) may be:
### (1) downloaded from the "rds" files; or
### (2) downloaded from the John Hopkins repository



### (1) Brazilian states data from John Hopkins repository
baseURLbr = "https://raw.githubusercontent.com/covid19br/covid19br.github.io/master/dados"

covid19states <- read.csv(file.path(baseURLbr,"EstadosCov19.csv"), check.names=FALSE, stringsAsFactors=FALSE) %>%
  rename(state = estado,
         date = data,
         n = casos.acumulados,
         d = obitos.acumulados,
         n_new = novos.casos,
         d_new = obitos.novos) %>%
  mutate(date = as.Date(date)) %>%
  dplyr::select(date, -n, -d, n_new, d_new, state) %>%
  na.omit() %>%
  arrange(state,date) %>% filter(date>=t0,date<=t)

for (u in states) {
  obs_filter = covid19states %>% filter(state==u)
  obs_states_ma[[u]][which(obs_states_ma$date %in% obs_filter$date)] = as.vector(sma(obs_filter$d_new,order=ma_order)$fitted)
}
plot_obs_states_ma = reshape2::melt(obs_states_ma, id.vars='date', variable.name='series')




### (2) Brazilian states data from rds files
data_dir = "Part2/Chapter5/data/"
# data_dir <- paste0("C:/_covid_T4/",t,"/")

for (u in states) {
  data_rds = readRDS(paste0(data_dir,"Brazil_",u,"_de.rds"))
  obs_filter = data_rds$Yobs %>% filter(date>=t0,date<=t)
  obs_states_ma[[u]] = as.vector(sma(obs_filter$d_new,order=ma_order)$fitted)
}
plot_obs_states_ma <- reshape2::melt(obs_states_ma, id.vars='date', variable.name='series')





### (3) Figure 5.1 generate
Sys.setlocale("LC_TIME", "C")
size_plot = 10

ggplot() + ylab("New deaths per day") + xlab("") +  ggtitle("Brazilian states") +
  
  labs(x="Date", y="New deaths per day") + 
  
  scale_x_date(breaks = seq(t0,t,by=21), date_labels="%d/%b/%Y", limits = c(t0,t)) +
  scale_y_continuous(breaks = seq(0, 6e2, by = 1e2)) +
  theme_bw() +

  theme(axis.title.y = element_text(size = size_plot),
        axis.text.y = element_text(size = size_plot),
        axis.text.x = element_text(angle = 90, size = size_plot),
        axis.title.x = element_text(size = size_plot),
        strip.text = element_text(size = size_plot),
        plot.title = element_text(hjust = 0.5, size = size_plot)) +

  geom_line(aes(x=date,y=value,group=series), data=plot_obs_states_ma %>% filter(!series %in% c('CE','SP','PR','SC','RS')), linetype="solid", color="gray70", size=0.5) +
  geom_line(aes(x=date,y=value,group=series), data=plot_obs_states_ma %>% filter(series=="CE"), linetype="solid", color="black", size=1) +
  geom_line(aes(x=date,y=value,group=series), data=plot_obs_states_ma %>% filter(series=="SP"), linetype="dotdash", color="black", size=1) +
  geom_line(aes(x=date,y=value,group=series), data=plot_obs_states_ma %>% filter(series %in% c('PR','SC','RS')), linetype="dashed", color="black", size=1)






