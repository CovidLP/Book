###---------------------------------------------------------------###
###   Building a Platform for Data-Driven Pandemic Prediction:    ###
###   From Data Modelling to Visualization - the CovidLP Project  ###
###                                                               ###
###   Chapter 3: Basic epidemiological features                   ###
###                                                               ###
###                                                               ###
###                                                               ###
###   Author: the CovidLP Team                                    ###
###---------------------------------------------------------------###

rm(list=ls())

###--------Packages
require(PandemicLP)
require(tidyr)
require(dplyr)

length.death = c(150,150,190,135,260)
length.cases = c(150,150,190,110,260)
covid_country_death=covid_country_cases=NULL


countrylist = c("Belgium","Canada","Italy","Japan","South Africa")

for( s in 1:length(countrylist)){
  
  data = load_covid(countrylist[s])
  
  covid_country = data$data[,c(1,4,5)]
  
  covid_country_death = rbind(covid_country_death,
                              covid_country[1:length.death[s],c(1,3)]
  )
  
  
  covid_country_cases = rbind(covid_country_cases,
                              covid_country[1:length.cases[s],c(1,2)]
  )
  
}

covid_country_death = data.frame(names=rep(countrylist,length.death),
                                 covid_country_death)

covid_country_cases = data.frame(names=rep(countrylist,length.cases),
                                 covid_country_cases)

head(covid_country_death)
unique(covid_country_death[,1])
#-----------------------------
# mu -Generalized Logistic
# NOTES: DOWNLOAD Generalized_Logistic FOLDER : https://github.com/CovidLP/Book/tree/main/Part2/Chapter3/data 
#-----------------------------

path.LG = "path of Generalized Logistic folder"
# deaths
setwd(paste0(path.LG,"\\death"))
file_list <- list.files(pattern = ".rds")
mu_country_deaths_LG = NULL
for(i in 1:length(file_list)){
  rds.data = readRDS(file_list[i])
  mu_plot_death = rds.data$mu_plot
  mu_country_deaths_LG = rbind(mu_country_deaths_LG,mu_plot_death[1:length.death[i],])
}

# cases
setwd(paste0(path.LG,"\\cases"))

file_list <- list.files(pattern = ".rds")
mu_country_cases_LG = NULL
for(i in 1:length(file_list)){
  rds.data = readRDS(file_list[i])
  mu_plot_cases = rds.data$mu_plot
  mu_country_cases_LG = rbind(mu_country_cases_LG,mu_plot_cases[1:length.cases[i],])
}



#-----------------------------
# mu - Logistic  
# NOTES: DOWNLOAD LOGISTIC FOLDER : https://github.com/CovidLP/Book/tree/main/Part2/Chapter3/data 
#-----------------------------

path.logistic = "path of Logistic folder"

# deaths
setwd(paste0(path.logistic,"\\death"))
file_list <- list.files(pattern = ".rds")

mu_country_deaths_L = NULL
for(i in 1:length(file_list)){
  rds.data = readRDS(file_list[i])
  mu_plot_death = rds.data$mu_plot
  mu_country_deaths_L = rbind(mu_country_deaths_L,mu_plot_death[1:length.death[i],])
}


# cases
setwd(paste0(path.logistic,"\\cases"))
file_list <- list.files(pattern = ".rds")
mu_country_cases_L = NULL
for(i in 1:length(file_list)){
  rds.data = readRDS(file_list[i])
  mu_plot_cases = rds.data$mu_plot
  mu_country_cases_L = rbind(mu_country_cases_L,mu_plot_cases[1:length.cases[i],])
}


#---------
# Figure
#---------
th = theme(axis.text.y = element_text(size=10),
           axis.text.x = element_text(size=8),
           strip.text = element_text(size=10)) 


Sys.setlocale("LC_TIME", "English")
figure_death =  data.frame(covid_country_death,mu_country_deaths_L$mu,
                           mu_country_deaths_LG$mu) %>% 
  rename(mu.L=mu_country_deaths_L.mu,
         mu.LG=mu_country_deaths_LG.mu) %>%
  ggplot()+
  geom_line(aes(x=date,y=new_deaths),size=0.3,colour="darkgray")+
  geom_point(aes(x=date,y=new_deaths),size=0.3,colour="darkgray")+
  geom_line(aes(x=date,y=mu.L),linetype=2,size=0.3)+
  geom_line(aes(x=date,y=mu.LG),linetype=1,size=0.3)+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "5 weeks")+
  facet_wrap(~names,scales = "free",ncol = 1)+theme_bw()+
  ylab(label = "New deaths per day")+th

figure_cases =  data.frame(covid_country_cases,mu_country_cases_L$mu,
                           mu_country_cases_LG$mu) %>% 
  rename(mu.L=mu_country_cases_L.mu,
         mu.LG=mu_country_cases_LG.mu) %>%
  ggplot()+
  geom_line(aes(x=date,y=new_cases),size=0.3,colour="darkgray")+
  geom_point(aes(x=date,y=new_cases),size=0.3,colour="darkgray")+
  geom_line(aes(x=date,y=mu.L),linetype=2,size=0.3)+
  geom_line(aes(x=date,y=mu.LG),linetype=1,size=0.3)+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "5 weeks")+
  facet_wrap(~names,scales = "free",ncol = 1)+theme_bw()+
  ylab(label = "New cases per day")+th

figure = cowplot::plot_grid(figure_cases,figure_death,ncol = 2)
figure
#---------
# Video
#---------

video_cases = figure_cases+gganimate::transition_reveal(date) 
video_death = figure_death+gganimate::transition_reveal(date)

v_cases = gganimate::animate(video_cases,height = 480, width = 640)
v_death = gganimate::animate(video_death,height = 480, width = 640)

cases_mgif=magick::image_read(v_cases)
death_mgif=magick::image_read(v_death)

new_gif<-magick::image_append(c(cases_mgif[1], death_mgif[1]))
for(i in 2:100){
  combined = magick::image_append(c(cases_mgif[i], death_mgif[i]))
  new_gif=c(new_gif,combined)
}
new_gif
