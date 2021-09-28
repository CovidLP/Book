###---------------------------------------------------------------###
###   Building a Platform for Data-Driven Pandemic Prediction:    ###
###   From Data Modelling to Visualization - the CovidLP Project  ###
###                                                               ###
###   Chapter 14: Comparing predictions.                          ###
###   Section 14.3: Analysis for countries                        ###
###   Section 14.4: Improvements from a two-waves modelling       ###
###   Subsection                                                  ###
###   Figure 14.6 - 14.11                                         ###
###                                                               ###
###   Author: the CovidLP Team                                    ###
###---------------------------------------------------------------###

##############################################
###############  Section 14.3  ###############
##############################################

#################################################
########### Figures July 15-28, 2020 ############
#################################################

# Common graph codes
library(tidyverse)
library(ggplot2)
library(plotly)
library(dplyr)
library(readr)
library(htmlwidgets)
library(rsvg)
rm(list=ls())

m <- list(  l = 50,  r = 50,  b = 70,  t = 70,  pad = 1 )
w=550 # width
h=400 # height
t=20  # title size
f=15  # font size
ci='rgb(160,160,160)' # color ihme
lbi='rgb(0,0,0)' # line ball ihme
cc='rgb(96,96,96)' # color covidlp
lbc='rgb(96,96,96)' # color ball covidp
cd='rgb(0, 0, 0)' # color observed data

last.date <- "2020-07-14"
last.date <- as.Date(last.date)
last.date.14 <- as.Date(last.date + 1:15)
jul <-  readRDS("./data_Countries/jul.rds")
load("./data_Countries/data_july.RData")

############################################
# Figure 14.6 (a): Colombia July 15-28, 2020
############################################

y = yCOL
y= y %>% select(2, 3, 8, 9, 10)
colnames(y)=c("local","date", "death", "LI", "LS")
Colombia= y%>% filter(death!=0)
Colombia <- subset(Colombia, date > as.Date(last.date) )
Colombia <- subset(Colombia, date < as.Date(last.date)+15 )
Colombia_d <- readRDS("./data_Countries/file_Colombia.RDS")
Colombia_d <- Colombia_d[["death"]]
Colombia_d <- Colombia_d[["short_term"]]
Colombia_d <- as.data.frame(Colombia_d)
cases <- Colombia_d$median.2020.07.15
q25 <- Colombia_d$q25.2020.07.15
q975 <- Colombia_d$q975.2020.07.15
date <- Colombia_d$median.date
Colombia_LP=data.frame(date,q25,cases,q975)
Colombia_LP<- Colombia_LP[complete.cases(Colombia_LP),]
colnames(Colombia_LP)=c("date", "LI", "death", "LS")
real_Colombia_plot <- subset(jul, c=="Colombia")
real_Colombia_plot<-real_Colombia_plot[1:14,]

fig_Colombia <- Colombia %>% plot_ly() %>% add_trace(x = ~date, y = ~death, type= "scatter", mode = "lines+markers",
                                                     line=list(color=ci, dash='solid', width=2.5),
                                                     marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)), name = "IHME",
                                                     error_y = list(
                                                       type = "data",
                                                       thickness=2,
                                                       width=7,
                                                       color=ci,
                                                       symmetric = FALSE,
                                                       array = c(Colombia$LS - Colombia$death),
                                                       arrayminus = c(Colombia$death - Colombia$LI)))

fig_Colombia <- fig_Colombia %>% add_trace(x = ~Colombia_LP[,1], y = ~Colombia_LP$death, type= "scatter", mode = "lines+markers",
                                           line=list(color=cc, dash='solid', width=2.5),
                                           marker=list(color=cc, size = 7, line = list(color = lbc, width = 1)), name = "CovidLP",
                                           error_y = list(
                                             type = "data",
                                             thickness=2,
                                             width=7,
                                             color=cc,
                                             symmetric = FALSE,
                                             array = c(Colombia_LP$LS - Colombia_LP$death),
                                             arrayminus = c(Colombia_LP$death - Colombia_LP$LI)))

fig_Colombia <- fig_Colombia %>% add_trace(x = ~real_Colombia_plot$date, y = ~real_Colombia_plot$death, type= "scatter", mode = "lines+markers",
                                           line=list(color=cd, dash='solid', width=2.5),
                                           marker=list(color=cd), name = "Observed Data")


fig_Colombia <- fig_Colombia%>% layout(titlefont=list(size=t), autosize = F, width = w, height = h, margin = m, xaxis = list(title = 'Date', type = 'date', tickformat = "%d/%b"), yaxis = list(title = 'Total Deaths', hoverformat = '.0f'), font = list(family = "Arial", size = f),
                                       legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_Colombia <- fig_Colombia %>% plotly::config(
    toImageButtonOptions = list( format = "svg", filename = "Colombia",
    width = 600, height = 400 ) )

fig_Colombia

# Figure 14.6 (b): Colombia error July 15-28, 2020

Colombia_v <- Colombia[1:length(real_Colombia_plot$date),2:3]
Colombia_vlp <- Colombia_LP[1:length(real_Colombia_plot$date),c(1,3)]
erro_Colombia <- cbind(Colombia_v,Colombia_vlp,real_Colombia_plot[,c(5)])
erro_Colombia <- erro_Colombia[,-c(3)]
erro_Colombia <- mutate(erro_Colombia, erro_ihme = (100*abs(death-death.2)/abs(death.2)))
erro_Colombia <- mutate(erro_Colombia, erro_lp = (100*abs(death.1-death.2)/abs(death.2)))

fig_erro_Colombia <- erro_Colombia %>% plot_ly() %>% add_trace(x = ~date, y = ~erro_ihme, type= "bar",
                                                               marker = list(color = ci), name = "IHME")
fig_erro_Colombia <- fig_erro_Colombia %>%add_trace(x = ~date, y = ~erro_lp, type= "bar",
                                                    marker = list(color = cc), name = "CovidLP")
fig_erro_Colombia <- fig_erro_Colombia %>% layout(titlefont=list(size=t), autosize = F, width = w, height = h, margin = m, xaxis = list(title = 'Date', type = 'date', tickformat = "%d/%b"), yaxis = list(title = 'Error', hoverformat = '.2f'), font = list(family = "Arial", size = f),
                                                  legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_erro_Colombia <- fig_erro_Colombia %>%
  plotly::config( toImageButtonOptions = list(
  format = "svg", filename = "Colombia_error",  width = 600,  height = 400 ) )

fig_erro_Colombia

##########################################
# Figure 14.7 (a): Mexico July 15-28, 2020
##########################################

y = yMEX
y= y %>% select(2, 3, 8, 9, 10)
colnames(y)=c("local","date", "death", "LI", "LS")
Mexico= y%>% filter(death!=0)
Mexico <- subset(Mexico, date > as.Date(last.date) )
Mexico <- subset(Mexico, date < as.Date(last.date)+15 )
Mexico_d <- readRDS("./data_Countries/file_Mexico.RDS")
Mexico_d <- Mexico_d[["death"]]
Mexico_d <- Mexico_d[["short_term"]]
Mexico_d <- as.data.frame(Mexico_d)
cases <- Mexico_d$median.2020.07.15
q25 <- Mexico_d$q25.2020.07.15
q975 <- Mexico_d$q975.2020.07.15
date <- Mexico_d$median.date
Mexico_LP=data.frame(date,q25,cases,q975)
Mexico_LP<- Mexico_LP[complete.cases(Mexico_LP),]
colnames(Mexico_LP)=c("date", "LI", "death", "LS")
real_Mexico_plot <- subset(jul, c=="Mexico")
real_Mexico_plot<-real_Mexico_plot[1:14,]

fig_Mexico <- Mexico %>% plot_ly() %>% add_trace(x = ~date, y = ~death, type= "scatter", mode = "lines+markers",
                                                 line=list(color=ci, dash='solid', width=2.5),
                                                 marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)), name = "IHME",
                                                 error_y = list(
                                                   type = "data",
                                                   thickness=2,
                                                   width=7,
                                                   color=ci,
                                                   symmetric = FALSE,
                                                   array = c(Mexico$LS - Mexico$death),
                                                   arrayminus = c(Mexico$death - Mexico$LI)))

fig_Mexico <- fig_Mexico %>% add_trace(x = ~Mexico_LP[,1], y = ~Mexico_LP$death, type= "scatter", mode = "lines+markers",
                                       line=list(color=cc, dash='solid', width=2.5),
                                       marker=list(color=cc, size = 7, line = list(color = lbc, width = 1)), name = "CovidLP",
                                       error_y = list(
                                         type = "data",
                                         thickness=2,
                                         width=7,
                                         color=cc,
                                         symmetric = FALSE,
                                         array = c(Mexico_LP$LS - Mexico_LP$death),
                                         arrayminus = c(Mexico_LP$death - Mexico_LP$LI)))

fig_Mexico <- fig_Mexico %>% add_trace(x = ~real_Mexico_plot$date, y = ~real_Mexico_plot$death, type= "scatter", mode = "lines+markers",
                                       line=list(color=cd, dash='solid', width=2.5),
                                       marker=list(color=cd), name = "Observed Data")

fig_Mexico <- fig_Mexico%>% layout(titlefont=list(size=t), autosize = F, width = w, height = h, margin = m, xaxis = list(title = 'Date', type = 'date', tickformat = "%d/%b"), yaxis = list(title = 'Total Deaths', hoverformat = '.0f'), font = list(family = "Arial", size = f),
                                   legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_Mexico <- fig_Mexico %>%
  plotly::config( toImageButtonOptions = list(
      format = "svg", filename = "Mexico", width = 600,  height = 400 ) )

fig_Mexico

# Figure 14.7 (b): Mexico error July 15-28, 2020

Mexico_v <- Mexico[1:length(real_Mexico_plot$date),2:3]
Mexico_vlp <- Mexico_LP[1:length(real_Mexico_plot$date),c(1,3)]
erro_Mexico <- cbind(Mexico_v,Mexico_vlp,real_Mexico_plot[,c(5)])
erro_Mexico <- erro_Mexico[,-c(3)]
erro_Mexico <- mutate(erro_Mexico, erro_ihme = (100*abs(death-death.2)/abs(death.2)))
erro_Mexico <- mutate(erro_Mexico, erro_lp = (100*abs(death.1-death.2)/abs(death.2)))

fig_erro_Mexico <- erro_Mexico %>% plot_ly() %>% add_trace(x = ~date, y = ~erro_ihme, type= "bar",
                                                           marker = list(color = ci), name = "IHME")
fig_erro_Mexico <- fig_erro_Mexico %>%add_trace(x = ~date, y = ~erro_lp, type= "bar",
                                                marker = list(color = cc), name = "CovidLP")
fig_erro_Mexico <- fig_erro_Mexico %>% layout(titlefont=list(size=t), autosize = F, width = w, height = h, margin = m, xaxis = list(title = 'Date', type = 'date', tickformat = "%d/%b"), yaxis = list(title = 'Error', hoverformat = '.2f'), font = list(family = "Arial", size = f),
                                              legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_erro_Mexico <- fig_erro_Mexico %>% plotly::config(
    toImageButtonOptions = list(format = "svg",
      filename = "Mexico_error",  width = 600,  height = 400  )  )

fig_erro_Mexico

##########################################
# Figure 14.8 (b): Turkey July 15-28, 2020
##########################################

y = yTUR
y= y %>% select(2, 3, 8, 9, 10)
colnames(y)=c("local","date", "death", "LI", "LS")
Turkey= y%>% filter(death!=0)
Turkey <- subset(Turkey, date > as.Date(last.date) )
Turkey <- subset(Turkey, date < as.Date(last.date)+15 )
Turkey_d <- readRDS("./data_Countries/file_Turkey.RDS")
Turkey_d <- Turkey_d[["death"]]
Turkey_d <- Turkey_d[["short_term"]]
Turkey_d <- as.data.frame(Turkey_d)
cases <- Turkey_d$median.2020.07.15
q25 <- Turkey_d$q25.2020.07.15
q975 <- Turkey_d$q975.2020.07.15
date <- Turkey_d$median.date
Turkey_LP=data.frame(date,q25,cases,q975)
Turkey_LP<- Turkey_LP[complete.cases(Turkey_LP),]
colnames(Turkey_LP)=c("date", "LI", "death", "LS")
real_Turkey_plot <- subset(jul, c=="Turkey")
real_Turkey_plot<-real_Turkey_plot[1:14,]

fig_Turkey <- Turkey %>% plot_ly() %>% add_trace(x = ~date, y = ~death, type= "scatter", mode = "lines+markers",
                                                 line=list(color=ci, dash='solid', width=2.5),
                                                 marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)), name = "IHME",
                                                 error_y = list(
                                                   type = "data",
                                                   thickness=2,
                                                   width=7,
                                                   color=ci,
                                                   symmetric = FALSE,
                                                   array = c(Turkey$LS - Turkey$death),
                                                   arrayminus = c(Turkey$death - Turkey$LI)))

fig_Turkey <- fig_Turkey %>% add_trace(x = ~Turkey_LP[,1], y = ~Turkey_LP$death, type= "scatter", mode = "lines+markers",
                                       line=list(color=cc, dash='solid', width=2.5),
                                       marker=list(color=cc, size = 7, line = list(color = lbc, width = 1)), name = "CovidLP",
                                       error_y = list(
                                         type = "data",
                                         thickness=2,
                                         width=7,
                                         color=cc,
                                         symmetric = FALSE,
                                         array = c(Turkey_LP$LS - Turkey_LP$death),
                                         arrayminus = c(Turkey_LP$death - Turkey_LP$LI)))

fig_Turkey <- fig_Turkey %>% add_trace(x = ~real_Turkey_plot$date, y = ~real_Turkey_plot$death, type= "scatter", mode = "lines+markers",
                                       line=list(color=cd, dash='solid', width=2.5),
                                       marker=list(color=cd), name = "Observed Data")


fig_Turkey <- fig_Turkey%>% layout(titlefont=list(size=t), autosize = F, width = w, height = h, margin = m, xaxis = list(title = 'Date', type = 'date', tickformat = "%d/%b"), yaxis = list(title = 'Total Deaths', hoverformat = '.0f'), font = list(family = "Arial", size = f),
                                   legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_Turkey <- fig_Turkey %>% plotly::config(
    toImageButtonOptions = list( format = "svg",
      filename = "Turkey",  width = 600,  height = 400 ) )

fig_Turkey

# Figure 14.8 (b): Turkey error July 15-28, 2020

Turkey_v <- Turkey[1:length(real_Turkey_plot$date),2:3]
Turkey_vlp <- Turkey_LP[1:length(real_Turkey_plot$date),c(1,3)]
erro_Turkey <- cbind(Turkey_v,Turkey_vlp,real_Turkey_plot[,c(5)])
erro_Turkey <- erro_Turkey[,-c(3)]
erro_Turkey <- mutate(erro_Turkey, erro_ihme = (100*abs(death-death.2)/abs(death.2)))
erro_Turkey <- mutate(erro_Turkey, erro_lp = (100*abs(death.1-death.2)/abs(death.2)))

fig_erro_Turkey <- erro_Turkey %>% plot_ly() %>% add_trace(x = ~date, y = ~erro_ihme, type= "bar",
                                                           marker = list(color = ci), name = "IHME")
fig_erro_Turkey <- fig_erro_Turkey %>%add_trace(x = ~date, y = ~erro_lp, type= "bar",
                                                marker = list(color = cc), name = "CovidLP")
fig_erro_Turkey <- fig_erro_Turkey %>% layout(titlefont=list(size=t), autosize = F, width = w, height = h, margin = m, xaxis = list(title = 'Date', type = 'date', tickformat = "%d/%b"), yaxis = list(title = 'Error', hoverformat = '.2f'), font = list(family = "Arial", size = f),
                                              legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_erro_Turkey <- fig_erro_Turkey %>%
  plotly::config( toImageButtonOptions = list(
      format = "svg", filename = "Turkey_error", width = 600, height = 400) )

fig_erro_Turkey

#############################################
# Figure 14.9 (b): Argentina July 15-28, 2020
#############################################

y = yARG
y= y %>% select(2, 3, 8, 9, 10)
colnames(y)=c("local","date", "death", "LI", "LS")
Argentina= y%>% filter(death!=0)
Argentina <- subset(Argentina, date > as.Date(last.date) )
Argentina <- subset(Argentina, date < as.Date(last.date)+15 )
Argentina_d <- readRDS("./data_Countries/file_Argentina.RDS")
Argentina_d <- Argentina_d[["death"]]
Argentina_d <- Argentina_d[["short_term"]]
Argentina_d <- as.data.frame(Argentina_d)
cases <- Argentina_d$median.2020.07.15
q25 <- Argentina_d$q25.2020.07.15
q975 <- Argentina_d$q975.2020.07.15
date <- Argentina_d$median.date
Argentina_LP=data.frame(date,q25,cases,q975)
Argentina_LP<- Argentina_LP[complete.cases(Argentina_LP),]
colnames(Argentina_LP)=c("date", "LI", "death", "LS")
real_Argentina_plot <- subset(jul, c=="Argentina")
real_Argentina_plot<-real_Argentina_plot[1:14,]

fig_Argentina <- Argentina %>% plot_ly() %>% add_trace(x = ~date, y = ~death, type= "scatter", mode = "lines+markers",
                                                       line=list(color=ci, dash='solid', width=2.5),
                                                       marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)), name = "IHME",
                                                       error_y = list(
                                                         type = "data",
                                                         thickness=2,
                                                         width=7,
                                                         color=ci,
                                                         symmetric = FALSE,
                                                         array = c(Argentina$LS - Argentina$death),
                                                         arrayminus = c(Argentina$death - Argentina$LI)))

fig_Argentina <- fig_Argentina %>% add_trace(x = ~Argentina_LP[,1], y = ~Argentina_LP$death, type= "scatter", mode = "lines+markers",
                                             line=list(color=cc, dash='solid', width=2.5),
                                             marker=list(color=cc, size = 7, line = list(color = lbc, width = 1)), name = "CovidLP",
                                             error_y = list(
                                               type = "data",
                                               thickness=2,
                                               width=7,
                                               color=cc,
                                               symmetric = FALSE,
                                               array = c(Argentina_LP$LS - Argentina_LP$death),
                                               arrayminus = c(Argentina_LP$death - Argentina_LP$LI)))

fig_Argentina <- fig_Argentina %>% add_trace(x = ~real_Argentina_plot$date, y = ~real_Argentina_plot$death, type= "scatter", mode = "lines+markers",
                                             line=list(color=cd, dash='solid', width=2.5),
                                             marker=list(color=cd), name = "Observed Data")

fig_Argentina <- fig_Argentina%>% layout(titlefont=list(size=t), autosize = F, width = w, height = h, margin = m, xaxis = list(title = 'Date', type = 'date', tickformat = "%d/%b"), yaxis = list(title = 'Total Deaths', hoverformat = '.0f'), font = list(family = "Arial", size = f),
                                         legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_Argentina <- fig_Argentina %>%
  plotly::config( toImageButtonOptions = list(
      format = "svg", filename = "Argentina", width = 600,height = 400 ) )

fig_Argentina 

# Figure 14.9 (b): Argentina error July 15-28, 2020

Argentina_v <- Argentina[1:length(real_Argentina_plot$date),2:3]
Argentina_vlp <- Argentina_LP[1:length(real_Argentina_plot$date),c(1,3)]
erro_Argentina <- cbind(Argentina_v,Argentina_vlp,real_Argentina_plot[,c(5)])
erro_Argentina <- erro_Argentina[,-c(3)]
erro_Argentina <- mutate(erro_Argentina, erro_ihme = (100*abs(death-death.2)/abs(death.2)))
erro_Argentina <- mutate(erro_Argentina, erro_lp = (100*abs(death.1-death.2)/abs(death.2)))

fig_erro_Argentina <- erro_Argentina %>% plot_ly() %>% add_trace(x = ~date, y = ~erro_ihme, type= "bar",
                                                                 marker = list(color = ci), name = "IHME")
fig_erro_Argentina <- fig_erro_Argentina %>%add_trace(x = ~date, y = ~erro_lp, type= "bar",
                                                      marker = list(color = cc), name = "CovidLP")
fig_erro_Argentina <- fig_erro_Argentina %>% layout(titlefont=list(size=t), autosize = F, width = w, height = h, margin = m, xaxis = list(title = 'Date', type = 'date', tickformat = "%d/%b"), yaxis = list(title = 'Error', hoverformat = '.2f'), font = list(family = "Arial", size = f),
                                                    legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_erro_Argentina <- fig_erro_Argentina %>%
  plotly::config( toImageButtonOptions = list(
  format = "svg", filename = "Argentina_error", width = 600, height = 400 ) )

fig_erro_Argentina

###########################################
# Figure 14.10 (a): Brazil July 15-28, 2020
###########################################

y = yBRA
y= y %>% select(2, 3, 8, 9, 10)
colnames(y)=c("local","date", "death", "LI", "LS")
Brazil= y%>% filter(death!=0)
Brazil <- subset(Brazil, date > as.Date(last.date) )
Brazil <- subset(Brazil, date < as.Date(last.date)+15 )
Brazil_d <- readRDS("./data_Countries/file_Brazil.RDS")
Brazil_d <- Brazil_d[["death"]]
Brazil_d <- Brazil_d[["short_term"]]
Brazil_d <- as.data.frame(Brazil_d)
cases <- Brazil_d$median.2020.07.15
q25 <- Brazil_d$q25.2020.07.15
q975 <- Brazil_d$q975.2020.07.15
date <- Brazil_d$median.date
Brazil_LP=data.frame(date,q25,cases,q975)
Brazil_LP<- Brazil_LP[complete.cases(Brazil_LP),]
colnames(Brazil_LP)=c("date", "LI", "death", "LS")
real_Brazil_plot <- subset(jul, c=="Brazil")
real_Brazil_plot<-real_Brazil_plot[1:14,]

fig_Brazil <- Brazil %>% plot_ly() %>% add_trace(x = ~date, y = ~death, type= "scatter", mode = "lines+markers",
                                                 line=list(color=ci, dash='solid', width=2.5),
                                                 marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)), name = "IHME",
                                                 error_y = list(
                                                   type = "data",
                                                   thickness=2,
                                                   width=7,
                                                   color=ci,
                                                   symmetric = FALSE,
                                                   array = c(Brazil$LS - Brazil$death),
                                                   arrayminus = c(Brazil$death - Brazil$LI)))

fig_Brazil <- fig_Brazil %>% add_trace(x = ~Brazil_LP[,1], y = ~Brazil_LP$death, type= "scatter", mode = "lines+markers",
                                       line=list(color=cc, dash='solid', width=2.5),
                                       marker=list(color=cc, size = 7, line = list(color = lbc, width = 1)), name = "CovidLP",
                                       error_y = list(
                                         type = "data",
                                         thickness=2,
                                         width=7,
                                         color=cc,
                                         symmetric = FALSE,
                                         array = c(Brazil_LP$LS - Brazil_LP$death),
                                         arrayminus = c(Brazil_LP$death - Brazil_LP$LI)))

fig_Brazil <- fig_Brazil %>% add_trace(x = ~real_Brazil_plot$date, y = ~real_Brazil_plot$death, type= "scatter", mode = "lines+markers",
                                       line=list(color=cd, dash='solid', width=2.5),
                                       marker=list(color=cd), name = "Observed Data")

fig_Brazil <- fig_Brazil%>% layout(titlefont=list(size=t), autosize = F, width = w, height = h, margin = m, xaxis = list(title = 'Date', type = 'date', tickformat = "%d/%b"), yaxis = list(title = 'Total Deaths', hoverformat = '.0f'), font = list(family = "Arial", size = f),
                                   legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_Brazil <- fig_Brazil%>%
  plotly::config( toImageButtonOptions = list(
  format = "svg", filename = "Brazil", width = 600,height = 400 ) )

fig_Brazil

# Figure 14.10 (b): Brazil error July 15-28, 2020

Brazil_v <- Brazil[1:length(real_Brazil_plot$date),2:3]
Brazil_vlp <- Brazil_LP[1:length(real_Brazil_plot$date),c(1,3)]
erro_Brazil <- cbind(Brazil_v,Brazil_vlp,real_Brazil_plot[,c(5)])
erro_Brazil <- erro_Brazil[,-c(3)]
erro_Brazil <- mutate(erro_Brazil, erro_ihme = (100*abs(death-death.2)/abs(death.2)))
erro_Brazil <- mutate(erro_Brazil, erro_lp = (100*abs(death.1-death.2)/abs(death.2)))

fig_erro_br <- erro_Brazil %>% plot_ly() %>% add_trace(x = ~date, y = ~erro_ihme, type= "bar",
                                                       marker = list(color = ci), name = "IHME")
fig_erro_br <- fig_erro_br %>%add_trace(x = ~date, y = ~erro_lp, type= "bar",
                                        marker = list(color = cc), name = "CovidLP")
fig_erro_br <- fig_erro_br %>% layout(titlefont=list(size=t), autosize = F, width = w, height = h, margin = m, xaxis = list(title = 'Date', type = 'date', tickformat = "%d/%b"), yaxis =list(title = 'Error', hoverformat = '.2f'), font = list(family = "Arial", size = f),
                                      legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_erro_br <- fig_erro_br %>%
  plotly::config( toImageButtonOptions = list(
  format = "svg", filename = "Brazil_error", width = 600,  height = 400 ) )

fig_erro_br

####################################################
########### Figures October 16-29, 2020 ############
####################################################

# Common configuration for the graphs
rm(list=ls())
m <- list(  l = 50,  r = 50,  b = 70,  t = 70,  pad = 1 )
w=550 # width
h=400 # height
t=20  # title size
f=15  # font size
ci='rgb(160,160,160)' # color ihme
lbi='rgb(0,0,0)' # line ball ihme
cc='rgb(96,96,96)' # color covidlp
lbc='rgb(96,96,96)' # color ball covidp
cd='rgb(0, 0, 0)' # color observed data
last.date <- "2020-10-15"
last.date <- as.Date(last.date)
last.date.14 <- as.Date(last.date + 1:15)
out <-  readRDS("./data_Countries/out.rds")
load("./data_Countries/data_october.RData")

###############################################
# Figure 14.6 (c): Colombia October 16-29, 2020
###############################################

y = yCOL
y= y %>% select(2, 4, 8, 9, 10)
colnames(y)=c("date","local", "death", "LI", "LS")
Colombia= y%>% filter(death!=0)
Colombia <- subset(Colombia, date > as.Date(last.date) )
Colombia <- subset(Colombia, date < as.Date(last.date)+15 )
Colombia_d <- readRDS("./data_Countries/file_Colombia.RDS")
Colombia_d <- Colombia_d[["death"]]
Colombia_d <- Colombia_d[["short_term"]]
Colombia_d <- as.data.frame(Colombia_d)
cases <- Colombia_d$median.2020.10.16
q25 <- Colombia_d$q25.2020.10.16
q975 <- Colombia_d$q975.2020.10.16
date <- Colombia_d$median.date
Colombia_LP=data.frame(date,q25,cases,q975)
Colombia_LP<- Colombia_LP[complete.cases(Colombia_LP),]
colnames(Colombia_LP)=c("date", "LI", "death", "LS")
real_Colombia_plot <- subset(out, c=="Colombia")
real_Colombia_plot<-real_Colombia_plot[1:14,]

fig_Colombia <- Colombia %>% plot_ly() %>% add_trace(x = ~date, y = ~death, type= "scatter", mode = "lines+markers",
                                                     line=list(color=ci, dash='solid', width=2.5),
                                                     marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)), name = "IHME",
                                                     error_y = list(
                                                       type = "data",
                                                       thickness=2,
                                                       width=7,
                                                       color=ci,
                                                       symmetric = FALSE,
                                                       array = c(Colombia$LS - Colombia$death),
                                                       arrayminus = c(Colombia$death - Colombia$LI)))

fig_Colombia <- fig_Colombia %>% add_trace(x = ~Colombia_LP[,1], y = ~Colombia_LP$death, type= "scatter", mode = "lines+markers",
                                           line=list(color=cc, dash='solid', width=2.5),
                                           marker=list(color=cc, size = 7, line = list(color = lbc, width = 1)), name = "CovidLP",
                                           error_y = list(
                                             type = "data",
                                             thickness=2,
                                             width=7,
                                             color=cc,
                                             symmetric = FALSE,
                                             array = c(Colombia_LP$LS - Colombia_LP$death),
                                             arrayminus = c(Colombia_LP$death - Colombia_LP$LI)))

fig_Colombia <- fig_Colombia %>% add_trace(x = ~real_Colombia_plot$date, y = ~real_Colombia_plot$death, type= "scatter", mode = "lines+markers",
                                           line=list(color=cd, dash='solid', width=2.5),
                                           marker=list(color=cd), name = "Observed Data")


fig_Colombia <- fig_Colombia%>% layout(titlefont=list(size=t), autosize = F, width = w, height = h, margin = m, xaxis = list(title = 'Date', type = 'date', tickformat = "%d/%b"), yaxis = list(title = 'Total Deaths', hoverformat = '.0f'), font = list(family = "Arial", size = f),
                                       legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_Colombia <- fig_Colombia %>%
  plotly::config( toImageButtonOptions = list(
      format = "svg", filename = "16_Colombia", width = 600, height = 400 ))

fig_Colombia

# Figure 14.6 (d): Colombia error October 16-29, 2020

Colombia_v <- Colombia[1:length(real_Colombia_plot$date),2:3]
Colombia_vlp <- Colombia_LP[1:length(real_Colombia_plot$date),c(1,3)]
erro_Colombia <- cbind(Colombia_v,Colombia_vlp,real_Colombia_plot[,c(5)])
erro_Colombia <- erro_Colombia[,-c(1)]
erro_Colombia <- mutate(erro_Colombia, erro_ihme = (100*abs(death-death.2)/abs(death.2)))
erro_Colombia <- mutate(erro_Colombia, erro_lp = (100*abs(death.1-death.2)/abs(death.2)))

fig_erro_Colombia <- erro_Colombia %>% plot_ly() %>% add_trace(x = ~date, y = ~erro_ihme, type= "bar",
                                                               marker = list(color = ci), name = "IHME")
fig_erro_Colombia <- fig_erro_Colombia %>%add_trace(x = ~date, y = ~erro_lp, type= "bar",
                                                    marker = list(color = cc), name = "CovidLP")
fig_erro_Colombia <- fig_erro_Colombia %>% layout(titlefont=list(size=t), autosize = F, width = w, height = h, margin = m, xaxis = list(title = 'Date', type = 'date', tickformat = "%d/%b"), yaxis = list(title = 'Error', hoverformat = '.2f'), font = list(family = "Arial", size = f),
                                                  legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_erro_Colombia <- fig_erro_Colombia %>%
  plotly::config( toImageButtonOptions = list(
      format = "svg", filename = "16_Colombia_error", width = 600, height = 400 ) )

fig_erro_Colombia

#############################################
# Figure 14.7 (c): Mexico October 16-29, 2020
#############################################

y = yMEX
y= y %>% select(2, 4, 8, 9, 10)
colnames(y)=c("date","local", "death", "LI", "LS")
Mexico= y%>% filter(death!=0)
Mexico <- subset(Mexico, date > as.Date(last.date) )
Mexico <- subset(Mexico, date < as.Date(last.date)+15 )
Mexico_d <- readRDS("./data_Countries/file_Mexico.RDS")
Mexico_d <- Mexico_d[["death"]]
Mexico_d <- Mexico_d[["short_term"]]
Mexico_d <- as.data.frame(Mexico_d)
cases <- Mexico_d$median.2020.10.16
q25 <- Mexico_d$q25.2020.10.16
q975 <- Mexico_d$q975.2020.10.16
date <- Mexico_d$median.date
Mexico_LP=data.frame(date,q25,cases,q975)
Mexico_LP<- Mexico_LP[complete.cases(Mexico_LP),]
colnames(Mexico_LP)=c("date", "LI", "death", "LS")
real_Mexico_plot <- subset(out, c=="Mexico")
real_Mexico_plot<-real_Mexico_plot[1:14,]

fig_Mexico <- Mexico %>% plot_ly() %>% add_trace(x = ~date, y = ~death, type= "scatter", mode = "lines+markers",
                                                 line=list(color=ci, dash='solid', width=2.5),
                                                 marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)), name = "IHME",
                                                 error_y = list(
                                                   type = "data",
                                                   thickness=2,
                                                   width=7,
                                                   color=ci,
                                                   symmetric = FALSE,
                                                   array = c(Mexico$LS - Mexico$death),
                                                   arrayminus = c(Mexico$death - Mexico$LI)))

fig_Mexico <- fig_Mexico %>% add_trace(x = ~Mexico_LP[,1], y = ~Mexico_LP$death, type= "scatter", mode = "lines+markers",
                                       line=list(color=cc, dash='solid', width=2.5),
                                       marker=list(color=cc, size = 7, line = list(color = lbc, width = 1)), name = "CovidLP",
                                       error_y = list(
                                         type = "data",
                                         thickness=2,
                                         width=7,
                                         color=cc,
                                         symmetric = FALSE,
                                         array = c(Mexico_LP$LS - Mexico_LP$death),
                                         arrayminus = c(Mexico_LP$death - Mexico_LP$LI)))

fig_Mexico <- fig_Mexico %>% add_trace(x = ~real_Mexico_plot$date, y = ~real_Mexico_plot$death, type= "scatter", mode = "lines+markers",
                                       line=list(color=cd, dash='solid', width=2.5),
                                       marker=list(color=cd), name = "Observed Data")

fig_Mexico <- fig_Mexico%>% layout(titlefont=list(size=t), autosize = F, width = w, height = h, margin = m, xaxis = list(title = 'Date', type = 'date', tickformat = "%d/%b"), yaxis = list(title = 'Total Deaths', hoverformat = '.0f'), font = list(family = "Arial", size = f),
                                   legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_Mexico <- fig_Mexico %>% plotly::config(
    toImageButtonOptions = list( format = "svg", 
    filename = "16_Mexico", width = 600, height = 400 ) )

fig_Mexico

# Figure 14.7 (d): Mexico error October 16-29, 2020

Mexico_v <- Mexico[1:length(real_Mexico_plot$date),2:3]
Mexico_vlp <- Mexico_LP[1:length(real_Mexico_plot$date),c(1,3)]
erro_Mexico <- cbind(Mexico_v,Mexico_vlp,real_Mexico_plot[,c(5)])
erro_Mexico <- erro_Mexico[,-c(1)]
erro_Mexico <- mutate(erro_Mexico, erro_ihme = (100*abs(death-death.2)/abs(death.2)))
erro_Mexico <- mutate(erro_Mexico, erro_lp = (100*abs(death.1-death.2)/abs(death.2)))

fig_erro_Mexico <- erro_Mexico %>% plot_ly() %>% add_trace(x = ~date, y = ~erro_ihme, type= "bar",
                                                           marker = list(color = ci), name = "IHME")
fig_erro_Mexico <- fig_erro_Mexico %>%add_trace(x = ~date, y = ~erro_lp, type= "bar",
                                                marker = list(color = cc), name = "CovidLP")
fig_erro_Mexico <- fig_erro_Mexico %>% layout(titlefont=list(size=t), autosize = F, width = w, height = h, margin = m, xaxis = list(title = 'Date', type = 'date', tickformat = "%d/%b"), yaxis = list(title = 'Error', hoverformat = '.2f'), font = list(family = "Arial", size = f),
                                              legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_erro_Mexico <- fig_erro_Mexico %>%
  plotly::config( toImageButtonOptions = list(
  format = "svg", filename = "16_Mexico_error",  width = 600,  height = 400  ))

fig_erro_Mexico

#############################################
# Figure 14.8 (c): Turkey October 16-29, 2020
#############################################

y = yTUR
y= y %>% select(2, 4, 8, 9, 10)
colnames(y)=c("date","local", "death", "LI", "LS")
Turkey= y%>% filter(death!=0)
Turkey <- subset(Turkey, date > as.Date(last.date) )
Turkey <- subset(Turkey, date < as.Date(last.date)+15 )
Turkey_d <- readRDS("./data_Countries/file_Turkey.RDS")
Turkey_d <- Turkey_d[["death"]]
Turkey_d <- Turkey_d[["short_term"]]
Turkey_d <- as.data.frame(Turkey_d)
cases <- Turkey_d$median.2020.10.16
q25 <- Turkey_d$q25.2020.10.16
q975 <- Turkey_d$q975.2020.10.16
date <- Turkey_d$median.date
Turkey_LP=data.frame(date,q25,cases,q975)
Turkey_LP<- Turkey_LP[complete.cases(Turkey_LP),]
colnames(Turkey_LP)=c("date", "LI", "death", "LS")

real_Turkey_plot <- subset(out, c=="Turkey")
real_Turkey_plot<-real_Turkey_plot[1:14,]

fig_Turkey <- Turkey %>% plot_ly() %>% add_trace(x = ~date, y = ~death, type= "scatter", mode = "lines+markers",
                                                 line=list(color=ci, dash='solid', width=2.5),
                                                 marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)), name = "IHME",
                                                 error_y = list(
                                                   type = "data",
                                                   thickness=2,
                                                   width=7,
                                                   color=ci,
                                                   symmetric = FALSE,
                                                   array = c(Turkey$LS - Turkey$death),
                                                   arrayminus = c(Turkey$death - Turkey$LI)))

fig_Turkey <- fig_Turkey %>% add_trace(x = ~Turkey_LP[,1], y = ~Turkey_LP$death, type= "scatter", mode = "lines+markers",
                                       line=list(color=cc, dash='solid', width=2.5),
                                       marker=list(color=cc, size = 7, line = list(color = lbc, width = 1)), name = "CovidLP",
                                       error_y = list(
                                         type = "data",
                                         thickness=2,
                                         width=7,
                                         color=cc,
                                         symmetric = FALSE,
                                         array = c(Turkey_LP$LS - Turkey_LP$death),
                                         arrayminus = c(Turkey_LP$death - Turkey_LP$LI)))

fig_Turkey <- fig_Turkey %>% add_trace(x = ~real_Turkey_plot$date, y = ~real_Turkey_plot$death, type= "scatter", mode = "lines+markers",
                                       line=list(color=cd, dash='solid', width=2.5),
                                       marker=list(color=cd), name = "Observed Data")

fig_Turkey <- fig_Turkey%>% layout(titlefont=list(size=t), autosize = F, width = w, height = h, margin = m, xaxis = list(title = 'Date', type = 'date', tickformat = "%d/%b"), yaxis = list(title = 'Total Deaths', hoverformat = '.0f'), font = list(family = "Arial", size = f),
                                   legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_Turkey <- fig_Turkey %>%
  plotly::config( toImageButtonOptions = list(
  format = "svg", filename = "16_Turkey", width = 600, height = 400 ) )

fig_Turkey

# Figure 14.8 (d): Turkey error October 16-29, 2020

Turkey_v <- Turkey[1:length(real_Turkey_plot$date),2:3]
Turkey_vlp <- Turkey_LP[1:length(real_Turkey_plot$date),c(1,3)]
erro_Turkey <- cbind(Turkey_v,Turkey_vlp,real_Turkey_plot[,c(5)])
erro_Turkey <- erro_Turkey[,-c(1)]
erro_Turkey <- mutate(erro_Turkey, erro_ihme = (100*abs(death-death.2)/abs(death.2)))
erro_Turkey <- mutate(erro_Turkey, erro_lp = (100*abs(death.1-death.2)/abs(death.2)))

fig_erro_Turkey <- erro_Turkey %>% plot_ly() %>% add_trace(x = ~date, y = ~erro_ihme, type= "bar",
                                                           marker = list(color = ci), name = "IHME")
fig_erro_Turkey <- fig_erro_Turkey %>%add_trace(x = ~date, y = ~erro_lp, type= "bar",
                                                marker = list(color = cc), name = "CovidLP")
fig_erro_Turkey <- fig_erro_Turkey %>% layout(titlefont=list(size=t), autosize = F, width = w, height = h, margin = m, xaxis = list(title = 'Date', type = 'date', tickformat = "%d/%b"), yaxis = list(title = 'Error', hoverformat = '.2f'), font = list(family = "Arial", size = f),
                                              legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_erro_Turkey <- fig_erro_Turkey %>% 
  plotly::config( toImageButtonOptions = list(
  format = "svg", filename = "16_Turkey_error", width = 600, height = 400 ) )

fig_erro_Turkey

#################################################
# Figure 14.9 (c): Argentina October 16-29, 2020
#################################################

y = yARG
y= y %>% select(2, 4, 8, 9, 10)
colnames(y)=c("date","local", "death", "LI", "LS")
Argentina= y%>% filter(death!=0)
Argentina <- subset(Argentina, date > as.Date(last.date) )
Argentina <- subset(Argentina, date < as.Date(last.date)+15 )
Argentina_d <- readRDS("./data_Countries/file_Argentina.RDS")
Argentina_d <- Argentina_d[["death"]]
Argentina_d <- Argentina_d[["short_term"]]
Argentina_d <- as.data.frame(Argentina_d)
cases <- Argentina_d$median.2020.10.16
q25 <- Argentina_d$q25.2020.10.16
q975 <- Argentina_d$q975.2020.10.16
date <- Argentina_d$median.date
Argentina_LP=data.frame(date,q25,cases,q975)
Argentina_LP<- Argentina_LP[complete.cases(Argentina_LP),]
colnames(Argentina_LP)=c("date", "LI", "death", "LS")
real_Argentina_plot <- subset(out, c=="Argentina")
real_Argentina_plot<-real_Argentina_plot[1:14,]

fig_Argentina <- Argentina %>% plot_ly() %>% add_trace(x = ~date, y = ~death, type= "scatter", mode = "lines+markers",
                                                       line=list(color=ci, dash='solid', width=2.5),
                                                       marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)), name = "IHME",
                                                       error_y = list(
                                                         type = "data",
                                                         thickness=2,
                                                         width=7,
                                                         color=ci,
                                                         symmetric = FALSE,
                                                         array = c(Argentina$LS - Argentina$death),
                                                         arrayminus = c(Argentina$death - Argentina$LI)))

fig_Argentina <- fig_Argentina %>% add_trace(x = ~Argentina_LP[,1], y = ~Argentina_LP$death, type= "scatter", mode = "lines+markers",
                                             line=list(color=cc, dash='solid', width=2.5),
                                             marker=list(color=cc, size = 7, line = list(color = lbc, width = 1)), name = "CovidLP",
                                             error_y = list(
                                               type = "data",
                                               thickness=2,
                                               width=7,
                                               color=cc,
                                               symmetric = FALSE,
                                               array = c(Argentina_LP$LS - Argentina_LP$death),
                                               arrayminus = c(Argentina_LP$death - Argentina_LP$LI)))

fig_Argentina <- fig_Argentina %>% add_trace(x = ~real_Argentina_plot$date, y = ~real_Argentina_plot$death, type= "scatter", mode = "lines+markers",
                                             line=list(color=cd, dash='solid', width=2.5),
                                             marker=list(color=cd), name = "Observed Data")


fig_Argentina <- fig_Argentina%>% layout(titlefont=list(size=t), autosize = F, width = w, height = h, margin = m, xaxis = list(title = 'Date', type = 'date', tickformat = "%d/%b"), yaxis = list(title = 'Total Deaths', hoverformat = '.0f'), font = list(family = "Arial", size = f),
                                         legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_Argentina <- fig_Argentina %>%
  plotly::config( toImageButtonOptions = list(
  format = "svg", filename = "16_Argentina", width = 600,  height = 400 ) )

fig_Argentina

# Figure 14.9 (d): Argentina error October 16-29, 2020

Argentina_v <- Argentina[1:length(real_Argentina_plot$date),2:3]
Argentina_vlp <- Argentina_LP[1:length(real_Argentina_plot$date),c(1,3)]
erro_Argentina <- cbind(Argentina_v,Argentina_vlp,real_Argentina_plot[,c(5)])
erro_Argentina <- erro_Argentina[,-c(1)]
erro_Argentina <- mutate(erro_Argentina, erro_ihme = (100*abs(death-death.2)/abs(death.2)))
erro_Argentina <- mutate(erro_Argentina, erro_lp = (100*abs(death.1-death.2)/abs(death.2)))

fig_erro_Argentina <- erro_Argentina %>% plot_ly() %>% add_trace(x = ~date, y = ~erro_ihme, type= "bar",
                                                                 marker = list(color = ci), name = "IHME")
fig_erro_Argentina <- fig_erro_Argentina %>%add_trace(x = ~date, y = ~erro_lp, type= "bar",
                                                      marker = list(color = cc), name = "CovidLP")
fig_erro_Argentina <- fig_erro_Argentina %>% layout(titlefont=list(size=t), autosize = F, width = w, height = h, margin = m, xaxis = list(title = 'Date', type = 'date', tickformat = "%d/%b"), yaxis = list(title = 'Error', hoverformat = '.2f'), font = list(family = "Arial", size = f),
                                                    legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_erro_Argentina <- fig_erro_Argentina %>%
  plotly::config( toImageButtonOptions = list(
  format = "svg", filename = "16_Argentina_error", width = 600, height = 400 ) )

fig_erro_Argentina

###########################################
# Figure 14.10 (c): US October 16-29, 2020
###########################################

y = yUSA
y= y %>% select(2, 4, 8, 9, 10)
colnames(y)=c("date","local", "death", "LI", "LS")
US= y%>% filter(death!=0)
US <- subset(US, date > as.Date(last.date) )
US <- subset(US, date < as.Date(last.date)+15 )
US_d <- readRDS("./data_Countries/file_US.RDS")
US_d <- US_d[["death"]]
US_d <- US_d[["short_term"]]
US_d <- as.data.frame(US_d)
cases <- US_d$median.2020.10.16
q25 <- US_d$q25.2020.10.16
q975 <- US_d$q975.2020.10.16
date <- US_d$median.date
US_LP=data.frame(date,q25,cases,q975)
US_LP<- US_LP[complete.cases(US_LP),]
colnames(US_LP)=c("date", "LI", "death", "LS")
real_US_plot <- subset(out, c=="US")
real_US_plot<-real_US_plot[1:14,]

fig_US <- US %>% plot_ly() %>% add_trace(x = ~date, y = ~death, type= "scatter", mode = "lines+markers",
                                         line=list(color=ci, dash='solid', width=2.5),
                                         marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)), name = "IHME",
                                         error_y = list(
                                           type = "data",
                                           thickness=2,
                                           width=7,
                                           color=ci,
                                           symmetric = FALSE,
                                           array = c(US$LS - US$death),
                                           arrayminus = c(US$death - US$LI)))

fig_US <- fig_US %>% add_trace(x = ~US_LP[,1], y = ~US_LP$death, type= "scatter", mode = "lines+markers",
                               line=list(color=cc, dash='solid', width=2.5),
                               marker=list(color=cc, size = 7, line = list(color = lbc, width = 1)), name = "CovidLP",
                               error_y = list(
                                 type = "data",
                                 thickness=2,
                                 width=7,
                                 color=cc,
                                 symmetric = FALSE,
                                 array = c(US_LP$LS - US_LP$death),
                                 arrayminus = c(US_LP$death - US_LP$LI)))

fig_US <- fig_US %>% add_trace(x = ~real_US_plot$date, y = ~real_US_plot$death, type= "scatter", mode = "lines+markers",
                               line=list(color=cd, dash='solid', width=2.5),
                               marker=list(color=cd), name = "Observed Data")

fig_US <- fig_US%>% layout(titlefont=list(size=t), autosize = F, width = w, height = h, margin = m, xaxis = list(title = 'Date', type = 'date', tickformat = "%d/%b"), yaxis = list(title = 'Total Deaths', hoverformat = '.0f'), font = list(family = "Arial", size = f),
                           legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_US <- fig_US %>%
  plotly::config( toImageButtonOptions = list(
  format = "svg", filename = "16_US", width = 600, height = 400 ) )

fig_US

# Figure 14.10 (d): US error October 16-29, 2020

US_v <- US[1:length(real_US_plot$date),2:3]
US_vlp <- US_LP[1:length(real_US_plot$date),c(1,3)]
erro_US <- cbind(US_v,US_vlp,real_US_plot[,c(5)])
erro_US <- erro_US[,-c(1)]
erro_US <- mutate(erro_US, erro_ihme = (100*abs(death-death.2)/abs(death.2)))
erro_US <- mutate(erro_US, erro_lp = (100*abs(death.1-death.2)/abs(death.2)))

fig_erro_US <- erro_US %>% plot_ly() %>% add_trace(x = ~date, y = ~erro_ihme, type= "bar",
                                                   marker = list(color = ci), name = "IHME")
fig_erro_US <- fig_erro_US %>%add_trace(x = ~date, y = ~erro_lp, type= "bar",
                                        marker = list(color = cc), name = "CovidLP")
fig_erro_US <- fig_erro_US %>% layout(titlefont=list(size=t), autosize = F, width = w, height = h, margin = m, xaxis = list(title = 'Date', type = 'date', tickformat = "%d/%b"), yaxis = list(title = 'Error', hoverformat = '.2f'), font = list(family = "Arial", size = f),
                                      legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_erro_US <- fig_erro_US %>%
  plotly::config( toImageButtonOptions = list(
  format = "svg", filename = "16_US_error", width = 600, height = 400 ) )

fig_erro_US

##############################################
###############  Section 14.4  ###############
##############################################

# common configuration for the graphs
rm(list=ls())
m <- list( l = 50,  r = 50,  b = 70,  t = 70,  pad = 1 )
w=550 # width
h=400 # height
t=20  # title size
f=15  # font size
ci='rgb(160,160,160)' # color ihme
lbi='rgb(0,0,0)' # line ball ihme
cc='rgb(96,96,96)' # color covidlp
lbc='rgb(96,96,96)' # color ball covidp
cd='rgb(0, 0, 0)' # color observed data
so1='rgb(224, 224, 224)'
so2='rgb(128, 128, 128)'

##############################################
# Figure 14.11 (a): Australia August 26, 2020
##############################################

Australia_d <- readRDS("./data_Countries/file_Australia.RDS")
Australia_d <- Australia_d[["confirmed"]]
Australia_d <- Australia_d[["short_term"]]
Australia_d <- as.data.frame(Australia_d)
cases <- Australia_d$median.2020.08.26
q25 <- Australia_d$q25.2020.08.26
q975 <- Australia_d$q975.2020.08.26
date <- Australia_d$median.date
Australia_LP_before=data.frame(date,q25,cases,q975)
Australia_LP_before <- Australia_LP_before[complete.cases(Australia_LP_before),]
colnames(Australia_LP_before)=c("date", "LI", "cases", "LS")
baseURL = "https://github.com/gabrieloa/covid/blob/master/file_Australia.RDS?raw=true"
Australia_d <- readRDS(file=url(baseURL))
Australia_d <- Australia_d[["confirmed"]]
Australia_d <- Australia_d[["short_term"]]
Australia_d <- as.data.frame(Australia_d)
cases <- Australia_d$median.2020.08.27
q25 <- Australia_d$q25.2020.08.27
q975 <- Australia_d$q975.2020.08.27
date <- Australia_d$median.date
Australia_LP_after=data.frame(date,q25,cases,q975)
Australia_LP_after <- Australia_LP_after[complete.cases(Australia_LP_after),]
colnames(Australia_LP_after)=c("date", "LI", "cases", "LS")

####################################################################################################
#the following file named "Data_Australia_05122020.csv" should be downloaded from CovidLP platform.
real_Australia_plot <-read_csv("./data_Countries/australia_comp.csv")
####################################################################################################

real_Australia_plot<-real_Australia_plot[219:232,]
colnames(real_Australia_plot) <- c("date", "c_cases","c_deaths", "cases", "deaths")

fig_Australia <- Australia_LP_before %>% plot_ly() %>% add_trace(x = ~date, y = ~cases, type= "scatter", mode = "lines+markers",
                                                                 line=list(color=ci, dash='solid', width=2.5),
                                                                 marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)), name = "CovidLP - before")

fig_Australia <- fig_Australia %>% add_trace(x = ~Australia_LP_after$date, y = ~Australia_LP_after$cases, type= "scatter", mode = "lines+markers",
                                             line=list(color=so1, dash='solid', width=2.5),
                                             marker=list(color=so1,  size = 7,line = list(color = lbi,width = 1)), name = "CovidLP - after")

fig_Australia <- fig_Australia %>% add_trace(x = ~real_Australia_plot$date, y = ~real_Australia_plot$c_cases, type= "scatter", mode = "lines+markers",
                                             line=list(color=cd, dash='solid', width=2.5),
                                             marker=list(color=cd), name = "Observed Data")

fig_Australia <- fig_Australia%>% layout(titlefont=list(size=t), autosize = F, width = w, height = h, margin = m, xaxis = list(title = 'Date'  , type = 'date', tickformat = "%d/%b"), yaxis = list(title = 'Confirmed Cases', hoverformat = '.0f'), font = list(family = "Arial", size = f),
                                         legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_Australia <- fig_Australia %>% add_trace(
  x = Australia_LP_before$date,
  y = Australia_LP_before$LI,
  showlegend = F,
  name = "95% CI",
  type = 'scatter',
  mode = 'lines', hoverinfo = "x+y",
  fillcolor = 'rgba(150, 150, 150, 0.5)',
  line = list(color = 'rgba(0, 0, 0, 1)', width = 0)
) %>%
  add_trace(
    x = Australia_LP_before$date,
    y = Australia_LP_before$LS,
    type = 'scatter',
    mode = 'lines', hoverinfo = "x+y",
    fill = 'tonexty',
    name = "95% CI",
    fillcolor = 'rgba(100, 100, 100, 0.5)',
    line = list(color = 'rgba(0, 0, 0, 1)', width = 0)
  )

fig_Australia <- fig_Australia %>% add_trace(
  x = Australia_LP_after$date,
  y = Australia_LP_after$LI,
  showlegend = F,
  name = "95% CI",
  type = 'scatter',
  mode = 'lines', hoverinfo = "x+y",
  fillcolor = 'rgba(150, 150, 150, 0.5)',
  line = list(color = 'rgba(0, 0, 0, 1)', width = 0)
) %>%
  add_trace(
    x = Australia_LP_after$date,
    y = Australia_LP_after$LS,
    showlegend = F,
    type = 'scatter',
    mode = 'lines', hoverinfo = "x+y",
    fill = 'tonexty',
    name = "95% CI",
    fillcolor = 'rgba(100, 100, 100, 0.5)',
    line = list(color = 'rgba(0, 0, 0, 1)', width = 0)
  )

fig_Australia <- fig_Australia %>%
  plotly::config( toImageButtonOptions = list(
  format = "svg", filename = "Australia", width = 600,  height = 400 ) )

fig_Australia

##########################################
# Figure 14.11 (b): Spain August 26, 2020
##########################################

Spain_d <- readRDS("./data_Countries/file_Spain.RDS")
Spain_d <- Spain_d[["confirmed"]]
Spain_d <- Spain_d[["short_term"]]
Spain_d <- as.data.frame(Spain_d)
cases <- Spain_d$median.2020.08.26
q25 <- Spain_d$q25.2020.08.26
q975 <- Spain_d$q975.2020.08.26
date <- Spain_d$median.date
Spain_LP_before=data.frame(date,q25,cases,q975)
Spain_LP_before <- Spain_LP_before[complete.cases(Spain_LP_before),]
colnames(Spain_LP_before)=c("date", "LI", "cases", "LS")
baseURL = "https://github.com/gabrieloa/covid/blob/master/file_Spain.RDS?raw=true"
Spain_d <- readRDS(file=url(baseURL))
Spain_d <- Spain_d[["confirmed"]]
Spain_d <- Spain_d[["short_term"]]
Spain_d <- as.data.frame(Spain_d)
cases <- Spain_d$median.2020.08.27
q25 <- Spain_d$q25.2020.08.27
q975 <- Spain_d$q975.2020.08.27
date <- Spain_d$median.date
Spain_LP_after=data.frame(date,q25,cases,q975)
Spain_LP_after <- Spain_LP_after[complete.cases(Spain_LP_after),]
colnames(Spain_LP_after)=c("date", "LI", "cases", "LS")
####################################################################################################
# the following file named "Data_Spain_14122020.csv" should be downloaded from CovidLP platform.
real_Spain_plot <-read_csv("./data_Countries/spain_comp.csv")
####################################################################################################
real_Spain_plot<-real_Spain_plot[219:232,]
colnames(real_Spain_plot) <- c("date", "c_cases","c_deaths", "cases", "deaths")

fig_Spain <- Spain_LP_before %>% plot_ly() %>% add_trace(x = ~date, y = ~cases, type= "scatter", mode = "lines+markers",
                                                         line=list(color=ci, dash='solid', width=2.5),
                                                         marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)), name = "CovidLP - before")

fig_Spain <- fig_Spain %>% add_trace(x = ~Spain_LP_after$date, y = ~Spain_LP_after$cases, type= "scatter", mode = "lines+markers",
                                     line=list(color=so1, dash='solid', width=2.5),
                                     marker=list(color=so1,  size = 7,line = list(color = lbi,width = 1)), name = "CovidLP - after")

fig_Spain <- fig_Spain %>% add_trace(x = ~real_Spain_plot$date, y = ~real_Spain_plot$c_cases, type= "scatter", mode = "lines+markers",
                                     line=list(color=cd, dash='solid', width=2.5),
                                     marker=list(color=cd), name = "Observed Data")

fig_Spain <- fig_Spain%>% layout(titlefont=list(size=t), autosize = F, width = w, height = h, margin = m, xaxis = list(title = 'Date', type = 'date', tickformat = "%d/%b"), yaxis = list(title = 'Confirmed Cases', hoverformat = '.0f'), font = list(family = "Arial", size = f),
                                 legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_Spain <- fig_Spain %>% add_trace(
  x = Spain_LP_before$date,
  y = Spain_LP_before$LI,
  showlegend = F,
  name = "95% CI",
  type = 'scatter',
  mode = 'lines', hoverinfo = "x+y",
  fillcolor = 'rgba(150, 150, 150, 0.5)',
  line = list(color = 'rgba(0, 0, 0, 1)', width = 0)
) %>%
  add_trace(
    x = Spain_LP_before$date,
    y = Spain_LP_before$LS,
    type = 'scatter',
    mode = 'lines', hoverinfo = "x+y",
    fill = 'tonexty',
    name = "95% CI",
    # fillcolor = 'rgba(150, 150, 150, 0.5)',
    fillcolor = 'rgba(100, 100, 100, 0.5)',
    line = list(color = 'rgba(0, 0, 0, 1)', width = 0)
  )

fig_Spain <- fig_Spain %>% add_trace(
  x = Spain_LP_after$date,
  y = Spain_LP_after$LI,
  showlegend = F,
  name = "95% CI",
  type = 'scatter',
  mode = 'lines', hoverinfo = "x+y",
  fillcolor = 'rgba(150, 150, 150, 0.5)',
  line = list(color = 'rgba(0, 0, 0, 1)', width = 0)
) %>%
  add_trace(
    x = Spain_LP_after$date,
    y = Spain_LP_after$LS,
    showlegend = F,
    type = 'scatter',
    mode = 'lines', hoverinfo = "x+y",
    fill = 'tonexty',
    name = "95% CI",
    fillcolor = 'rgba(100, 100, 100, 0.5)',
    line = list(color = 'rgba(0, 0, 0, 1)', width = 0))

fig_Spain <- fig_Spain %>%
  plotly::config( toImageButtonOptions = list(
  format = "svg", filename = "Spain", width = 600,  height = 400 ) )

fig_Spain
