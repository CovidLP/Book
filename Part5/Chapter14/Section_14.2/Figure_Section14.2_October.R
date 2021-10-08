###---------------------------------------------------------------###
###   Building a Platform for Data-Driven Pandemic Prediction:    ###
###   From Data Modelling to Visualization - the CovidLP Project  ###
###                                                               ###
###   Chapter 14: Comparing predictions.                          ###
###   Section 14.2: Analysis for Brazilian states                 ###
###   Subsection                                                  ###
###   Figure 14.1 - 14.5 (Period of October)                      ###
###                                                               ###
###   Author: the CovidLP Team                                    ###
###---------------------------------------------------------------###

library(rsvg)
require(dplyr)
require(ggplot2)
require(plotly)
require(MLmetrics)
require(highcharter)

w=650 # width
h=500 # height
t=25  # title size
f=19  # font size

## Colors ##

ci='rgb(160,160,160)' # color ihme
lbi='rgb(160,160,160)' # line ball ihme
cc='rgb(96, 96, 96)' # color covidlp
lbc='rgb(96, 96, 96)' # color ball covidp
cd='rgb(0, 0, 0)' # color observed data

x = load("./data_Brazil_October/data_october.RData")

################################################
### Figure 14.1 - State of Minas Gerais, Brazil.
### Period: October
################################################

y = yMG
location_name <- y$location_name
date <- as.Date(y$date)
mean <- y$totdea_mean
LI <- y$totdea_lower
LS <- y$totdea_upper

y <- cbind.data.frame(location_name,date,mean,LI,LS)
colnames(y)=c("local","datas", "med", "LI", "LS")
y <- data.frame(y)
estado_IHME <- data.frame()
estado_IHME= y[252:265,]

file_estado <- readRDS("./data_Brazil_October/file_MG.RDS")
med_MG <- file_estado[["death"]][["short_term"]][["median"]]$`2020-10-12`
inf_MG <- file_estado[["death"]][["short_term"]][["q25"]]$`2020-10-12`
sup_MG <- file_estado[["death"]][["short_term"]][["q975"]]$`2020-10-12`
estado_LP_MG <- cbind(med_MG,inf_MG,sup_MG)
estado_LP_MG <- estado_LP_MG[120:133,]
datas <- estado_IHME$datas
estado_LP_MG <- cbind.data.frame(datas,estado_LP_MG)

real_MG <- read.csv("./data_Brazil_October/bases/STpred_Brazil_MG_d_23102020.csv")
colnames(real_MG)  <- c("data", "cumdeath", "pred25", "predmed", "pred75" )
real_MG <- as.data.frame(real_MG)
med_MG <- real_MG$cumdeath[231:244]
med_MG <- as.data.frame(med_MG)
datas = date[252:265]
estado_obs_MG <- cbind.data.frame(datas,med_MG)

m <- list(  l = 100,  r = 100,  b = 100,  t = 100,  pad = 4 )

fig_estado_MG <- estado_IHME %>% plot_ly() %>% add_trace(x = ~datas, y = ~med, type= "scatter", mode = "lines+markers",
                 line=list(color=ci, dash='solid', width=2.5),
                 marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)),  
                 name = "IHME", error_y = list( type = "data", thickness=2, width=7,
                 color=ci, symmetric = FALSE,
                 array = c(estado_IHME$LS - estado_IHME$med),
                 arrayminus = c(estado_IHME$med - estado_IHME$LI)))

fig_estado_MG <- fig_estado_MG %>% add_trace(x = ~estado_LP_MG$datas, y = ~estado_LP_MG$med, type= "scatter", mode = "lines+markers",
                 line=list(color=cc, dash='solid', width=2.5),
                 marker=list(color=cc, size = 7, line = list(color = lbc, width = 1)),  
                 name = "CovidLP", error_y = list( type = "data", thickness=2, width=7,
                 color=cc, symmetric = FALSE,
                 array = c(estado_LP_MG$sup - estado_LP_MG$med),
                 arrayminus = c(estado_LP_MG$med - estado_LP_MG$inf)))

fig_estado_MG <- fig_estado_MG %>% add_trace(x = ~estado_obs_MG$datas, 
                 y = ~estado_obs_MG$med, type= "scatter", mode = "lines+markers",
                 line=list(color=cd, dash='solid', width=3),
                 marker=list(color=cd, width=8), name = "Observed Data")

fig_estado_MG <- fig_estado_MG %>% layout(
  title = "Minas Gerais - Predictions Comparison",
  xaxis = list(title = "date"),  yaxis = list(title = "Total Deaths"))

fig_estado_MG

### Relative error

med <- real_MG$cumdeath[231:244]
med_ <- as.numeric(y$med[252:265])
MSE_IHME <- MSE(med,med_)

med_LP <- as.numeric(estado_LP_MG$med[1:14])
MSE_LP <- MSE(med,med_LP)

erro_IHME <- (abs(med_ - med)/abs(med))*100 
erro_CovidLP <- (abs(med_LP - med)/abs(med))*100
erro_estado <- cbind.data.frame(datas,erro_IHME,erro_CovidLP)

summary(erro_CovidLP)
summary(erro_IHME)

fig_erro_estado_MG <- erro_estado %>% plot_ly() %>% add_trace(x = ~datas, 
                      y = ~erro_IHME, type= "bar", marker = list(color = ci), 
                      name = "IHME")

fig_erro_estado_MG <- fig_erro_estado_MG %>%add_trace(x = ~datas, y = ~erro_CovidLP, type= "bar",
                      marker = list(color = cc), name = "CovidLP")

fig_erro_estado_MG <- fig_erro_estado_MG %>% layout(
  title = "Minas Gerais - Absolute Relative Error",
  xaxis = list(title = "date"), yaxis = list(title = "Error"))

fig_erro_estado_MG

#########################################
### Figure 14.2 - State of Bahia, Brazil.
### Period: October
#########################################

y = yBA 
location_name <- y$location_name
date <- as.Date(y$date)
mean <- y$totdea_mean
LI <- y$totdea_lower
LS <- y$totdea_upper

y <- cbind.data.frame(location_name,date,mean,LI,LS)
colnames(y)=c("local","datas", "med", "LI", "LS")
y <- data.frame(y)
estado_IHME <- data.frame()
estado_IHME= y[252:265,]

estado_IHME$datas <- format(as.Date(estado_IHME$datas), "%d/%b")
estado_IHME$datas <- as.Date(estado_IHME$datas)

file_estado <- readRDS("./data_Brazil_October/file_BA.RDS")
med_Bahia <- file_estado[["death"]][["short_term"]][["median"]]$`2020-10-12`
inf_Bahia <- file_estado[["death"]][["short_term"]][["q25"]]$`2020-10-12`
sup_Bahia <- file_estado[["death"]][["short_term"]][["q975"]]$`2020-10-12`
estado_LP_Bahia <- cbind(med_Bahia,inf_Bahia,sup_Bahia)
estado_LP_Bahia <- estado_LP_Bahia[120:133,]
datas <- estado_IHME$datas
estado_LP_Bahia <- cbind.data.frame(datas,estado_LP_Bahia)

real_BA <- read.csv("./data_Brazil_October/bases/STpred_Brazil_BA_d_23102020.csv")
colnames(real_BA)  <- c("data", "cumdeath", "pred25", "predmed", "pred75" )
real_BA <- as.data.frame(real_BA)
med_Bahia <- real_BA$cumdeath[231:244]
med_Bahia <- as.data.frame(med_Bahia)
datas = date[252:265]

datass <- format(as.Date(datas), "%d-%b")
estado_obs_Bahia <- cbind.data.frame(datas,med_Bahia)

estado_obs_Bahia$datas <- format(as.Date(estado_obs_Bahia$datas), "%d/%b")

m <- list(  l = 100,  r = 100,  b = 100,  t = 100,  pad = 4 )

fig_estado_Bahia <- estado_IHME %>% plot_ly() %>% add_trace(x = ~datass, y = ~med, type= "scatter", mode = "lines+markers",
                    line=list(color=ci, dash='solid', width=2.5),
                    marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)),  
                    name = "IHME", error_y = list( type = "data", thickness=2, width=7,
                    color=ci, symmetric = FALSE,
                    array = c(estado_IHME$LS - estado_IHME$med),
                    arrayminus = c(estado_IHME$med - estado_IHME$LI)))

fig_estado_Bahia <- fig_estado_Bahia %>% add_trace(x = ~datass, y = ~estado_LP_Bahia$med, type= "scatter", mode = "lines+markers",
                    line=list(color=cc, dash='solid', width=2.5),
                    marker=list(color=cc, size = 7, line = list(color = lbc, width = 1)),  
                    name = "CovidLP", error_y = list( type = "data", thickness=2, width=7,
                    color=cc, symmetric = FALSE,
                    array = c(estado_LP_Bahia$sup - estado_LP_Bahia$med),
                    arrayminus = c(estado_LP_Bahia$med - estado_LP_Bahia$inf)))

fig_estado_Bahia <- fig_estado_Bahia %>% add_trace(x = ~datass, y = ~estado_obs_Bahia$med, 
                    type= "scatter", mode = "lines+markers",line=list(color=cd, dash='solid', width=3),
                    marker=list(color=cd, width=8), name = "Observed Data")

fig_estado_Bahia <- fig_estado_Bahia %>% layout(titlefont=list(size=t), 
                    autosize = F, width = w, height = h, margin = m, 
                    xaxis = list(title = 'Date'), yaxis = list(title = 'Total Deaths', 
                    hoverformat = '.0f'), font = list(family = "Arial", size = f), 
                    legend = list(orientation = "h", xanchor = "center",
                    x = 0.5, y = 1.15))

fig_estado_Bahia

fig_estado_Bahia <- fig_estado_Bahia %>% plotly::config(toImageButtonOptions = list(
      format = "svg", filename = "myplot", width = 600, height = 400 ))

fig_estado_Bahia

### Relative error 

med <- real_BA$cumdeath[231:244]
med_ <- as.numeric(y$med[252:265])
MSE_IHME <- MSE(med,med_)

med_LP <- as.numeric(estado_LP_Bahia$med[1:14])
MSE_LP <- MSE(med,med_LP)

erro_IHME <- (abs(med_ - med)/abs(med))*100 
erro_CovidLP <- (abs(med_LP - med)/abs(med))*100
erro_estado <- cbind.data.frame(datas,erro_IHME,erro_CovidLP)

summary(erro_CovidLP)
summary(erro_IHME)

fig_erro_estado_Bahia <- erro_estado %>% plot_ly() %>% add_trace(x = ~datass, 
                         y = ~erro_IHME, type= "bar", marker = list(color = ci), 
                         name = "IHME")

fig_erro_estado_Bahia <- fig_erro_estado_Bahia %>%add_trace(x = ~datass, 
                         y = ~erro_CovidLP, type= "bar",
                         marker = list(color = cc), name = "CovidLP")

fig_erro_estado_Bahia <- fig_erro_estado_Bahia %>% layout(titlefont=list(size=t), 
                         autosize = F, width = w, height = h, margin = m, 
                         xaxis = list(title = 'Date'), yaxis = list(title = 'Error', 
                         hoverformat = '.0f'), font = list(family = "Arial", size = f), 
                         legend = list(orientation = "h", xanchor = "center",
                         x = 0.5, y = 1.15))

fig_erro_estado_Bahia

fig_erro_estado_Bahia <- fig_erro_estado_Bahia %>% plotly::config( 
      toImageButtonOptions = list( format = "svg", filename = "myplot", 
      width = 600, height = 400 ) )

fig_erro_estado_Bahia


############################################
### Figure 14.3 - State of Maranhao, Brazil.
### Period: October
############################################

y = yMA
location_name <- y$location_name
date <- as.Date(y$date)
mean <- y$totdea_mean
LI <- y$totdea_lower
LS <- y$totdea_upper

y <- cbind.data.frame(location_name,date,mean,LI,LS)
colnames(y)=c("local","datas", "med", "LI", "LS")
y <- data.frame(y)
estado_IHME <- data.frame()
estado_IHME= y[252:265,]

file_estado <- readRDS("./data_Brazil_October/file_MA.RDS")
med_MA <- file_estado[["death"]][["short_term"]][["median"]]$`2020-10-12`
inf_MA <- file_estado[["death"]][["short_term"]][["q25"]]$`2020-10-12`
sup_MA <- file_estado[["death"]][["short_term"]][["q975"]]$`2020-10-12`
estado_LP_MA <- cbind(med_MA,inf_MA,sup_MA)
estado_LP_MA <- estado_LP_MA[120:133,]
datas <- estado_IHME$datas
estado_LP_MA <- cbind.data.frame(datas,estado_LP_MA)

real_MA <- read.csv("./data_Brazil_October/bases/STpred_Brazil_MA_d_23102020.csv")
colnames(real_MA)  <- c("data", "cumdeath", "pred25", "predmed", "pred75" )
real_MA <- as.data.frame(real_MA)
med_MA <- real_MA$cumdeath[231:244]
med_MA <- as.data.frame(med_MA)
datas = date[252:265]
estado_obs_MA <- cbind.data.frame(datas,med_MA)

m <- list( l = 100,  r = 100,  b = 100,  t = 100,  pad = 4 )

fig_estado_MA <- estado_IHME %>% plot_ly() %>% add_trace(x = ~datas, y = ~med, type= "scatter", mode = "lines+markers",
                 line=list(color=ci, dash='solid', width=2.5),
                 marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)),  
                 name = "IHME", error_y = list( type = "data", thickness=2, width=7,
                 color=ci, symmetric = FALSE,
                 array = c(estado_IHME$LS - estado_IHME$med),
                 arrayminus = c(estado_IHME$med - estado_IHME$LI)))

fig_estado_MA <- fig_estado_MA %>% add_trace(x = ~estado_LP_MA$datas, y = ~estado_LP_MA$med, type= "scatter", mode = "lines+markers",
                 line=list(color=cc, dash='solid', width=2.5),
                 marker=list(color=cc, size = 7, line = list(color = lbc, width = 1)),  
                 name = "CovidLP", error_y = list( type = "data", thickness=2, width=7,
                 color=cc, symmetric = FALSE,
                 array = c(estado_LP_MA$sup - estado_LP_MA$med),
                 arrayminus = c(estado_LP_MA$med - estado_LP_MA$inf)))

fig_estado_MA <- fig_estado_MA %>% add_trace(x = ~estado_obs_MA$datas, 
                 y = ~estado_obs_MA$med, type= "scatter", mode = "lines+markers",
                 line=list(color=cd, dash='solid', width=3),
                 marker=list(color=cd, width=8), name = "Observed Data")

fig_estado_MA <- fig_estado_MA %>% layout( title = "Maranh?o - Predictions Comparison",
  xaxis = list(title = "date"), yaxis = list(title = "Total Deaths"))

fig_estado_MA

### Relative error

med <- real_MA$cumdeath[231:244]
med_ <- as.numeric(y$med[252:265])
MSE_IHME <- MSE(med,med_)

med_LP <- as.numeric(estado_LP_MA$med[1:14])
MSE_LP <- MSE(med,med_LP)

erro_IHME <- (abs(med_ - med)/abs(med))*100 
erro_CovidLP <- (abs(med_LP - med)/abs(med))*100
erro_estado <- cbind.data.frame(datas,erro_IHME,erro_CovidLP)

summary(erro_CovidLP)
summary(erro_IHME)

fig_erro_estado_MA <- erro_estado %>% plot_ly() %>% add_trace(x = ~datas, 
                      y = ~erro_IHME, type= "bar", marker = list(color = ci), 
                      name = "IHME")

fig_erro_estado_MA <- fig_erro_estado_MA %>%add_trace(x = ~datas, y = ~erro_CovidLP, 
                      type= "bar", marker = list(color = cc), name = "CovidLP")

fig_erro_estado_MA <- fig_erro_estado_MA %>% layout(
  title = "Maranh?o - Absolute Relative Error",
  xaxis = list(title = "date"),  yaxis = list(title = "Error"))

fig_erro_estado_MA

#####################################################
### Figure 14.4 - State of Rio Grande do Sul, Brazil.
### Period: October
#####################################################

y = yRS
location_name <- y$location_name
date <- as.Date(y$date)
mean <- y$totdea_mean
LI <- y$totdea_lower
LS <- y$totdea_upper

y <- cbind.data.frame(location_name,date,mean,LI,LS)
colnames(y)=c("local","datas", "med", "LI", "LS")
y <- data.frame(y)
estado_IHME <- data.frame()
estado_IHME= y[252:265,]

estado_IHME$datas <- format(as.Date(estado_IHME$datas), "%d/%b")
estado_IHME$datas <- as.Date(estado_IHME$datas)

file_estado <- readRDS("./data_Brazil_October/file_RS.RDS")
med_RGSul <- file_estado[["death"]][["short_term"]][["median"]]$`2020-10-12`
inf_RGSul <- file_estado[["death"]][["short_term"]][["q25"]]$`2020-10-12`
sup_RGSul <- file_estado[["death"]][["short_term"]][["q975"]]$`2020-10-12`
estado_LP_RGSul <- cbind(med_RGSul,inf_RGSul,sup_RGSul)
estado_LP_RGSul <- estado_LP_RGSul[120:133,]
datas <- estado_IHME$datas
estado_LP_RGSul <- cbind.data.frame(datas,estado_LP_RGSul)

real_RS <- read.csv("./data_Brazil_October/bases/STpred_Brazil_RS_d_23102020.csv")
colnames(real_RS)  <- c("data", "cumdeath", "pred25", "predmed", "pred75" )
real_RS <- as.data.frame(real_RS)
med_RGSul <- real_RS$cumdeath[231:244]
med_RGSul <- as.data.frame(med_RGSul)
datas = date[252:265]

datass <- format(as.Date(datas), "%d-%b")
estado_obs_RGSul <- cbind.data.frame(datas,med_RGSul)

estado_obs_RGSul$datas <- format(as.Date(estado_obs_RGSul$datas), "%d/%b")

m <- list( l = 100,  r = 100,  b = 100,  t = 100,  pad = 4 )

fig_estado_RGSul <- estado_IHME %>% plot_ly() %>% add_trace(x = ~datass, y = ~med, type= "scatter", mode = "lines+markers",
                    line=list(color=ci, dash='solid', width=2.5),
                    marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)),  
                    name = "IHME", error_y = list( type = "data", thickness=2,  width=7,
                    color=ci, symmetric = FALSE,
                    array = c(estado_IHME$LS - estado_IHME$med),
                    arrayminus = c(estado_IHME$med - estado_IHME$LI)))

fig_estado_RGSul <- fig_estado_RGSul %>% add_trace(x = ~datass, y = ~estado_LP_RGSul$med, type= "scatter", mode = "lines+markers",
                    line=list(color=cc, dash='solid', width=2.5),
                    marker=list(color=cc, size = 7, line = list(color = lbc, width = 1)),  
                    name = "CovidLP", error_y = list(  type = "data",  thickness=2,
                    width=7,  color=cc, symmetric = FALSE,
                    array = c(estado_LP_RGSul$sup - estado_LP_RGSul$med),
                    arrayminus = c(estado_LP_RGSul$med - estado_LP_RGSul$inf)))

fig_estado_RGSul <- fig_estado_RGSul %>% add_trace(x = ~datass, y = ~estado_obs_RGSul$med, 
                    type= "scatter", mode = "lines+markers",
                    line=list(color=cd, dash='solid', width=3),
                    marker=list(color=cd, width=8), name = "Observed Data")

fig_estado_RGSul <- fig_estado_RGSul %>% layout(titlefont=list(size=t), 
                    autosize = F, width = w, height = h, margin = m, 
                    xaxis = list(title = 'Date'), 
                    yaxis = list(title = 'Total Deaths', hoverformat = '.0f'), 
                    font = list(family = "Arial", size = f), 
                    legend = list(orientation = "h", 
                    xanchor = "center", x = 0.5, y = 1.15))

fig_estado_RGSul

fig_estado_RGSul <- fig_estado_RGSul %>% plotly::config(
    toImageButtonOptions = list( format = "svg", filename = "myplot",
    width = 600, height = 400 ) )

fig_estado_RGSul

### Relative error 

med <- real_RS$cumdeath[231:244]
med_ <- as.numeric(y$med[252:265])
MSE_IHME <- MSE(med,med_)

med_LP <- as.numeric(estado_LP_RGSul$med[1:14])
MSE_LP <- MSE(med,med_LP)

erro_IHME <- (abs(med_ - med)/abs(med))*100 
erro_CovidLP <- (abs(med_LP - med)/abs(med))*100
erro_estado <- cbind.data.frame(datas,erro_IHME,erro_CovidLP)

summary(erro_CovidLP)
summary(erro_IHME)

fig_erro_estado_RGSul <- erro_estado %>% plot_ly() %>% add_trace(x = ~datass, 
                         y = ~erro_IHME, type= "bar", marker = list(color = ci), 
                         name = "IHME")

fig_erro_estado_RGSul <- fig_erro_estado_RGSul %>% add_trace(x = ~datass, 
                        y = ~erro_CovidLP, type= "bar",
                        marker = list(color = cc), name = "CovidLP")

fig_erro_estado_RGSul <- fig_erro_estado_RGSul %>% layout(titlefont=list(size=t), 
                         autosize = F, width = w, 
                         height = h, margin = m, 
                         xaxis = list(title = 'Date'), 
                         yaxis = list(title = 'Error', 
                         hoverformat = '.0f'), 
                         font = list(family = "Arial", size = f), 
                         legend = list(orientation = "h",
                         xanchor = "center",  x = 0.5, y = 1.15))

fig_erro_estado_RGSul

fig_erro_estado_RGSul <- fig_erro_estado_RGSul %>% plotly::config(
    toImageButtonOptions = list( format = "svg",
      filename = "myplot", width = 600, height = 400 ) )

fig_erro_estado_RGSul

#############################################
### Figure 14.5 - State of Sao Paulo, Brazil.
### Period: October
#############################################

y = ySP
location_name <- y$location_name
date <- as.Date(y$date)
mean <- y$totdea_mean
LI <- y$totdea_lower
LS <- y$totdea_upper

y <- cbind.data.frame(location_name,date,mean,LI,LS)
colnames(y)=c("local","datas", "med", "LI", "LS")
y <- data.frame(y)
estado_IHME <- data.frame()
estado_IHME= y[252:265,]

file_estado <- readRDS("./data_Brazil_October/file_SP.RDS")
med_SP <- file_estado[["death"]][["short_term"]][["median"]]$`2020-10-12`
inf_SP <- file_estado[["death"]][["short_term"]][["q25"]]$`2020-10-12`
sup_SP <- file_estado[["death"]][["short_term"]][["q975"]]$`2020-10-12`
estado_LP_SP <- cbind(med_SP,inf_SP,sup_SP)
estado_LP_SP <- estado_LP_SP[120:133,]
datas <- estado_IHME$datas
estado_LP_SP <- cbind.data.frame(datas,estado_LP_SP)

real_SP <- read.csv("./data_Brazil_October/bases/STpred_Brazil_SP_d_23102020.csv")
colnames(real_SP)  <- c("data", "cumdeath", "pred25", "predmed", "pred75" )
real_SP <- as.data.frame(real_SP)
med_SP <- real_SP$cumdeath[231:244]
med_SP <- as.data.frame(med_SP)
datas = date[252:265]
estado_obs_SP <- cbind.data.frame(datas,med_SP)

m <- list( l = 100,  r = 100,  b = 100,  t = 100,  pad = 4 )

fig_estado_SP <- estado_IHME %>% plot_ly() %>% add_trace(x = ~datas, y = ~med, type= "scatter", mode = "lines+markers",
                 line=list(color=ci, dash='solid', width=2.5),
                 marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)),  
                 name = "IHME", error_y = list( type = "data", thickness=2, width=7,
                 color=ci, symmetric = FALSE,
                 array = c(estado_IHME$LS - estado_IHME$med),
                 arrayminus = c(estado_IHME$med - estado_IHME$LI)))

fig_estado_SP <- fig_estado_SP %>% add_trace(x = ~estado_LP_SP$datas, y = ~estado_LP_SP$med, type= "scatter", mode = "lines+markers",
                 line=list(color=cc, dash='solid', width=2.5),
                 marker=list(color=cc, size = 7, line = list(color = lbc, width = 1)),  
                 name = "CovidLP", error_y = list( type = "data", thickness=2, width=7,
                 color=cc, symmetric = FALSE,
                 array = c(estado_LP_SP$sup - estado_LP_SP$med),
                 arrayminus = c(estado_LP_SP$med - estado_LP_SP$inf)))

fig_estado_SP <- fig_estado_SP %>% add_trace(x = ~estado_obs_SP$datas, 
                 y = ~estado_obs_SP$med, type= "scatter", mode = "lines+markers",
                 line=list(color=cd, dash='solid', width=3),
                 marker=list(color=cd, width=8), name = "Observed Data")

fig_estado_SP <- fig_estado_SP %>% layout(
  title = "S?o Paulo - Predictions Comparison",
  xaxis = list(title = "date"),  yaxis = list(title = "Total Deaths"))

fig_estado_SP

### Relative error

med <- real_SP$cumdeath[231:244]
med_ <- as.numeric(y$med[252:265])
MSE_IHME <- MSE(med,med_)

med_LP <- as.numeric(estado_LP_SP$med[1:14])
MSE_LP <- MSE(med,med_LP)

erro_IHME <- (abs(med_ - med)/abs(med))*100 
erro_CovidLP <- (abs(med_LP - med)/abs(med))*100
erro_estado <- cbind.data.frame(datas,erro_IHME,erro_CovidLP)

fig_erro_estado_SP <- erro_estado %>% plot_ly() %>% add_trace(x = ~datas, 
                      y = ~erro_IHME, type= "bar", marker = list(color = ci), 
                      name = "IHME")

fig_erro_estado_SP <- fig_erro_estado_SP %>%add_trace(x = ~datas, y = ~erro_CovidLP, 
                      type= "bar",  marker = list(color = cc), name = "CovidLP")

fig_erro_estado_SP <- fig_erro_estado_SP %>% layout(
  title = "S?o Paulo - Absolute Relative Error",
  xaxis = list(title = "date"),
  yaxis = list(title = "Error")
)

fig_erro_estado_SP

##################################################
### Figure 14.5 - State of Rio de Janeiro, Brazil.
### Period: October
##################################################

y = yRJ
location_name <- y$location_name
date <- as.Date(y$date)
mean <- y$totdea_mean
LI <- y$totdea_lower
LS <- y$totdea_upper

y <- cbind.data.frame(location_name,date,mean,LI,LS)
colnames(y)=c("local","datas", "med", "LI", "LS")
y <- data.frame(y)
estado_IHME <- data.frame()
estado_IHME= y[252:265,]

file_estado <- readRDS("./data_Brazil_October/file_RJ.RDS")
med_RJ <- file_estado[["death"]][["short_term"]][["median"]]$`2020-10-12`
inf_RJ <- file_estado[["death"]][["short_term"]][["q25"]]$`2020-10-12`
sup_RJ <- file_estado[["death"]][["short_term"]][["q975"]]$`2020-10-12`
estado_LP_RJ <- cbind(med_RJ,inf_RJ,sup_RJ)
estado_LP_RJ <- estado_LP_RJ[120:133,]
datas <- estado_IHME$datas
estado_LP_RJ <- cbind.data.frame(datas,estado_LP_RJ)

real_RJ <- read.csv("./data_Brazil_October/bases/STpred_Brazil_RJ_d_23102020.csv")
colnames(real_RJ)  <- c("data", "cumdeath", "pred25", "predmed", "pred75" )
real_RJ <- as.data.frame(real_RJ)
med_RJ <- real_RJ$cumdeath[231:244]
med_RJ <- as.data.frame(med_RJ)
datas = date[252:265]
estado_obs_RJ <- cbind.data.frame(datas,med_RJ)

m <- list( l = 100,  r = 100,  b = 100,  t = 100,  pad = 4 )

fig_estado_RJ <- estado_IHME %>% plot_ly() %>% add_trace(x = ~datas, y = ~med, type= "scatter", mode = "lines+markers",
                 line=list(color=ci, dash='solid', width=2.5),
                 marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)),  
                 name = "IHME", error_y = list( type = "data", thickness=2, width=7,
                 color=ci, symmetric = FALSE,
                 array = c(estado_IHME$LS - estado_IHME$med),
                 arrayminus = c(estado_IHME$med - estado_IHME$LI)))

fig_estado_RJ <- fig_estado_RJ %>% add_trace(x = ~estado_LP_RJ$datas, y = ~estado_LP_RJ$med, type= "scatter", mode = "lines+markers",
                 line=list(color=cc, dash='solid', width=2.5),
                 marker=list(color=cc, size = 7, line = list(color = lbc, width = 1)),  
                 name = "CovidLP", error_y = list( type = "data", thickness=2, width=7,
                 color=cc, symmetric = FALSE,
                 array = c(estado_LP_RJ$sup - estado_LP_RJ$med),
                 arrayminus = c(estado_LP_RJ$med - estado_LP_RJ$inf)))

fig_estado_RJ <- fig_estado_RJ %>% add_trace(x = ~estado_obs_RJ$datas, 
                y = ~estado_obs_RJ$med, type= "scatter", mode = "lines+markers",
                line=list(color=cd, dash='solid', width=3),
                marker=list(color=cd, width=8), name = "Observed Data")

fig_estado_RJ <- fig_estado_RJ %>% layout( 
                 title = "Rio de Janeiro - Predictions Comparison",
                 xaxis = list(title = "date"), yaxis = list(title = "Total Deaths") )

fig_estado_RJ

### Relative error

med <- real_RJ$cumdeath[231:244]
med_ <- as.numeric(y$med[252:265])
MSE_IHME <- MSE(med,med_)

med_LP <- as.numeric(estado_LP_RJ$med[1:14])
MSE_LP <- MSE(med,med_LP)

erro_IHME <- (abs(med_ - med)/abs(med))*100 
erro_CovidLP <- (abs(med_LP - med)/abs(med))*100
erro_estado <- cbind.data.frame(datas,erro_IHME,erro_CovidLP)

summary(erro_CovidLP)
summary(erro_IHME)

fig_erro_estado_RJ <- erro_estado %>% plot_ly() %>% add_trace(x = ~datas, 
                      y = ~erro_IHME, type= "bar", marker = list(color = ci), 
                      name = "IHME")

fig_erro_estado_RJ <- fig_erro_estado_RJ %>%add_trace(x = ~datas, 
                      y = ~erro_CovidLP, type= "bar",
                      marker = list(color = cc), name = "CovidLP")

fig_erro_estado_RJ <- fig_erro_estado_RJ %>% layout(
                      title = "Rio de  - Absolute Relative Error",
                      xaxis = list(title = "date"),  yaxis = list(title = "Error"))

fig_erro_estado_RJ

