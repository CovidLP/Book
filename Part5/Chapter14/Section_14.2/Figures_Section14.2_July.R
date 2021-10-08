###---------------------------------------------------------------###
###   Building a Platform for Data-Driven Pandemic Prediction:    ###
###   From Data Modelling to Visualization - the CovidLP Project  ###
###                                                               ###
###   Chapter 14: Comparing predictions.                          ###
###   Section 14.2: Analysis for Brazilian states                 ###
###   Subsection                                                  ###
###   Figure 14.1 - 14.5 (Period of July)                         ###
###                                                               ###
###   Author: the CovidLP Team                                    ###
###---------------------------------------------------------------###

require(dplyr)
require(ggplot2)
require(plotly)
require(MLmetrics)
require(highcharter)

w=650 # width
h=500 # height
t=25  # title size
f=19  # font size

## set colors ##
ci='rgb(160,160,160)' # cor ihme
lbi='rgb(160,160,160)' # line ball ihme
cc='rgb(96, 96, 96)' # cor covidlp
lbc='rgb(96, 96, 96)' # cor ball covidp
cd='rgb(0, 0, 0)' # cor observed data

load("./data_Brazil_July/data_july.RData")

################################################
### Figure 14.1 - State of Minas Gerais, Brazil.
### Period: July
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
estado_IHME= y[153:166,]
file_MG <- readRDS("./data_Brazil_July/file_MG.RDS")
med_MG <- file_MG[["death"]][["short_term"]][["median"]]$`2020-07-06.x`
inf_MG <- file_MG[["death"]][["short_term"]][["q25"]]$`2020-07-06.x`
sup_MG <- file_MG[["death"]][["short_term"]][["q975"]]$`2020-07-06.x`

estado_LP_MG <- cbind(med_MG,inf_MG,sup_MG)
estado_LP_MG <- estado_LP_MG[23:36,]
datas <- estado_IHME$datas
estado_LP_MG <- cbind.data.frame(datas,estado_LP_MG)

med <- c(1201,1230,1282,1355,1445,1504,1550,1576,1615,1688,1752,1834,1904,1964)
med <- as.data.frame(med)
datas = date[153:166]
estado_obs_MG <- cbind.data.frame(datas,med)

m <- list(l = 100, r = 100, b = 100, t = 100, pad = 4)

datass <- format(as.Date(datas), "%d-%b")
fig_estado <- estado_IHME %>% plot_ly() %>% add_trace(x = ~datass, y = ~med, type= "scatter", mode = "lines+markers",
              line=list(color=ci, dash='solid', width=2.5),
              marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)),  
              name = "IHME", error_y = list( type = "data", thickness=2, width=7,
              color=ci, symmetric = FALSE, array = c(estado_IHME$LS - estado_IHME$med),
              arrayminus = c(estado_IHME$med - estado_IHME$LI)))

fig_estado <- fig_estado %>% add_trace(x = ~datass, y = ~estado_LP_MG$med, type= "scatter", mode = "lines+markers",
              line=list(color=cc, dash='solid', width=2.5),
              marker=list(color=cc, size = 7, line = list(color = lbc, width = 1)),  
              name = "CovidLP", error_y = list( type = "data", thickness=2, width=7,
              color=cc, symmetric = FALSE, array = c(estado_LP_MG$sup - estado_LP_MG$med),
              arrayminus = c(estado_LP_MG$med - estado_LP_MG$inf)))

fig_estado <- fig_estado %>% add_trace(x = ~datass, y = ~estado_obs_MG$med, 
              type= "scatter", mode = "lines+markers",
              line=list(color=cd, dash='solid', width=3),
              marker=list(color=cd, width=8), name = "Observed Data")

fig_estado <- fig_estado %>% layout(titlefont=list(size=t), 
              autosize = F, width = w, height = h, margin = m, 
              xaxis = list(title = 'Date'), 
              yaxis = list(title = 'Total Deaths', hoverformat = '.0f'), 
              font = list(family = "Arial", size = f), 
              legend = list(orientation = "h", xanchor = "center", x=0.5,y=1.15))

fig_estado

fig_estado <- fig_estado%>%
  plotly::config( toImageButtonOptions = list( format = "svg", filename = "myplot",
      width = 600, height = 400 ) )

fig_estado

### Relative error

med <- c(1201,1230,1282,1355,1445,1504,1550,1576,1615,1688,1752,1834,1904,1964)
med_ <- as.numeric(y$med[153:166])
MSE_IHME <- MSE(med,med_)

med_LP <- as.numeric(estado_LP_MG$med[1:14])
MSE_LP <- MSE(med,med_LP)

erro_IHME <- (abs(med_ - med)/abs(med))*100 
erro_CovidLP <- (abs(med_LP - med)/abs(med))*100
erro_estado <- cbind.data.frame(datas,erro_IHME,erro_CovidLP)

summary(erro_CovidLP)
summary(erro_IHME)

fig_erro_estado_MG <- erro_estado %>% plot_ly() %>% add_trace(x = ~datass, 
                      y = ~erro_IHME, type= "bar", marker = list(color = ci), name = "IHME")

fig_erro_estado_MG <- fig_erro_estado_MG %>%add_trace(x = ~datass, 
                      y = ~erro_CovidLP, type= "bar", marker = list(color = cc), name = "CovidLP")

fig_erro_estado_MG <- fig_erro_estado_MG %>% layout(titlefont=list(size=t), 
                      autosize = F, width = w, height = h, margin = m, 
                      xaxis = list(title = 'Date'), yaxis = list(title = 'Error', 
                      hoverformat = '.0f'), font = list(family = "Arial", 
                      size = f), legend = list(orientation = "h",
                      xanchor = "center", x=0.5,y=1.15))

fig_erro_estado_MG <- fig_erro_estado_MG %>% plotly::config( toImageButtonOptions = list(
      format = "svg", filename = "myplot", width = 600, height = 400 ) )

fig_erro_estado_MG

#########################################
### Figure 14.2 - State of Bahia, Brazil.
### Period: July
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
estado_IHME= y[153:166,]
file_BA <- readRDS("./data_Brazil_July/file_BA.RDS")
med_BA <- file_BA[["death"]][["short_term"]][["median"]]$`2020-07-06.x`
inf_BA <- file_BA[["death"]][["short_term"]][["q25"]]$`2020-07-06.x`
sup_BA <- file_BA[["death"]][["short_term"]][["q975"]]$`2020-07-06.x`

estado_LP_BA <- cbind(med_BA,inf_BA,sup_BA)
estado_LP_BA <- estado_LP_BA[23:36,]
datas <- estado_IHME$datas
estado_LP_BA <- cbind.data.frame(datas,estado_LP_BA)

med <- c(2107,2168,2216,2277,2328,2383,2436,2483,2535,2584,2638,2693,2738,2793)
med <- as.data.frame(med)
datas = date[153:166]
estado_obs_BA <- cbind.data.frame(datas,med)

m <- list( l = 100, r = 100, b = 100, t = 100, pad = 4 )

datass <- format(as.Date(datas), "%d-%h")

fig_estado <- estado_IHME %>% plot_ly() %>% add_trace(x = ~datass, y = ~med, type= "scatter", mode = "lines+markers",
              line=list(color=ci, dash='solid', width=2.5),
              marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)),  
              name = "IHME", error_y = list( type = "data",  thickness=2, width=7,
              color=ci, symmetric = FALSE,
              array = c(estado_IHME$LS - estado_IHME$med),
              arrayminus = c(estado_IHME$med - estado_IHME$LI)))

fig_estado <- fig_estado %>% add_trace(x = ~datass, y = ~estado_LP_BA$med, type= "scatter", mode = "lines+markers",
              line=list(color=cc, dash='solid', width=2.5),
              marker=list(color=cc, size = 7, line = list(color = lbc, width = 1)),  
              name = "CovidLP", error_y = list( type = "data", thickness=2, width=7,
              color=cc, symmetric = FALSE,
              array = c(estado_LP_BA$sup - estado_LP_BA$med),
              arrayminus = c(estado_LP_BA$med - estado_LP_BA$inf)))

fig_estado <- fig_estado %>% add_trace(x = ~datass, y = ~estado_obs_BA$med, 
              type= "scatter", mode = "lines+markers",
              line=list(color=cd, dash='solid', width=3),
              marker=list(color=cd, width=8), name = "Observed Data")

fig_estado <- fig_estado %>% layout(titlefont=list(size=t), autosize = F, 
              width = w, height = h, margin = m, xaxis = list(title = 'Date'), 
              yaxis = list(title = 'Total Deaths', hoverformat = '.0f'), 
              font = list(family = "Arial", size = f), 
              legend = list(orientation = "h",
              xanchor = "center", x=0.5,y=1.15))

fig_estado

fig_estado <- fig_estado %>% plotly::config( toImageButtonOptions = list(
      format = "svg", filename = "myplot", width = 600, height = 400 ) )

fig_estado

### Relative error 

med <- c(2107,2168,2216,2277,2328,2383,2436,2483,2535,2584,2638,2693,2738,2793)
med_ <- as.numeric(y$med[153:166])
MSE_IHME <- MSE(med,med_)

med_LP <- as.numeric(estado_LP_BA$med[1:14])
MSE_LP <- MSE(med,med_LP)

erro_IHME <- (abs(med_ - med)/abs(med))*100 
erro_CovidLP <- (abs(med_LP - med)/abs(med))*100
erro_estado <- cbind.data.frame(datas,erro_IHME,erro_CovidLP)

summary(erro_CovidLP)
summary(erro_IHME)
fig_erro <- erro_estado %>% plot_ly() %>% add_trace(x = ~datass, y = ~erro_IHME, 
            type= "bar", marker = list(color = ci), name = "IHME")

fig_erro <- fig_erro %>%add_trace(x = ~datass, y = ~erro_CovidLP, type= "bar",
            marker = list(color = cc), name = "CovidLP")

fig_erro <- fig_erro %>% layout(titlefont=list(size=t), autosize = F, width = w, 
            height = h, margin = m, xaxis = list(title = 'Date'), 
            yaxis = list(title = 'Error', hoverformat = '.0f'), 
            font = list(family = "Arial", size = f),legend = list(orientation = "h",
            xanchor = "center", x=0.5,y=1.15))

fig_erro

fig_erro <- fig_erro %>% plotly::config(
    toImageButtonOptions = list( format = "svg",
      filename = "myplot", width = 600, height = 400 ) )

fig_erro

############################################
### Figure 14.3 - State of Maranhao, Brazil.
### Period: July
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
estado_IHME= y[153:166,]
file_MA <- readRDS("./data_Brazil_July/file_MA.RDS")
med_MA <- file_MA[["death"]][["short_term"]][["median"]]$`2020-07-06.x`
inf_MA <- file_MA[["death"]][["short_term"]][["q25"]]$`2020-07-06.x`
sup_MA <- file_MA[["death"]][["short_term"]][["q975"]]$`2020-07-06.x`

estado_LP_MA <- cbind(med_MA,inf_MA,sup_MA)
estado_LP_MA <- estado_LP_MA[23:36,]
datas <- estado_IHME$datas
estado_LP_MA <- cbind.data.frame(datas,estado_LP_MA)

med <- c(2219,2250,2286,2324,2357,2392,2426,2463,2501,2536,2572,2608,2640,2676)
med <- as.data.frame(med)
datas = date[153:166]
estado_obs_MA <- cbind.data.frame(datas,med)

m <- list(  l = 100,  r = 100,  b = 100,  t = 100,  pad = 4 )

datass <- format(as.Date(datas), "%d-%b")

fig_estado <- estado_IHME %>% plot_ly() %>% add_trace(x = ~datass, y = ~med, type= "scatter", mode = "lines+markers",
              line=list(color=ci, dash='solid', width=2.5),
              marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)),  
              name = "IHME", error_y = list( type = "data", thickness=2, width=7,
              color=ci, symmetric = FALSE, array = c(estado_IHME$LS - estado_IHME$med),
              arrayminus = c(estado_IHME$med - estado_IHME$LI)))

fig_estado <- fig_estado %>% add_trace(x = ~datass, y = ~estado_LP_MA$med, type= "scatter", mode = "lines+markers",
              line=list(color=cc, dash='solid', width=2.5),
              marker=list(color=cc, size = 7, line = list(color = lbc, width = 1)),  
              name = "CovidLP", error_y = list( type = "data", thickness=2, width=7,
              color=cc, symmetric = FALSE,
              array = c(estado_LP_MA$sup - estado_LP_MA$med),
              arrayminus = c(estado_LP_MA$med - estado_LP_MA$inf)))

fig_estado <- fig_estado %>% add_trace(x = ~datass, y = ~estado_obs_MA$med, 
              type= "scatter", mode = "lines+markers",line=list(color=cd, 
              dash='solid', width=3),marker=list(color=cd, width=8), 
              name = "Observed Data")

fig_estado <- fig_estado %>% layout(titlefont=list(size=t), autosize = F, width = w, 
              height = h, margin = m, xaxis = list(title = 'Date'), 
              yaxis = list(title = 'Total Deaths', hoverformat = '.0f'), 
              font = list(family = "Arial", size = f), 
              legend = list(orientation = "h", xanchor = "center", x = 0.5, y = 1.15))

fig_estado

fig_estado <- fig_estado %>% plotly::config( toImageButtonOptions = list(
      format = "svg", filename = "myplot", width = 600, height = 400 ))

fig_estado

### Relative error 

med <- c(2219,2250,2286,2324,2357,2392,2426,2463,2501,2536,2572,2608,2640,2676)
med_ <- as.numeric(y$med[153:166])
MSE_IHME <- MSE(med,med_)

med_LP <- as.numeric(estado_LP_MA$med[1:14])
MSE_LP <- MSE(med,med_LP)

erro_IHME <- (abs(med_ - med)/abs(med))*100 
erro_CovidLP <- (abs(med_LP - med)/abs(med))*100
erro_estado <- cbind.data.frame(datas,erro_IHME,erro_CovidLP)

summary(erro_CovidLP)
summary(erro_IHME)

fig_erro_estado_MA <- erro_estado %>% plot_ly() %>% add_trace(x = ~datass, 
                      y = ~erro_IHME, type= "bar", marker = list(color = ci), name = "IHME")

fig_erro_estado_MA <- fig_erro_estado_MA %>%add_trace(x = ~datass, y = ~erro_CovidLP, 
                      type= "bar", marker = list(color = cc), name = "CovidLP")

fig_erro_estado_MA <- fig_erro_estado_MA %>% layout(titlefont=list(size=t), 
                      autosize = F, width = w, height = h, margin = m, 
                      xaxis = list(title = 'Date'), yaxis = list(title = 'Error', 
                      hoverformat = '.0f'), font = list(family = "Arial", size = f), 
                      legend = list(orientation = "h", xanchor = "center", x = 0.5, y = 1.15))

fig_erro_estado_MA

fig_erro_estado_MA <- fig_erro_estado_MA %>% plotly::config(
    toImageButtonOptions = list( format = "svg",
    filename = "myplot", width = 600, height = 400 ) )

fig_erro_estado_MA


#####################################################
### Figure 14.4 - State of Rio Grande do Sul, Brazil.
### Period: July
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
estado_IHME= y[153:166,]
file_RS <- readRDS("./data_Brazil_July/file_RS.RDS")
med_RS <- file_RS[["death"]][["short_term"]][["median"]]$`2020-07-06.x`
inf_RS <- file_RS[["death"]][["short_term"]][["q25"]]$`2020-07-06.x`
sup_RS <- file_RS[["death"]][["short_term"]][["q975"]]$`2020-07-06.x`

estado_LP_RS <- cbind(med_RS,inf_RS,sup_RS)
estado_LP_RS <- estado_LP_RS[23:36,]
datas <- estado_IHME$datas
estado_LP_RS <- cbind.data.frame(datas,estado_LP_RS)

med <- c(727,759,791,825,870,919,943,962,995,1060,1101,1141,1166,1229)
med <- as.data.frame(med)
datas = date[153:166]
estado_obs_RS <- cbind.data.frame(datas,med)

m <- list(  l = 100,  r = 100,  b = 100,  t = 100,  pad = 4 )

datass <- format(as.Date(datas), "%d-%b")

fig_estado <- estado_IHME %>% plot_ly() %>% add_trace(x = ~datass, y = ~med, type= "scatter", mode = "lines+markers",
            line=list(color=ci, dash='solid', width=2.5),
            marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)),  
            name = "IHME", error_y = list( type = "data", thickness=2, width=7,
            color=ci, symmetric = FALSE,
            array = c(estado_IHME$LS - estado_IHME$med),
            arrayminus = c(estado_IHME$med - estado_IHME$LI)))

fig_estado <- fig_estado %>% add_trace(x = ~datass, y = ~estado_LP_RS$med, type= "scatter", mode = "lines+markers",
              line=list(color=cc, dash='solid', width=2.5),
              marker=list(color=cc, size = 7, line = list(color = lbc, width = 1)),  
              name = "CovidLP", error_y = list(type = "data", thickness=2,
              width=7, color=cc, symmetric = FALSE,
              array = c(estado_LP_RS$sup - estado_LP_RS$med),
              arrayminus = c(estado_LP_RS$med - estado_LP_RS$inf)))

fig_estado <- fig_estado %>% add_trace(x = ~datass, y = ~estado_obs_RS$med, 
              type= "scatter", mode = "lines+markers", 
              line = list(color=cd, dash='solid', width=3), 
              marker=list(color=cd, width=8), name = "Observed Data")

fig_estado <- fig_estado %>% layout(titlefont=list(size=t), autosize = F, width = w, 
              height = h, margin = m, xaxis = list(title = 'Date'), 
              yaxis = list(title = 'Total Deaths', hoverformat = '.0f'), 
              font = list(family = "Arial", size = f), 
              legend = list(orientation = "h", xanchor = "center", x = 0.5, y = 1.15 ))

fig_estado

fig_estado <- fig_estado %>% plotly::config( toImageButtonOptions = list(
      format = "svg", filename = "myplot", width = 600, height = 400 ))

fig_estado

### Relative error

med <- c(727,759,791,825,870,919,943,962,995,1060,1101,1141,1166,1229)
med_ <- as.numeric(y$med[153:166])
MSE_IHME <- MSE(med,med_)

med_LP <- as.numeric(estado_LP_RS$med[1:14])
MSE_LP <- MSE(med,med_LP)

erro_IHME <- (abs(med_ - med)/abs(med))*100 
erro_CovidLP <- (abs(med_LP - med)/abs(med))*100
erro_estado <- cbind.data.frame(datas,erro_IHME,erro_CovidLP)

summary(erro_CovidLP)
summary(erro_IHME)

fig_erro_estado_RS <- erro_estado %>% plot_ly() %>% add_trace(x = ~datass, 
                      y = ~erro_IHME, type= "bar", marker = list(color = ci), name = "IHME")

fig_erro_estado_RS <- fig_erro_estado_RS %>%add_trace(x = ~datass, 
                      y = ~erro_CovidLP, type= "bar", marker = list(color = cc), name = "CovidLP")

fig_erro_estado_RS <- fig_erro_estado_RS %>% layout(titlefont=list(size=t), 
                      autosize = F, width = w, height = h, margin = m, 
                      xaxis = list(title = 'Date'), yaxis = list(title = 'Error', 
                      hoverformat = '.0f'), font = list(family = "Arial", size = f),  
                      legend = list(orientation = "h", xanchor = "center", x = 0.5, y = 1.15))

fig_erro_estado_RS

fig_erro_estado_RS <- fig_erro_estado_RS %>% plotly::config( toImageButtonOptions = list(
      format = "svg", filename = "myplot", width = 600,height = 400 ))

fig_erro_estado_RS

#############################################
### Figure 14.5 - State of Sao Paulo, Brazil.
### Period: July
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
estado_IHME= y[153:166,]
file_SP <- readRDS("./data_Brazil_July/file_SP.RDS")
med_SP <- file_SP[["death"]][["short_term"]][["median"]]$`2020-07-06.x`
inf_SP <- file_SP[["death"]][["short_term"]][["q25"]]$`2020-07-06.x`
sup_SP <- file_SP[["death"]][["short_term"]][["q975"]]$`2020-07-06.x`

estado_LP_SP <- cbind(med_SP,inf_SP,sup_SP)
estado_LP_SP <- estado_LP_SP[23:36,]
datas <- estado_IHME$datas
estado_LP_SP <- cbind.data.frame(datas,estado_LP_SP)

med <- c(16078,16134,16475,16788,17118,17442,17702,17848,17907,18324,18640,19038,19377,19647)
med <- as.data.frame(med)
datas = date[153:166]
estado_obs_SP <- cbind.data.frame(datas,med)

m <- list(  l = 100,  r = 100,  b = 100,  t = 100,  pad = 4 )

datass <- format(as.Date(datas), "%d-%b")

fig_estado <- estado_IHME %>% plot_ly() %>% add_trace(x = ~datass, y = ~med, type= "scatter", mode = "lines+markers",
              line=list(color=ci, dash='solid', width=2.5),
              marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)),  
              name = "IHME", error_y = list( type = "data", thickness=2, width=7,
              color=ci, symmetric = FALSE,
              array = c(estado_IHME$LS - estado_IHME$med),
              arrayminus = c(estado_IHME$med - estado_IHME$LI)))

fig_estado <- fig_estado %>% add_trace(x = ~datass, y = ~estado_LP_SP$med, type= "scatter", mode = "lines+markers",
              line=list(color=cc, dash='solid', width=2.5),
              marker=list(color=cc, size = 7, line = list(color = lbc, width = 1)),  
              name = "CovidLP", error_y = list( type = "data",  thickness=2,  width=7,
              color=cc,  symmetric = FALSE,
              array = c(estado_LP_SP$sup - estado_LP_SP$med),
              arrayminus = c(estado_LP_SP$med - estado_LP_SP$inf)))

fig_estado <- fig_estado %>% add_trace(x = ~datass, 
              y = ~estado_obs_SP$med, type= "scatter", mode = "lines+markers",
              line=list(color=cd, dash='solid', width=3),marker=list(color=cd, width=8), 
              name = "Observed Data")

fig_estado <- fig_estado %>% layout(titlefont=list(size=t), 
              autosize = F, width = w, height = h, margin = m,  
              xaxis = list(title = 'Date'), 
              yaxis = list(title = 'Total Deaths', hoverformat = '.0f'), 
              font = list(family = "Arial", size = f),  legend = list(orientation = "h",
              xanchor = "center", x = 0.5, y = 1.15))

fig_estado

fig_estado <- fig_estado %>% plotly::config( toImageButtonOptions = list(
      format = "svg", filename = "myplot", width = 600, height = 400 ) )

fig_estado

### Relative error 

med <- c(16078,16134,16475,16788,17118,17442,17702,17848,17907,18324,18640,19038,19377,19647)
med_ <- as.numeric(y$med[153:166])
MSE_IHME <- MSE(med,med_)

med_LP <- as.numeric(estado_LP_SP$med[1:14])
MSE_LP <- MSE(med,med_LP)

erro_IHME <- (abs(med_ - med)/abs(med))*100 
erro_CovidLP <- (abs(med_LP - med)/abs(med))*100
erro_estado <- cbind.data.frame(datas,erro_IHME,erro_CovidLP)

fig_erro_estado_SP <- erro_estado %>% plot_ly() %>% add_trace(x = ~datass, 
                      y = ~erro_IHME, type= "bar", marker = list(color = ci), name = "IHME")

fig_erro_estado_SP <- fig_erro_estado_SP %>%add_trace(x = ~datass, y = ~erro_CovidLP,
                      type= "bar", marker = list(color = cc), name = "CovidLP")

fig_erro_estado_SP <- fig_erro_estado_SP %>% layout(titlefont=list(size=t), 
                      autosize = F, width = w, height = h, margin = m, 
                      xaxis = list(title = 'Date'), 
                      yaxis = list(title = 'Error', hoverformat = '.0f'), 
                      font = list(family = "Arial", size = f), 
                      legend = list(orientation = "h", 
                      xanchor = "center", x = 0.5, y = 1.15))

fig_erro_estado_SP

fig_erro_estado_SP <- fig_erro_estado_SP%>% plotly::config( 
      toImageButtonOptions = list( format = "svg", filename = "myplot", 
      width = 600, height = 400 ))

fig_erro_estado_SP


##################################################
### Figure 14.5 - State of Rio de Janeiro, Brazil.
### Period: July
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
estado_IHME= y[153:166,]
file_RJ <- readRDS("./data_Brazil_July/file_RJ.RDS")
med_RJ <- file_RJ[["death"]][["short_term"]][["median"]]$`2020-07-06.x`
inf_RJ <- file_RJ[["death"]][["short_term"]][["q25"]]$`2020-07-06.x`
sup_RJ <- file_RJ[["death"]][["short_term"]][["q975"]]$`2020-07-06.x`

estado_LP_RJ <- cbind(med_RJ,inf_RJ,sup_RJ)
estado_LP_RJ <- estado_LP_RJ[23:36,]
datas <- estado_IHME$datas
estado_LP_RJ <- cbind.data.frame(datas,estado_LP_RJ)

med <- c(10667,10698,10881,10970,11115,11280,11406,11415,11474,11624,11757,11849,11919,11919)
med <- as.data.frame(med)
datas = date[153:166]
estado_obs_RJ <- cbind.data.frame(datas,med)

m <- list( l = 100, r = 100, b = 100, t = 100, pad = 4 )

datass <- format(as.Date(datas), "%d-%b")

fig_estado <- estado_IHME %>% plot_ly() %>% add_trace(x = ~datass, y = ~med, type= "scatter", mode = "lines+markers",
              line=list(color=ci, dash='solid', width=2.5),
              marker=list(color=ci,  size = 7,line = list(color = lbi,width = 1)),  
              name = "IHME", error_y = list( type = "data", thickness=2, width=7,
              color=ci, symmetric = FALSE,
              array = c(estado_IHME$LS - estado_IHME$med),
              arrayminus = c(estado_IHME$med - estado_IHME$LI)))

fig_estado <- fig_estado %>% add_trace(x = ~datass, y = ~estado_LP_RJ$med, type= "scatter", mode = "lines+markers",
              line=list(color=cc, dash='solid', width=2.5),
              marker=list(color=cc, size = 7, line = list(color = lbc, width = 1)),  
              name = "CovidLP",  error_y = list( type = "data", thickness=2, width=7,
              color=cc, symmetric = FALSE,
              array = c(estado_LP_RJ$sup - estado_LP_RJ$med),
              arrayminus = c(estado_LP_RJ$med - estado_LP_RJ$inf)))

fig_estado <- fig_estado %>% add_trace(x = ~datass, y = ~estado_obs_RJ$med, 
              type= "scatter", mode = "lines+markers",
              line=list(color=cd, dash='solid', width=3),
              marker=list(color=cd, width=8), name = "Observed Data")

fig_estado <- fig_estado %>% layout(titlefont=list(size=t), autosize = F, width = w, 
              height = h, margin = m, xaxis = list(title = 'Date'), 
              yaxis = list(title = 'Total Deaths', hoverformat = '.0f'), 
              font = list(family = "Arial", size = f), 
              legend = list(orientation = "h",
              xanchor = "center", x = 0.5, y = 1.15))

fig_estado

fig_estado <- fig_estado %>% plotly::config( toImageButtonOptions = list(
      format = "svg", filename = "myplot", width = 600, height = 400 ) )

fig_estado

### Relative error

med <- c(10667,10698,10881,10970,11115,11280,11406,11415,11474,11624,11757,11849,11919,11919)
med_ <- as.numeric(y$med[153:166])
MSE_IHME <- MSE(med,med_)

med_LP <- as.numeric(estado_LP_RJ$med[1:14])
MSE_LP <- MSE(med,med_LP)

erro_IHME <- (abs(med_ - med)/abs(med))*100 
erro_CovidLP <- (abs(med_LP - med)/abs(med))*100
erro_estado <- cbind.data.frame(datas,erro_IHME,erro_CovidLP)

summary(erro_CovidLP)
summary(erro_IHME)

fig_erro_estado_RJ <- erro_estado %>% plot_ly() %>% add_trace(x = ~datass, 
                      y = ~erro_IHME, type= "bar", marker = list(color = ci), 
                      name = "IHME")

fig_erro_estado_RJ <- fig_erro_estado_RJ %>%add_trace(x = ~datass, y = ~erro_CovidLP, 
                      type= "bar",  marker = list(color = cc), name = "CovidLP")

fig_erro_estado_RJ <- fig_erro_estado_RJ %>% layout(titlefont=list(size=t), 
                      autosize = F, width = w, height = h, margin = m, 
                      xaxis = list(title = 'Date'), 
                      yaxis = list(title = 'Error', hoverformat = '.0f'), 
                      font = list(family = "Arial", size = f), 
                      legend = list(orientation = "h",
                      xanchor = "center", x = 0.5, y = 1.15))

fig_erro_estado_RJ

fig_erro_estado_RJ <- fig_erro_estado_RJ %>% plotly::config( toImageButtonOptions = list(
      format = "svg", filename = "myplot", width = 600, height = 400 ) )

fig_erro_estado_RJ

