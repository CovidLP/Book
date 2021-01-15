###---------------------------------------------------------------###
###   Building a Platform for Data-Driven Pandemic Prediction:    ###
###   From Data Modelling to Visualization - the CovidLP Project  ###
###                                                               ###
###   Chapter 10: Investigating inference results                 ###
###   Section 10.4  Practical situation                           ###
###   Subsection 10.4.1 Overall comparison                        ###
###   Figure 10.4                                                 ###
###                                                               ###
###   Author: the CovidLP Team                                    ###
###---------------------------------------------------------------###

### Removing alocated objects
rm(list = ls())

###-----------------------------
### Packages
###-----------------------------
library(data.table)
library(ggplot2)
library(tidyverse)

###-----------------------------
### Setting seed
set.seed(123456789)
###-----------------------------

###-----------------------------
### Setting directories
data_directory<- "Part4/Chapter10/data/"
results_directory<- "Part4/Chapter10/results/"
###-----------------------------

###-----------------------------
### Graphical specifications (you may need to adapt these settings)
linetype_plot<- 1.2
size_line_plot<- 1.2
size_plot<- 15
vjust_plot<- 1.8

###-----------------------------

###-----------------------------###-----------------------------###-----------------------------###

###-----------------###
###   FIGURE 10.3   ###
###-----------------###

country_name<- "Argentina"

### Observed data
Y<- readRDS(file = paste0(data_directory, "data_Argentina.rds"))

### Reading results from GitHub
baseURL_countries<- paste0("https://github.com/gabrieloa/covid/blob/master/file_", country_name, ".RDS?raw=true")
results_countries<- readRDS(file=url(baseURL_countries))

### R system in English
Sys.setlocale("LC_TIME", "C")

### Auxiliary data frame with results of daily updated prediction obtained by the CovidLP project
df_aux<- results_countries$confirmed$long_term$median

### Dates to evaluate (to avoid incorrect recordings)
dates_eval<- as.Date(intersect(x = colnames(df_aux),
                               y = as.character(seq(from = as.Date(colnames(df_aux)[2]),
                                                    to = as.Date("2020-10-31"), by = "days"))))

### Data frame to calculate relative error
df<- df_aux[, colnames(df_aux) %in% c("date", as.character(dates_eval))]

###------------------------------###------------------------------###------------------------------###

###-----------------------------
### Coverage percentage
###-----------------------------

df_aux_li<- results_countries$confirmed$long_term$q25
df_aux_ls<- results_countries$confirmed$long_term$q975

df_li<- df_aux_li[, colnames(df_aux_li) %in% c("date", as.character(dates_eval))]
df_ls<- df_aux_ls[, colnames(df_aux_ls) %in% c("date", as.character(dates_eval))]

### Long-term comparison: number of steps head to display CP
k_steps_ahead_long<- length(dates_eval)

### Short-term comparison: number of steps head to display CP
k_steps_ahead_short<- 7

### Case type: n_new or d_new
type_case<- "n_new"

###------------------------------

### Beginning data frame to use in ggplot
k<- 1

### Builds data frame with observations and prediction in date dates_eval[k]
df_cp_long<- merge(x = merge(x = Y[,c("date", type_case)],
                             y = df_li[, colnames(df_li) %in% c("date", as.character(dates_eval)[k])],
                          by = "date"),
                   y = df_ls[, colnames(df_ls) %in% c("date", as.character(dates_eval)[k])],
                   by = "date")
colnames(df_cp_long)<- c("date", type_case, "li", "ls")
df_cp_long<- df_cp_long[complete.cases(df_cp_long),]

### Calculates RE for long-term comparison
df_long_ggplot<- df_cp_long
df_long_ggplot$updation_date<- rep(x = dates_eval[k], times = nrow(df_cp_long))
df_long_ggplot$cp_long<- as.numeric(data.table::between(x = df_long_ggplot[,type_case], lower = df_cp_long$li,
                                                        upper = df_cp_long$ls))
                              

### Calculates RE for short-term comparison
df_short_ggplot<- df_cp_long[1:k_steps_ahead_short,]
df_short_ggplot$updation_date<- rep(x = dates_eval[k], times = k_steps_ahead_short)
df_short_ggplot$cp_short<- as.numeric(data.table::between(x = df_short_ggplot[,type_case],
                                                          lower = df_short_ggplot$li, upper = df_short_ggplot$ls))


### Repeating for all dates that will be evaluated (dates_eval)
for(k in 2:k_steps_ahead_long){
  
  ### Builds data frame with observations and prediction in date dates_eval[k]
  df_relative_error<- merge(x = Y[,c("date", type_case)],
                            y = df[, colnames(df) %in% c("date", as.character(dates_eval)[k])],
                            by = "date")
  colnames(df_relative_error)<- c("date", type_case, "median")
  df_relative_error<- df_relative_error[complete.cases(df_relative_error),]
  
  ### Calculates RE for long-term comparison
  df_long_ggplot_aux<- df_relative_error
  df_long_ggplot_aux$updation_date<- rep(x = dates_eval[k], times = nrow(df_relative_error))
  df_long_ggplot_aux$re_long<- ((df_relative_error$median - df_relative_error[,type_case]) /
                                  (abs(df_relative_error[,type_case]) + 1))*100
  df_long_ggplot<- merge(x = df_long_ggplot, y = df_long_ggplot_aux, all = TRUE)
  
  ### We want only full weeks ahead
  if(nrow(df_relative_error) >= k_steps_ahead_short){
    ### Calculates RE for short-term comparison
    df_short_ggplot_aux<- df_relative_error[1:k_steps_ahead_short,]
    df_short_ggplot_aux$updation_date<- rep(x = dates_eval[k], times = k_steps_ahead_short)
    df_short_ggplot_aux$re_short<- (((df_relative_error$median - df_relative_error[,type_case]) /
                                       (abs(df_relative_error[,type_case]) + 1))*100)[1:k_steps_ahead_short]
    df_short_ggplot<- merge(x = df_short_ggplot, y = df_short_ggplot_aux, all = TRUE)
  }
  
}

###-----------------------------
### Coverage percentage - 7 days ahead
###-----------------------------

df_aux_li_v2<- results_countries$confirmed$long_term$q25
df_aux_ls_v2<- results_countries$confirmed$long_term$q975
dates_eval_v2<- as.Date(intersect(x = intersect(x = intersect(x = colnames(df_aux_li_v2), y = colnames(df_aux_ls_v2)),
                                                y = as.character(seq(from = as.Date(colnames(df_aux_li_v2)[2]), to = as.Date("2020-10-31"),
                                                                     by = "days"))),
                                  y = as.character(seq(from = as.Date(colnames(df_aux_ls_v2)[2]), to = as.Date("2020-10-31"), by = "days"))))
df_li_v2<- df_aux_li_v2[, colnames(df_aux_li_v2) %in% c("date", as.character(dates_eval_v2))]
df_ls_v2<- df_aux_ls_v2[, colnames(df_aux_ls_v2) %in% c("date", as.character(dates_eval_v2))]

last(which(dates_eval_v2 <= last(dates_eval_v2) - 7 + 1)) # ultimo dia com 7 dias de previsao
k_steps_ahead_v2<- max(last(which(dates_eval_v2 <= last(dates_eval_v2) - 7)))

pc_v2<- rep(x = NA, times = k_steps_ahead_v2)
names(pc_v2)<- dates_eval_v2[1:k_steps_ahead_v2]

for(k in 1:k_steps_ahead_v2){
  
  df_cp_v2<- merge(x = merge(x = Y[,c("date", "n_new")],
                             y = df_li_v2[, colnames(df_li_v2) %in% c("date", as.character(dates_eval_v2)[k])], by = "date"),
                   y = df_ls_v2[, colnames(df_ls_v2) %in% c("date", as.character(dates_eval_v2)[k])], by = "date")
  colnames(df_cp_v2)<- c("date", "n_new", "q25", "q975")
  df_cp_v2<- df_cp_v2[complete.cases(df_cp_v2),]
  df_cp_v2<- df_cp_v2[df_cp_v2$date %in% seq(from = df_cp_v2$date[1], to = df_cp_v2$date[1]+6, by = "days"),]
  
  pc_aux_v2<- as.numeric(between(x = df_cp_v2$n_new, df_cp_v2$q25, df_cp_v2$q975))
  pc_v2[k]<- (sum(pc_aux_v2) / length(pc_aux_v2))*100
  rm(pc_aux_v2)
}