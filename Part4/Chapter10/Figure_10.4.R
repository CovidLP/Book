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
df_cp<- merge(x = merge(x = Y[,c("date", type_case)],
                             y = df_li[, colnames(df_li) %in% c("date", as.character(dates_eval)[k])],
                          by = "date"),
                   y = df_ls[, colnames(df_ls) %in% c("date", as.character(dates_eval)[k])],
                   by = "date")
colnames(df_cp)<- c("date", type_case, "li", "ls")
df_cp<- df_cp[complete.cases(df_cp),]
df_cp$cp_long<- as.numeric(data.table::between(x = df_cp[,type_case], lower = df_cp$li,
                                                    upper = df_cp$ls))

df_ggplot<- data.frame(update_date = dates_eval[k],
                       cp_long = ( sum(df_cp$cp_long) / nrow(df_cp))*100,
                       cp_short = ( sum(df_cp$cp_long[1:k_steps_ahead_short]) / k_steps_ahead_short)*100)
                              

### Repeating for all dates that will be evaluated (dates_eval)
for(k in 2:k_steps_ahead_long){
  
  ### Builds data frame with observations and prediction in date dates_eval[k]
  df_cp<- merge(x = merge(x = Y[,c("date", type_case)],
                          y = df_li[, colnames(df_li) %in% c("date", as.character(dates_eval)[k])],
                          by = "date"),
                y = df_ls[, colnames(df_ls) %in% c("date", as.character(dates_eval)[k])],
                by = "date")
  colnames(df_cp)<- c("date", type_case, "li", "ls")
  df_cp<- df_cp[complete.cases(df_cp),]
  df_cp$cp_long<- as.numeric(data.table::between(x = df_cp[,type_case], lower = df_cp$li,
                                                 upper = df_cp$ls))
  
  cp_long <- ( sum(df_cp$cp_long) / nrow(df_cp))*100
  if(nrow(df_cp) >= k_steps_ahead_short){
    cp_short <- ( sum(df_cp$cp_long[1:k_steps_ahead_short]) / k_steps_ahead_short)*100
  } else{
    cp_short<- NA
  }
  
  df_ggplot<- df_ggplot %>% add_row(update_date = dates_eval[k], cp_long = cp_long, cp_short = cp_short)
  
}


### Plot (long term)
ggplot(data = df_ggplot) +
  geom_point(mapping = aes(x = update_date, y = cp_long), color = "#999999") + # plot observations as dots
  geom_line(aes(x = update_date, y = cp_long), color = "#999999") + # plot observations as dots + lines
  labs(x = "Update date", y = "Coverage percentage (long-term)") + # x- and y-labels
  ggtitle(country_name) + # plot title
  scale_x_date(breaks = seq(from = first(dates_eval), to = last(dates_eval), by = 7), date_labels = "%d/%b/%Y") + #
  # specifies x-scale
  ylim(0, 100) + # y limits
  theme_bw() + # white backgroud
  theme(axis.text.x = element_text(angle = 90, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot), # optional settings for title
        strip.text = element_text(size = size_plot))


### Plot (short term)
ggplot(data = df_ggplot) +
  geom_point(mapping = aes(x = update_date, y = cp_short), color = "#999999") + # plot observations as dots
  geom_line(aes(x = update_date, y = cp_short), color = "#999999") + # plot observations as dots + lines
  labs(x = "Update date", y = "Coverage percentage (short-term)") + # x- and y-labels
  ggtitle(country_name) + # plot title
  scale_x_date(breaks = seq(from = first(dates_eval), to = last(dates_eval), by = 7), date_labels = "%d/%b/%Y") + #
  # specifies x-scale
  ylim(0, 100) + # y limits
  theme_bw() + # white backgroud
  theme(axis.text.x = element_text(angle = 90, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot), # optional settings for title
        strip.text = element_text(size = size_plot))




