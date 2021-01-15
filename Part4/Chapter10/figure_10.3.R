###---------------------------------------------------------------###
###   Building a Platform for Data-Driven Pandemic Prediction:    ###
###   From Data Modelling to Visualization - the CovidLP Project  ###
###                                                               ###
###   Chapter 10: Investigating inference results                 ###
###   Section 10.4  Practical situation                           ###
###   Subsection 10.4.1 Overall comparison                        ###
###   Figure 10.3                                                 ###
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
### Relative error (RE)
###-----------------------------

### Long-term comparison: number of steps head to display RE
k_steps_ahead_long<- length(dates_eval)
relative_error_long<- matrix(data = NA, nrow = nrow(df), ncol = length(dates_eval))

### Short-term comparison: number of steps head to display RE
k_steps_ahead_short<- 7
relative_error_short<- matrix(data = NA, nrow = k_steps_ahead_short, ncol = length(dates_eval))

### Case type: n_new or d_new
type_case<- "n_new"


### Beggining data frame to use in ggplot
k<- 1

### Builds data frame with observations and prediction in date dates_eval[k]
df_relative_error<- merge(x = Y[,c("date", type_case)],
                          y = df[, colnames(df) %in% c("date", as.character(dates_eval)[k])],
                          by = "date")
colnames(df_relative_error)<- c("date", type_case, "median")
df_relative_error<- df_relative_error[complete.cases(df_relative_error),]

### Calculates RE for long-term comparison
df_long_ggplot<- df_relative_error
df_long_ggplot$updation_date<- rep(x = dates_eval[k], times = nrow(df_relative_error))
df_long_ggplot$re_long<- ((df_relative_error$median - df_relative_error[,type_case]) /
                            (abs(df_relative_error[,type_case]) + 1))*100

### Calculates RE for short-term comparison
df_short_ggplot<- df_relative_error[1:k_steps_ahead_short,]
df_short_ggplot$updation_date<- rep(x = dates_eval[k], times = k_steps_ahead_short)
df_short_ggplot$re_short<- (((df_relative_error$median - df_relative_error[,type_case]) /
                               (abs(df_relative_error[,type_case]) + 1))*100)[1:k_steps_ahead_short]

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


### Plot (long term)
ggplot(data = df_long_ggplot) +
  geom_boxplot(mapping = aes(x = updation_date, y = re_long, group = updation_date), fill = "#999999") + # plot
  # boxplots of RE per day of update
  labs(x = "Date of update", y = "Relative error (long-term)") + # x- and y-labels
  ggtitle(country_name) + # plot title
  scale_x_date(breaks = seq(from = first(dates_eval), to = last(dates_eval), by = 7), date_labels = "%d/%b/%Y") + #
  # specifies x-scale
  geom_hline(yintercept = 0, linetype = linetype_plot, color = "black", size = size_line_plot) + # reference line
  theme_bw() + # white backgroud
  theme(axis.text.x = element_text(angle = 90, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot), # optional settings for title
        strip.text = element_text(size = size_plot))

### Plot (short term)
ggplot(data = df_short_ggplot) +
  geom_boxplot(mapping = aes(x = updation_date, y = re_short, group = updation_date), fill = "#999999") + # plot
  # boxplots of RE per day of update
  labs(x = "Date of update", y = "Relative error (short-term)") + # x- and y-labels
  ggtitle(country_name) + # plot title
  scale_x_date(breaks = seq(from = first(dates_eval), to = last(dates_eval), by = 7), date_labels = "%d/%b/%Y") + #
  # specifies x-scale
  geom_hline(yintercept = 0, linetype = linetype_plot, color = "black", size = size_line_plot) + # reference line
  theme_bw() + # white backgroud
  theme(axis.text.x = element_text(angle = 90, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot), # optional settings for title
        strip.text = element_text(size = size_plot))











