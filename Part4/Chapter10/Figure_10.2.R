###---------------------------------------------------------------###
###   Building a Platform for Data-Driven Pandemic Prediction:    ###
###   From Data Modelling to Visualization - the CovidLP Project  ###
###                                                               ###
###   Chapter 10: Investigating inference results                 ###
###   Section 10.4  Practical situation                           ###
###   Subsection 10.4.1 Overall comparison                         ###
###   Figure 10.2                                                 ###
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
library(plyr)

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
size_line_plot<- 1.2
size_plot<- 15
vjust_plot<- 1.8
###-----------------------------

###-----------------------------###-----------------------------###-----------------------------###

###-----------------###
###   FIGURE 10.2   ###
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
df_aux<- df_aux[df_aux$date < as.Date(last(Y$date) + 30),] # filtering results

### Dates to evaluate (to avoid incorrect recordings)
dates_eval<- as.Date(intersect(x = colnames(df_aux),
                               y = as.character(seq(from = as.Date(colnames(df_aux)[2]),
                                                    to = as.Date("2020-10-31"), by = "days"))))
df_aux<- df_aux[, colnames(df_aux) %in% c("date", as.character(dates_eval))]

### Number of estimated curves that will be ploted
n_days<- 6

### Position of dates in which the estimated curves were updated
aux_seq_dates<- round(x = seq(from = 1, to = length(dates_eval), length.out = n_days), digits = 0)

### Data frame with results of daily updated prediction obtained by the CovidLP project
df_ggplot <- merge(x = Y, y = df_aux[,c(1, 2)], by.x = "date", by.y = "date", all.x = TRUE, all.y = TRUE)
colnames(df_ggplot)<- c("date", "n", "d", "n_new", "d_new", "median")
df_ggplot$update_date <- rep(x = colnames(df_aux)[2], times = nrow(df_ggplot))

for(k in 2:length(dates_eval)){
  
  df_ggplot_aux <- merge(x = Y, y = df_aux[,c(1, k+1)], by.x = "date", by.y = "date", all.x = TRUE, all.y = TRUE)
  colnames(df_ggplot_aux)<- c("date", "n", "d", "n_new", "d_new", "median")
  df_ggplot_aux$update_date <- rep(x = colnames(df_aux)[k+1], times = nrow(df_ggplot_aux))
  
  
  df_ggplot<- df_ggplot %>% add_row(df_ggplot_aux)
}

###------------------------------###------------------------------###------------------------------###

### R System in English
Sys.setlocale("LC_TIME", "C")

### Plot
n_days<- 6
chosen_dates<- as.character(dates_eval[seq(from = 1, to = length(dates_eval), length.out = n_days)])

colors_plot<- c("#999999", rep(x = "black", times = n_days))
linetype_plot<- c(1, 1:n_days)
shape_plot<- c(1, rep(x = NA, times = n_days))
names(colors_plot) <- names(linetype_plot) <- names(shape_plot) <- c("observations", chosen_dates)

ggplot(data = df_ggplot) +
  geom_point(aes(x = date, y = n_new, colour = "observations", shape = "observations")) + # plot observations as dots
  geom_line(aes(x = date, y = n_new, colour = "observations", linetype = "observations")) + # plot observations as
  # dots + lines
  geom_line(aes(x = date, y = median, group = update_date, colour = update_date, linetype = update_date,
                shape = update_date),
            data = function(x)(x[x$update_date %in% chosen_dates,])) + 
  labs(x = "Date", y = "Number of confirmed cases") + # x- and y-labels
  ggtitle(country_name) + # plot title
  scale_x_date(breaks = seq(from = first(df_ggplot$date), to = last(df_ggplot$date), by = 21),
               date_labels = "%d/%b/%Y") + # specifies x-scale
  scale_colour_manual(name = "Updated in", values = colors_plot,
                      labels = c(format(x = as.Date(chosen_dates), "%d/%b/%Y"), "observations")) +
  scale_fill_manual(name = "",values = colors_plot,
                    labels = c(format(x = as.Date(chosen_dates), "%d/%b/%Y"), "observations")) +
  scale_linetype_manual(name = "Updated in", values = linetype_plot,
                        labels = c(format(x = as.Date(chosen_dates), "%d/%b/%Y"), "observations")) +
  scale_shape_manual(name = "Updated in", values = shape_plot,
                     labels = c(format(x = as.Date(chosen_dates), "%d/%b/%Y"), "observations")) +
  theme_bw() + # white backgroud
  theme(axis.text.x = element_text(angle = 90, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = size_plot),
        legend.title = element_text(size = size_plot),
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot), # optional settings for title
        strip.text = element_text(size = size_plot))
