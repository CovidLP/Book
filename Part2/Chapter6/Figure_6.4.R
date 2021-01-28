###---------------------------------------------------------------###
###   Building a Platform for Data-Driven Pandemic Prediction:    ###
###   From Data Modelling to Visualization - the CovidLP Project  ###
###                                                               ###
###   Chapter 6: Modelling specific data features                 ###
###   Section 6.3: Seasonality                                    ###
###   Figure 6.4                                                  ###
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

###-----------------------------
### Setting seed
set.seed(123456789)
###-----------------------------

###-----------------------------
### Setting directories
data_directory<- "Part2/Chapter6/data/"
results_directory<- "Part2/Chapter6/results/"
###-----------------------------

###-----------------------------
### Graphical specifications (you may need to adapt these settings)
size_plot<- 15
vjust_plot<- 0.5
###-----------------------------

###-----------------------------###-----------------------------###-----------------------------###

###------------------------------###
###   FIGURE 6.3 (a)             ###
###------------------------------###

country_name<- "Brazil"

### Observed data
Y<- readRDS(file = paste0(data_directory, "data_Brazil.rds"))

### MCMC results
results<- readRDS(file = paste0(results_directory, "Brazil_d.rds"))
df<- data.frame(date = results$mu_plot$date[1:nrow(Y)], median = results$mu_plot$mu[1:nrow(Y)]) # mean curve data

### R system in English
Sys.setlocale("LC_TIME", "C")

### Plot
ggplot(data = Y) +
  geom_point(mapping = aes(x = date, y = d_new), color = "#999999") + # plot observations as dots
  geom_line(aes(x = date, y = d_new), color = "#999999") + # plot observations as dots + lines
  labs(x = "Reporting date", y = "Daily of confirmed deaths") + # x- and y-labels
  scale_x_date(breaks = seq(from = first(Y$date), to = last(Y$date), by = 21), date_labels = "%d/%b/%Y") + # specifies x-scale
  theme_bw() + # white backgroud
  theme(axis.text.x = element_text(angle = 90, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        strip.text = element_text(size = size_plot)) +
  geom_line(data = df, mapping = aes(x = date, y = median), color = "black") # adds fitted mean curve


###------------------------------###
###   FIGURE 6.3 (b)             ###
###------------------------------###

country_name<- "Costa Rica"

### Observed data
Y<- readRDS(file = paste0(data_directory, "data_CostaRica.rds"))

### MCMC results
results<- readRDS(file = paste0(results_directory, "Costa-Rica_n.rds"))
df<- data.frame(date = results$mu_plot$date[1:nrow(Y)], median = results$mu_plot$mu[1:nrow(Y)]) # mean curve data

### R system in English
Sys.setlocale("LC_TIME", "C")

### Plot
ggplot(data = Y) +
  geom_point(mapping = aes(x = date, y = n_new), color = "#999999") + # plot observations as dots
  geom_line(aes(x = date, y = n_new), color = "#999999") + # plot observations as dots + lines
  labs(x = "Reporting date", y = "Daily confirmed cases") + # x- and y-labels
  scale_x_date(breaks = seq(from = first(Y$date), to = last(Y$date), by = 21), date_labels = "%d/%b/%Y") + # specifies x-scale
  theme_bw() + # white backgroud
  theme(axis.text.x = element_text(angle = 90, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        strip.text = element_text(size = size_plot)) +
  geom_line(data = df, mapping = aes(x = date, y = median), color = "black") # adds fitted mean curve





