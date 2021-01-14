###---------------------------------------------------------------###
###   Building a Platform for Data-Driven Pandemic Prediction:    ###
###   From Data Modelling to Visualization - the CovidLP Project  ###
###                                                               ###
###   Chapter 9: Daily evaluation of the updated data             ###
###   Section 9.4: Seasonality                                    ###
###   Figures 9.9 and 9.10                                        ###
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
data_directory<- "Part4/Chapter9/data/"
###-----------------------------

###-----------------------------
### Graphical specifications (you may need to adapt these settings)
size_plot<- 15
vjust_plot<- 1.8
###-----------------------------

###-----------------------------###-----------------------------###-----------------------------###

###---------------------------------------###
###   FIGURE 9.9 (a) and FIGURE 9.9 (b)   ###
###---------------------------------------###

country_name<- "Brazil"

### Observed data
Y<- readRDS(file = paste0(data_directory, "data_Brazil.rds"))

### Creating variables with weekday information
Sys.setlocale("LC_TIME", "C") # R system in English
Y$weekday<- factor(x = weekdays(Y$date),
                   levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

### Plot
ggplot(data = Y) +
  geom_boxplot(mapping = aes(x = weekday, y = n_new), fill = "#999999") + # plot boxplots of cases per weekday
  labs(x = "Day of the week", y = "Number of confirmed cases") + # x- and y-labels
  ggtitle(country_name) + # plot title
  theme_bw() + # white backgroud
  theme(axis.text.x = element_text(angle = 90, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot), # optional settings for title
        strip.text = element_text(size = size_plot))

### Plot
ggplot(data = Y) +
  geom_boxplot(mapping = aes(x = weekday, y = d_new), fill = "#999999") + # plot boxplots of cases per weekday
  labs(x = "Day of the week", y = "Number of confirmed deaths") + # x- and y-labels
  ggtitle(country_name) + # plot title
  theme_bw() + # white backgroud
  theme(axis.text.x = element_text(angle = 90, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot), # optional settings for title
        strip.text = element_text(size = size_plot))


###-----------------------------###-----------------------------###-----------------------------###

###-----------------------------------------###
###   FIGURE 9.10 (a) and FIGURE 9.10 (b)   ###
###-----------------------------------------###

country_name<- "Costa Rica"

### Observed data
Y<- readRDS(file = paste0(data_directory, "data_CostaRica.rds"))

### Creating variables with weekday information
Sys.setlocale("LC_TIME", "C") # R system in English
Y$weekday<- factor(x = weekdays(Y$date),
                   levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

### Plot
ggplot(data = Y) +
  geom_boxplot(mapping = aes(x = weekday, y = n_new), fill = "#999999") + # plot boxplots of cases per weekday
  labs(x = "Day of the week", y = "Number of confirmed cases") + # x- and y-labels
  ggtitle(country_name) + # plot title
  theme_bw() + # white backgroud
  theme(axis.text.x = element_text(angle = 90, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot), # optional settings for title
        strip.text = element_text(size = size_plot))

### Plot
ggplot(data = Y) +
  geom_boxplot(mapping = aes(x = weekday, y = d_new), fill = "#999999") + # plot boxplots of cases per weekday
  labs(x = "Day of the week", y = "Number of confirmed deaths") + # x- and y-labels
  ggtitle(country_name) + # plot title
  theme_bw() + # white backgroud
  theme(axis.text.x = element_text(angle = 90, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot), # optional settings for title
        strip.text = element_text(size = size_plot))

###-----------------------------###-----------------------------###-----------------------------###

###-----------------------------------------###
###   FIGURE 9.10 (c) and FIGURE 9.10 (d)   ###
###-----------------------------------------###

country_name<- "Paraguay"

### Observed data
Y<- readRDS(file = paste0(data_directory, "data_Paraguay.rds"))

### Creating variables with weekday information
Sys.setlocale("LC_TIME", "C") # R system in English
Y$weekday<- factor(x = weekdays(Y$date),
                   levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

### Plot
ggplot(data = Y) +
  geom_boxplot(mapping = aes(x = weekday, y = n_new), fill = "#999999") + # plot boxplots of cases per weekday
  labs(x = "Day of the week", y = "Number of confirmed cases") + # x- and y-labels
  ggtitle(country_name) + # plot title
  theme_bw() + # white backgroud
  theme(axis.text.x = element_text(angle = 90, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot), # optional settings for title
        strip.text = element_text(size = size_plot))

### Plot
ggplot(data = Y) +
  geom_boxplot(mapping = aes(x = weekday, y = d_new), fill = "#999999") + # plot boxplots of cases per weekday
  labs(x = "Day of the week", y = "Number of confirmed deaths") + # x- and y-labels
  ggtitle(country_name) + # plot title
  theme_bw() + # white backgroud
  theme(axis.text.x = element_text(angle = 90, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot), # optional settings for title
        strip.text = element_text(size = size_plot))
