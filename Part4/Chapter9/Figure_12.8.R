###---------------------------------------------------------------###
###   Building a Platform for Data-Driven Pandemic Prediction:    ###
###   From Data Modelling to Visualization - the CovidLP Project  ###
###                                                               ###
###   Chapter 9: Daily evaluation of the updated data             ###
###   Section 9.4: Seasonality                                    ###
###   Figure 9.8                                                  ###
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
###   FIGURE 9.8 (a) and FIGURE 9.8 (b)   ###
###---------------------------------------###

country_name<- "Brazil"

### Observed data
Y<- readRDS(file = paste0(data_directory, "data_Brazil.rds"))

### R system in English
Sys.setlocale("LC_TIME", "C")

### Plot
ggplot(data = Y) +
  geom_point(mapping = aes(x = date, y = n_new), color = "#999999") + # plot observations as dots
  geom_line(aes(x = date, y = n_new), color = "#999999") + # plot observations as dots + lines
  labs(x = "Notification Date", y = "Daily confirmed cases") + # x- and y-labels
  ggtitle(country_name) + # plot title
  scale_x_date(breaks = seq(from = first(Y$date), to = last(Y$date), by = 21), date_labels = "%d/%b/%Y") + # specifies x-scale
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
  geom_point(mapping = aes(x = date, y = d_new), color = "#999999") + # plot observations as dots
  geom_line(aes(x = date, y = d_new), color = "#999999") + # plot observations as dots + lines
  labs(x = "Notification Date", y = "Daily confirmed deaths") + # x- and y-labels
  ggtitle(country_name) + # plot title
  scale_x_date(breaks = seq(from = first(Y$date), to = last(Y$date), by = 21), date_labels = "%d/%b/%Y") + # specifies x-scale
  theme_bw() + # white backgroud
  theme(axis.text.x = element_text(angle = 90, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot), # optional settings for title
        strip.text = element_text(size = size_plot))

###---------------------------------------###
###   FIGURE 9.8 (c) and FIGURE 9.8 (d)   ###
###---------------------------------------###

country_name<- "Costa Rica"

### Observed data
Y<- readRDS(file = paste0(data_directory, "data_CostaRica.rds"))

### R system in English
Sys.setlocale("LC_TIME", "C")

### Plot
ggplot(data = Y) +
  geom_point(mapping = aes(x = date, y = n_new), color = "#999999") + # plot observations as dots
  geom_line(aes(x = date, y = n_new), color = "#999999") + # plot observations as dots + lines
  labs(x = "Notification Date", y = "Daily confirmed cases") + # x- and y-labels
  ggtitle(country_name) + # plot title
  scale_x_date(breaks = seq(from = first(Y$date), to = last(Y$date), by = 21), date_labels = "%d/%b/%Y") + # specifies x-scale
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
  geom_point(mapping = aes(x = date, y = d_new), color = "#999999") + # plot observations as dots
  geom_line(aes(x = date, y = d_new), color = "#999999") + # plot observations as dots + lines
  labs(x = "Notification Date", y = "Daily confirmed deaths") + # x- and y-labels
  ggtitle(country_name) + # plot title
  scale_x_date(breaks = seq(from = first(Y$date), to = last(Y$date), by = 21), date_labels = "%d/%b/%Y") + # specifies x-scale
  theme_bw() + # white backgroud
  theme(axis.text.x = element_text(angle = 90, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot), # optional settings for title
        strip.text = element_text(size = size_plot))

###---------------------------------------###
###   FIGURE 9.8 (e) and FIGURE 9.8 (f)   ###
###---------------------------------------###

country_name<- "Paraguay"

### Observed data
Y<- readRDS(file = paste0(data_directory, "data_Paraguay.rds"))

### R system in English
Sys.setlocale("LC_TIME", "C")

### Plot
ggplot(data = Y) +
  geom_point(mapping = aes(x = date, y = n_new), color = "#999999") + # plot observations as dots
  geom_line(aes(x = date, y = n_new), color = "#999999") + # plot observations as dots + lines
  labs(x = "Notification Date", y = "Daily confirmed cases") + # x- and y-labels
  ggtitle(country_name) + # plot title
  scale_x_date(breaks = seq(from = first(Y$date), to = last(Y$date), by = 21), date_labels = "%d/%b/%Y") + # specifies x-scale
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
  geom_point(mapping = aes(x = date, y = d_new), color = "#999999") + # plot observations as dots
  geom_line(aes(x = date, y = d_new), color = "#999999") + # plot observations as dots + lines
  labs(x = "Notification Date", y = "Daily confirmed deaths") + # x- and y-labels
  ggtitle(country_name) + # plot title
  scale_x_date(breaks = seq(from = first(Y$date), to = last(Y$date), by = 21), date_labels = "%d/%b/%Y") + # specifies x-scale
  theme_bw() + # white backgroud
  theme(axis.text.x = element_text(angle = 90, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot), # optional settings for title
        strip.text = element_text(size = size_plot))

