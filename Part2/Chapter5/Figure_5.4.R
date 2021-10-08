###---------------------------------------------------------------###
###   Building a Platform for Data-Driven Pandemic Prediction:    ###
###   From Data Modelling to Visualization - the CovidLP Project  ###
###                                                               ###
###   Chapter 5: Modelling specific data features                 ###
###   Section 5.3: Seasonality                                    ###
###   Figure 5.4                                                  ###
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
data_directory<- "Part2/Chapter5/data/"
results_directory<- "Part2/Chapter5/results/"
###-----------------------------

###-----------------------------
### Graphical specifications (you may need to adapt these settings)
size_plot <- 15
vjust_plot <- 0.5
theme_plot <- theme_bw() + # white backgroud
  theme(axis.text.x = element_text(angle = 90, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        strip.text = element_text(size = size_plot))
###-----------------------------

###-----------------------------###-----------------------------###-----------------------------###

###------------------------------###
###   FIGURE 5.3 (a)             ###
###------------------------------###

country_name<- "Brazil"

### Observed data (available in https://github.com/CovidLP/Book/tree/main/Part2/Chapter5/data)
Y<- readRDS(file = paste0(data_directory, "data_Brazil.rds"))

### MCMC results (available in https://github.com/CovidLP/Book/tree/main/Part2/Chapter5/results)
results<- readRDS(file = paste0(results_directory, "Brazil_d.rds"))
df<- data.frame(date = results$mu_plot$date[1:nrow(Y)],
                median = results$mu_plot$mu[1:nrow(Y)]) # mean curve data
df_proj <- data.frame(date = results$lt_predict$date, 
                      median = results$lt_predict$med,
                      ci_inf = results$lt_predict$q25,
                      ci_sup = results$lt_predict$q975) # projections

### R system in English
Sys.setlocale("LC_TIME", "C")

### Plot
ggplot(data = Y) +
  geom_point(mapping = aes(x = date, y = d_new), color = "#999999") + # plot observations as dots
  geom_line(mapping = aes(x = date, y = d_new), color = "#999999", size = 0.8) + # plot observations as dots + lines
  geom_line(mapping = aes(x = date, y = median), data = df, color = "black", size = 0.8) +  # adds fitted mean curve
  geom_line(mapping = aes(x = date, y = median), data = df_proj[1:60, ], color = "black", size = 0.8) +  # adds fitted mean curve
  geom_ribbon(mapping = aes(x = date, ymin = ci_inf, ymax = ci_sup),
              data = df_proj[1:60, ], alpha = 0.7, colour = NA, fill = "black") + # adds projections
  geom_vline(xintercept = last(Y$date), colour = "#999999", linetype = 2, size = 0.8) + 
  labs(x = "Reporting date", y = "Daily confirmed deaths") + # x- and y-labels
  scale_x_date(breaks = seq(from = first(Y$date), to = last(Y$date) + 60, by = 21), date_labels = "%d/%b/%Y") + # specifies x-scale
  theme_plot

###------------------------------###
###   FIGURE 5.3 (b)             ###
###------------------------------###

country_name<- "Costa Rica"

### Observed data (available in https://github.com/CovidLP/Book/tree/main/Part2/Chapter5/data)
Y<- readRDS(file = paste0(data_directory, "data_CostaRica.rds"))

### MCMC results (available in https://github.com/CovidLP/Book/tree/main/Part2/Chapter5/results)
results<- readRDS(file = paste0(results_directory, "Costa-Rica_n.rds"))
df<- data.frame(date = results$mu_plot$date[1:nrow(Y)],
                median = results$mu_plot$mu[1:nrow(Y)]) # mean curve data
df_proj <- data.frame(date = results$lt_predict$date, 
                      median = results$lt_predict$med,
                      ci_inf = results$lt_predict$q25,
                      ci_sup = results$lt_predict$q975) # projections

### R system in English
Sys.setlocale("LC_TIME", "C")

### Plot
ggplot(data = Y) +
  geom_point(mapping = aes(x = date, y = n_new), color = "#999999") + # plot observations as dots
  geom_line(mapping = aes(x = date, y = n_new), color = "#999999", size = 0.8) + # plot observations as dots + lines
  geom_line(mapping = aes(x = date, y = median), data = df, color = "black", size = 0.8) +  # adds fitted mean curve
  geom_line(mapping = aes(x = date, y = median), data = df_proj[1:60, ], color = "black", size = 0.8) +  # adds fitted mean curve
  geom_ribbon(mapping = aes(x = date, ymin = ci_inf, ymax = ci_sup),
              data = df_proj[1:60, ], alpha = 0.7, colour = NA, fill = "black") + # adds projections
  geom_vline(xintercept = last(Y$date), colour = "#999999", linetype = 2, size = 0.8) + 
  labs(x = "Reporting date", y = "Daily confirmed cases") + # x- and y-labels
  scale_x_date(breaks = seq(from = first(Y$date), to = last(Y$date) + 60, by = 21), date_labels = "%d/%b/%Y") + # specifies x-scale
  theme_plot






