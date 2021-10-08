###---------------------------------------------------------------###
###   Building a Platform for Data-Driven Pandemic Prediction:    ###
###   From Data Modelling to Visualization - the CovidLP Project  ###
###                                                               ###
###   Chapter 13: Investigating inference results                 ###
###   Section                                                     ###
###   Subsection                                                  ###
###   Figure                                                      ###
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
library(tibble)

###-----------------------------
### Setting seed
set.seed(123456789)
###-----------------------------

###-----------------------------
### Setting directories
data_directory <- "Part5/Chapter13/data/"
results_directory <- "Part5/Chapter13/results/"
###-----------------------------

###-----------------------------
### Graphical specifications (you may need to adapt these settings)
size_plot <- 30
vjust_plot <- 1.8
theme_plot <-   theme_bw() + # white backgroud
  theme(axis.text.x = element_text(size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        legend.key.size = unit(0.5, "cm"), # optional settings for symbol size in legend
        legend.text = element_text(size = size_plot), # optional setting for legend size
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot), # optional settings for title
        strip.text = element_text(size = size_plot))
###-----------------------------

###-----------------------------###-----------------------------###-----------------------------###

###------------------------------###
###   FIGURE                     ###
###------------------------------###

state_name <- "MG"

### Observed data (download available in https://github.com/CovidLP/Book/tree/main/Part4/Chapter10/data)
Y <- readRDS(file = paste0(data_directory, "data_Brazil_", state_name, ".rds"))
Y_weekly <- readRDS(file = paste0(data_directory, "data_Brazil_weekly_", state_name, ".rds"))

### MCMC results (download available in https://github.com/CovidLP/Book/tree/main/Part4/Chapter10/results)
results_regular <- readRDS(file = paste0(results_directory, "fit_regular_Brazil_", state_name, "_ne.rds"))$mu_plot[1:nrow(Y),]
results_weekend <- readRDS(file = paste0(results_directory, "fit_weekend_Brazil_", state_name, "_ne.rds"))$mu_plot[1:nrow(Y),]
results_weekly <- readRDS(file = paste0(results_directory, "fit_weekly_Brazil_", state_name, "_ne.rds"))

### Data frame to plot
df_ggplot <- data.frame(re = c(((Y$n_new - results_regular$mu) / (Y$n_new + 1))*100,
                               ((Y$n_new - results_weekend$mu) / (Y$n_new + 1))*100,
                               ((Y_weekly$summed - results_weekly$median) / (Y_weekly$summed + 1))*100),
                        group = c(rep(x = "regular", times = length(Y$n_new)),
                                   rep(x = "weekend effect", times = length(Y$n_new)),
                                   rep(x = "weekly scale", times = length(Y_weekly$summed)))
)

### Plots


ggplot(data = Y_weekly) +
  geom_point(mapping = aes(x = epi_week, y = summed), colour = "#999999") +
  geom_line(mapping = aes(x = epi_week, y = summed), colour = "#999999") +
  geom_line(mapping = aes(x = epi_week, y = median), data = results_weekly, colour = "black", size = 1) +
  ggtitle(state_name) + # plot title
  labs(x = "Epidemic week", y = "Weekly confirmed counts") + # x- and y-labels
  theme_plot

ggplot(data = df_ggplot) +
  geom_boxplot(mapping = aes(x = group, y = re), fill = "#999999", outlier.shape = NA) + # plot boxplots of RE per weekday
  geom_hline(yintercept = 0, linetype = 1, color = "black", size = 1) + # reference line
  labs(x = "", y = "Relative error") + # x- and y-labels
  ggtitle(state_name) + # plot title
  ylim(-120, 93) + 
  theme_plot


###------------------------------###
###   FIGURE                     ###
###------------------------------###

state_name <- "SP"

### Observed data (download available in https://github.com/CovidLP/Book/tree/main/Part4/Chapter10/data)
Y <- readRDS(file = paste0(data_directory, "data_Brazil_", state_name, ".rds"))
Y_weekly <- readRDS(file = paste0(data_directory, "data_Brazil_weekly_", state_name, ".rds"))

### MCMC results (download available in https://github.com/CovidLP/Book/tree/main/Part4/Chapter10/results)
results_regular <- readRDS(file = paste0(results_directory, "fit_regular_Brazil_", state_name, "_ne.rds"))$mu_plot[1:nrow(Y),]
results_weekend <- readRDS(file = paste0(results_directory, "fit_weekend_Brazil_", state_name, "_ne.rds"))$mu_plot[1:nrow(Y),]
results_weekly <- readRDS(file = paste0(results_directory, "fit_weekly_Brazil_", state_name, "_ne.rds"))

### Data frame to plot
df_ggplot <- data.frame(re = c(((Y$n_new - results_regular$mu) / (Y$n_new + 1))*100,
                               ((Y$n_new - results_weekend$mu) / (Y$n_new + 1))*100,
                               ((Y_weekly$summed - results_weekly$median) / (Y_weekly$summed + 1))*100),
                        group = c(rep(x = "regular", times = length(Y$n_new)),
                                  rep(x = "weekend effect", times = length(Y$n_new)),
                                  rep(x = "weekly scale", times = length(Y_weekly$summed)))
)

### Plot

ggplot(data = Y_weekly) +
  geom_point(mapping = aes(x = epi_week, y = summed), colour = "#999999") +
  geom_line(mapping = aes(x = epi_week, y = summed), colour = "#999999") +
  geom_line(mapping = aes(x = epi_week, y = median), data = results_weekly, colour = "black", size = 1) +
  ggtitle(state_name) + # plot title
  labs(x = "Epidemic week", y = "Weekly confirmed counts") + # x- and y-labels
  theme_plot


ggplot(data = df_ggplot) +
  geom_boxplot(mapping = aes(x = group, y = re), fill = "#999999", outlier.shape = NA) + # plot boxplots of RE per weekday
  geom_hline(yintercept = 0, linetype = 1, color = "black", size = 1) + # reference line
  labs(x = "", y = "Relative error") + # x- and y-labels
  ggtitle(state_name) + # plot title
  ylim(-60, 68) + 
  theme_plot
