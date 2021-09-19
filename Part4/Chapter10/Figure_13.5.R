###---------------------------------------------------------------###
###   Building a Platform for Data-Driven Pandemic Prediction:    ###
###   From Data Modelling to Visualization - the CovidLP Project  ###
###                                                               ###
###   Chapter 13: Investigating inference results                 ###
###   Section 13.4: Practical situation                           ###
###   Subsection 13.4.2: Seasonality                              ###
###   Figure 13.5                                                 ###
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
data_directory<- "Part5/Chapter13/data/"
results_directory<- "Part5/Chapter13/results/"
###-----------------------------

###-----------------------------
### Graphical specifications (you may need to adapt these settings)
size_plot<- 15
vjust_plot<- 1.8
###-----------------------------

###-----------------------------###-----------------------------###-----------------------------###

###------------------------------###
###   FIGURE 13.5 (a)            ###
###------------------------------###

state_name<- "MG" 

### Observed data
Y<- readRDS(file = paste0(data_directory, "data_Brazil_", state_name, ".rds"))

### MCMC results
results_regular<- readRDS(file = paste0(results_directory, "fit_regular_Brazil_", state_name, "_ne.rds"))
results_weekend<- readRDS(file = paste0(results_directory, "fit_weekend_Brazil_", state_name, "_ne.rds"))

df_regular<- data.frame(date = results_regular$mu_plot$date[1:nrow(Y)],
                        median = results_regular$mu_plot$mu[1:nrow(Y)]) # mean curve data
df_weekend<- data.frame(date = results_weekend$mu_plot$date[1:nrow(Y)],
                        median = results_weekend$mu_plot$mu[1:nrow(Y)]) # mean curve data


### Data frame to plot
df_ggplot<- merge(x = Y, y = df_regular, by.x = "date", by.y = "date", all.x = TRUE, all.y = TRUE)
colnames(df_ggplot)<- c("date", "n", "d", "n_new", "d_new", "state", "median_regular")

df_ggplot<- merge(x = df_ggplot, y = df_weekend, by.x = "date", by.y = "date", all.x = TRUE, all.y = TRUE)
colnames(df_ggplot)<- c("date", "n", "d", "n_new", "d_new", "state", "median_regular", "median_weekend")

### R system in English
Sys.setlocale("LC_TIME", "C")

### Plot
colors_plot <- c("#999999", rep(x = "black", times = 2))
linetype_plot <- c(1, 2, 1)
shape_plot <- c(1, NA, NA)
names(colors_plot) <- names(linetype_plot) <- names(shape_plot) <- c("observations", "regular", "weekend effect")

ggplot(data = df_ggplot) +
  geom_point(aes(x = date, y = n_new, colour = "observations", shape = "observations")) + # plot observations as dots
  geom_line(aes(x = date, y = n_new, colour = "observations", linetype = "observations")) + # plot observations as
  # dots + lines
  geom_line(aes(x = date, y = median_regular, group = "regular", colour = "regular", linetype = "regular",
                shape = "regular")) +
  geom_line(aes(x = date, y = median_weekend, group = "weekend effect", colour = "weekend effect",
                linetype = "weekend effect", shape = "weekend effect")) + 
  labs(x = "Date", y = "Daily confirmed cases") + # x- and y-labels
  ggtitle(state_name) + # plot title
  scale_x_date(breaks = seq(from = first(df_ggplot$date), to = last(df_ggplot$date), by = 21),
               date_labels = "%d/%b/%Y") + # specifies x-scale
  scale_colour_manual(name = "", values = colors_plot) +
  scale_fill_manual(name = "", values = colors_plot) +
  scale_linetype_manual(name = "", values = linetype_plot) +
  scale_shape_manual(name = "", values = shape_plot) +
  theme_bw() + # white backgroud
  theme(axis.text.x = element_text(angle = 90, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        legend.text = element_text(size = size_plot),
        legend.key.size = unit(1.5, "cm"),
        legend.title = element_text(size = size_plot),
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot), # optional settings for title
        strip.text = element_text(size = size_plot))

###------------------------------###
###   FIGURE 13.5 (b)            ###
###------------------------------###

state_name<- "SP" 

### Observed data
Y<- readRDS(file = paste0(data_directory, "data_Brazil_", state_name, ".rds"))

### MCMC results
results_regular<- readRDS(file = paste0(results_directory, "fit_regular_Brazil_", state_name, "_ne.rds"))
results_weekend<- readRDS(file = paste0(results_directory, "fit_weekend_Brazil_", state_name, "_ne.rds"))

df_regular<- data.frame(date = results_regular$mu_plot$date[1:nrow(Y)],
                        median = results_regular$mu_plot$mu[1:nrow(Y)]) # mean curve data
df_weekend<- data.frame(date = results_weekend$mu_plot$date[1:nrow(Y)],
                        median = results_weekend$mu_plot$mu[1:nrow(Y)]) # mean curve data


### Data frame to plot
df_ggplot<- merge(x = Y, y = df_regular, by.x = "date", by.y = "date", all.x = TRUE, all.y = TRUE)
colnames(df_ggplot)<- c("date", "n", "d", "n_new", "d_new", "state", "median_regular")

df_ggplot<- merge(x = df_ggplot, y = df_weekend, by.x = "date", by.y = "date", all.x = TRUE, all.y = TRUE)
colnames(df_ggplot)<- c("date", "n", "d", "n_new", "d_new", "state", "median_regular", "median_weekend")

### R system in English
Sys.setlocale("LC_TIME", "C")

### Plot
colors_plot <- c("#999999", rep(x = "black", times = 2))
linetype_plot <- c(1, 2, 1)
shape_plot <- c(1, NA, NA)
names(colors_plot) <- names(linetype_plot) <- names(shape_plot) <- c("observations", "regular", "weekend effect")

ggplot(data = df_ggplot) +
  geom_point(aes(x = date, y = n_new, colour = "observations", shape = "observations")) + # plot observations as dots
  geom_line(aes(x = date, y = n_new, colour = "observations", linetype = "observations")) + # plot observations as
  # dots + lines
  geom_line(aes(x = date, y = median_regular, group = "regular", colour = "regular", linetype = "regular",
                shape = "regular")) +
  geom_line(aes(x = date, y = median_weekend, group = "weekend effect", colour = "weekend effect",
                linetype = "weekend effect", shape = "weekend effect")) + 
  labs(x = "Date", y = "Daily confirmed cases") + # x- and y-labels
  ggtitle(state_name) + # plot title
  scale_x_date(breaks = seq(from = first(df_ggplot$date), to = last(df_ggplot$date), by = 21),
               date_labels = "%d/%b/%Y") + # specifies x-scale
  scale_colour_manual(name = "", values = colors_plot) +
  scale_fill_manual(name = "", values = colors_plot) +
  scale_linetype_manual(name = "", values = linetype_plot) +
  scale_shape_manual(name = "", values = shape_plot) +
  theme_bw() + # white backgroud
  theme(axis.text.x = element_text(angle = 90, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        legend.text = element_text(size = size_plot),
        legend.key.size = unit(1.5, "cm"),
        legend.title = element_text(size = size_plot),
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot), # optional settings for title
        strip.text = element_text(size = size_plot))


