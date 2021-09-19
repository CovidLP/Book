###---------------------------------------------------------------###
###   Building a Platform for Data-Driven Pandemic Prediction:    ###
###   From Data Modelling to Visualization - the CovidLP Project  ###
###                                                               ###
###   Chapter 13: Investigating inference results                 ###
###   Section 13.4: Practical situation                           ###
###   Subsection 13.4.2: Seasonality                              ###
###   Figure 13.6                                                 ###
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
###   FIGURE 13.6 (a)            ###
###------------------------------###

state_name<- "MG" 

### Observed data
Y<- readRDS(file = paste0(data_directory, "data_Brazil_", state_name, ".rds"))

### R system in English
Sys.setlocale("LC_TIME", "C")
Y$weekday<- factor(x = weekdays(x = Y$date),
                   levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

### MCMC results
results_regular<- readRDS(file = paste0(results_directory, "fit_regular_Brazil_", state_name, "_ne.rds"))
results_weekend<- readRDS(file = paste0(results_directory, "fit_weekend_Brazil_", state_name, "_ne.rds"))

df_regular<- data.frame(date = results_regular$mu_plot$date[1:nrow(Y)],
                        median = results_regular$mu_plot$mu[1:nrow(Y)]) # mean curve data
df_weekend<- data.frame(date = results_weekend$mu_plot$date[1:nrow(Y)],
                        median = results_weekend$mu_plot$mu[1:nrow(Y)]) # mean curve data


### Data frame to plot
df_ggplot<- merge(x = Y, y = df_regular, by.x = "date", by.y = "date", all.x = TRUE, all.y = TRUE)
df_ggplot<- df_ggplot %>% add_row(merge(x = Y, y = df_weekend, by.x = "date", by.y = "date", all.x = TRUE,
                                        all.y = TRUE))
df_ggplot$type_model<- c(rep(x = "regular", times = nrow(df_regular)),
                        rep(x = "weekend effect", times = nrow(df_regular)))

### Calculating relative error
df_ggplot$re<-  ((df_ggplot$median - df_ggplot$n_new) / (abs(df_ggplot$n_new + 1)))*100

### Plot
colors_plot <- grey(level = c(0.8, 0.4))
names(colors_plot) <- c("regular", "weekend effect")

### Plot
ggplot(data = df_ggplot) +
  geom_boxplot(mapping = aes(x = weekday, y = re, fill = type_model), outlier.size = -1) + # plot boxplots of RE per weekday
  geom_hline(yintercept = 0, linetype = 1, color = "black", size = 1) + # reference line
  labs(x = "Day of the week", y = "Relative error") + # x- and y-labels
  ggtitle(state_name) + # plot title
  ylim(-68, 248) + 
  scale_colour_manual(name = "", values = colors_plot) +
  scale_fill_manual(name = "", values = colors_plot) +
  theme_bw() + # white backgroud
  theme(axis.text.x = element_text(angle = 90, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        legend.key.size = unit(0.5, "cm"), # optional settings for symbol size in legend
        legend.text = element_text(size = size_plot), # optional setting for legend size
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot), # optional settings for title
        strip.text = element_text(size = size_plot))

###-----------------------------###-----------------------------###-----------------------------###


###------------------------------###
###   FIGURE 13.6 (b)            ###
###------------------------------###

state_name<- "SP" 

### Observed data
Y<- readRDS(file = paste0(data_directory, "data_Brazil_", state_name, ".rds"))

### R system in English
Sys.setlocale("LC_TIME", "C")
Y$weekday<- factor(x = weekdays(x = Y$date),
                   levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

### MCMC results
results_regular<- readRDS(file = paste0(results_directory, "fit_regular_Brazil_", state_name, "_ne.rds"))
results_weekend<- readRDS(file = paste0(results_directory, "fit_weekend_Brazil_", state_name, "_ne.rds"))

df_regular<- data.frame(date = results_regular$mu_plot$date[1:nrow(Y)],
                        median = results_regular$mu_plot$mu[1:nrow(Y)]) # mean curve data
df_weekend<- data.frame(date = results_weekend$mu_plot$date[1:nrow(Y)],
                        median = results_weekend$mu_plot$mu[1:nrow(Y)]) # mean curve data


### Data frame to plot
df_ggplot<- merge(x = Y, y = df_regular, by.x = "date", by.y = "date", all.x = TRUE, all.y = TRUE)
df_ggplot<- df_ggplot %>% add_row(merge(x = Y, y = df_weekend, by.x = "date", by.y = "date", all.x = TRUE,
                                        all.y = TRUE))
df_ggplot$type_model<- c(rep(x = "regular", times = nrow(df_regular)),
                         rep(x = "weekend effect", times = nrow(df_regular)))

### Calculating relative error
df_ggplot$re<-  ((df_ggplot$median - df_ggplot$n_new) / (abs(df_ggplot$n_new + 1)))*100

### Plot
colors_plot <- grey(level = c(0.8, 0.4))
names(colors_plot) <- c("regular", "weekend effect")

### Plot
ggplot(data = df_ggplot) +
  geom_boxplot(mapping = aes(x = weekday, y = re, fill = type_model), outlier.size = -1) + # plot boxplots of RE per weekday
  geom_hline(yintercept = 0, linetype = 1, color = "black", size = 1) + # reference line
  labs(x = "Day of the week", y = "Relative error") + # x- and y-labels
  ggtitle(state_name) + # plot title
  ylim(-68, 247) + 
  scale_colour_manual(name = "", values = colors_plot) +
  scale_fill_manual(name = "", values = colors_plot) +
  theme_bw() + # white backgroud
  theme(axis.text.x = element_text(angle = 90, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        legend.key.size = unit(0.5, "cm"), # optional settings for symbol size in legend
        legend.text = element_text(size = size_plot), # optional settings for legend size
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot), # optional settings for title
        strip.text = element_text(size = size_plot))
