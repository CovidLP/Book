###---------------------------------------------------------------###
###   Building a Platform for Data-Driven Pandemic Prediction:    ###
###   From Data Modelling to Visualization - the CovidLP Project  ###
###                                                               ###
###   Chapter 10: Investigating inference results                 ###
###   Section 10.4  Practical situation                           ###
###   Subsction 10.4.1 Overall comparison                         ###
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
df<- merge(x = Y, y = df_aux[,c(1, aux_seq_dates + 1)], by.x = "date", by.y = "date", all.x = TRUE, all.y = TRUE)
colnames(df)[6:11]<- format(x = as.Date(colnames(df)[6:11]), "%d/%b/%Y")

###------------------------------###------------------------------###------------------------------###

### R System in English
Sys.setlocale("LC_TIME", "C")

### Plot
colors_plot<- c("#999999", rep(x = "black", times = 6)) ; linetype_plot<- c(1, 1:6)
names(colors_plot)<- names(linetype_plot)<- c("observations", last(x = names(df)[-1], n = n_days))

ggplot(data = df, linetype = linetype_plot) +
  # geom_point(mapping = aes(x = date, y = n_new, colour = "observations")) + # plot observations as dots
  geom_line(aes(x = date, y = n_new, colour = "observations", linetype = "observations")) + # plot observations as dots + lines
  labs(x = "Date", y = "Number of confirmed cases") + # x- and y-labels
  ggtitle(country_name) + # plot title
  scale_x_date(breaks = seq(from = first(df$date), to = last(df$date), by = 21), date_labels = "%d/%b/%Y") + # specifies x-scale
  
  geom_line(mapping = aes(x = date, y = `14/Jun/2020`, colour = "14/Jun/2020", linetype = "14/Jun/2020"), size = size_line_plot) + 
  geom_line(mapping = aes(x = date, y = `10/Jul/2020`, colour = "10/Jul/2020", linetype = "10/Jul/2020"), size = size_line_plot) +
  geom_line(mapping = aes(x = date, y = `02/Aug/2020`, colour = "02/Aug/2020", linetype = "02/Aug/2020"), size = size_line_plot) + 
  geom_line(mapping = aes(x = date, y = `29/Aug/2020`, colour = "29/Aug/2020", linetype = "29/Aug/2020"), size = size_line_plot) + 
  geom_line(mapping = aes(x = date, y = `05/Oct/2020`, colour = "05/Oct/2020", linetype = "05/Oct/2020"), size = size_line_plot) + 
  geom_line(mapping = aes(x = date, y = `28/Oct/2020`, colour = "28/Oct/2020", linetype = "28/Oct/2020"), size = size_line_plot) + 
  
  scale_color_manual("Uptaded in", values = colors_plot) +
  scale_linetype_manual("Uptaded in", values = linetype_plot) +
  theme_bw() + # white backgroud
  theme(axis.text.x = element_text(angle = 90, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = size_plot),
        legend.title = element_text(size = size_plot),
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot), # optional settings for title
        strip.text = element_text(size = size_plot))





