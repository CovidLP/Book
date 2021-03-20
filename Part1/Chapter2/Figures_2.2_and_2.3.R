###---------------------------------------------------------------###
###   Building a Platform for Data-Driven Pandemic Prediction:    ###
###   From Data Modelling to Visualization - the CovidLP Project  ###
###                                                               ###
###   Chapter 2: Pandemic data                                    ###
###   Section 2.4: Data reconstruction                            ###
###   Figure 2.2 and Figure 2.3                                   ###
###                                                               ###
###   Author: the CovidLP Team                                    ###
###---------------------------------------------------------------###

### Removing allocated objects
rm(list = ls())

###-----------------------------
### Packages
###-----------------------------
library(lubridate)
library(tidyverse)

###-----------------------------
### Setting directories
data_directory<- "Part1/Chapter2/data/"
###-----------------------------

###-----------------------------
### Graphical specifications (you may need to adapt these settings)
linetype_plot <- 1.2
size_line_plot <- 1.2
size_plot <- 15
vjust_plot <- 1.8
theme_plot <- theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 0, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        legend.key.size = unit(0.5, "cm"),
        legend.position = c(0.2, 0.7),
        legend.text = element_text(size = size_plot),
        legend.title = element_blank(),
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot),
        strip.text = element_text(size = size_plot))  # optional settings for title) 
###-----------------------------

### R system in English
Sys.setlocale("LC_TIME", "C")

###-----------------------------
### Reading data
dadosBR2 <- read_csv(paste0(data_directory, "dadosBR2.csv"))
###-----------------------------


dadosBR <- dadosBR2 %>% 
  mutate_at( vars(starts_with("DT_")), dmy  ) %>% 
  filter(DT_NOTIFIC >= "2020-02-26", DT_SIN_PRI <= DT_DIGITA) 

dadosBR2 <- dadosBR2 %>% 
  mutate_at( vars(starts_with("DT_")), dmy  ) %>% 
  filter(DT_NOTIFIC >= "2020-02-26", DT_SIN_PRI <= DT_DIGITA) 


dadosBR.ag <- dadosBR %>% 
  # Seis meses de dados
  filter(DT_DIGITA < "2020-09-01") %>% 
  group_by( Date = DT_DIGITA ) %>% 
  tally(name = "Notification date") %>% 
  right_join( 
    dadosBR %>% 
      filter(DT_DIGITA < "2020-09-01") %>% 
      group_by( Date = DT_SIN_PRI) %>% 
      tally(name = "Occurrence date"),
    by = "Date") %>%
  gather(key = "Time", value = "Cases", -Date) 


###-------------------###
###   FIGURE 2.2 (a)  ###
###-------------------###

dadosBR.ag %>% 
  #filter(Date < "2020-09-01") %>% 
  ggplot(aes(x = Date, y = Cases, linetype = Time)) +
  geom_line() + 
  ylab("Number of cases") + 
  scale_linetype_manual(values = c("dashed", "solid") )+
  scale_size_manual(values = c(1, 1.5)) +
  scale_x_date(date_breaks = "months", date_labels = "%b") + 
  theme_plot + 
  ggtitle("New hospitalized COVID-19 cases by date in Brazil")


###------------------------------###------------------------------###------------------------------###

###-------------------###
###   FIGURE 2.2 (b)  ###
###-------------------###

dates.a <- c("2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01", "2020-09-01")

aux <- map(dates.a, function(x) dadosBR %>% 
             #filter(DT_DIGITA <= x, DT_SIN_PRI <= "2020-05-01") %>% 
             filter(DT_DIGITA <= x) %>% 
             group_by( Date = DT_SIN_PRI ) %>% tally(name = "Cases") %>% 
             mutate(
               `Release date` = x
             )
)

aux <- bind_rows(aux)
aux$`Release date`<- format(x = as.Date(aux$`Release date`), "%d/%b/%Y")

aux %>% 
  ggplot(aes(x = Date, y = Cases, linetype = `Release date`)) +
  geom_line(show.legend = TRUE) + 
  ylab("Number of cases") + 
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 0, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        legend.key.size = unit(0.5, "cm"),
        legend.position = c(0.18, 0.67),
        legend.text = element_text(size = size_plot),
        legend.title = element_text(size = size_plot),
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot)) +
  ggtitle("New hospitalized COVID-19 cases in Brazil") 


###---------------###
###   FIGURE 2.3  ###
###---------------###

aux4 <- dadosBR2 %>% 
  filter(DT_DIGITA < "2020-09-01") %>% 
  mutate(
    Date_Epiweek = epiweek(DT_SIN_PRI)
  ) %>% 
  group_by(Date_Epiweek) %>% 
  summarise(
    `SARS-CoV-2 positive` = sum(PCR == 1),
    `SARS-CoV-2 negative` = sum(PCR == 0),
  ) %>% gather(key = "Test result", value = "Cases", -Date_Epiweek)

aux4 %>% group_by(`Test result`) %>% summarise(Cases = sum(Cases))

aux4 %>% 
  mutate(
    `Test result` = factor(`Test result`, 
                           levels = c("SARS-CoV-2 positive", "SARS-CoV-2 negative")
    )
  ) %>% 
  ggplot(aes( x = Date_Epiweek, y = Cases, linetype = `Test result`)) +
  geom_line(show.legend = T) + 
  ylab("Number of hospitalized SARI cases") + 
  xlab("Week") + 
  scale_size_manual(values = c(1, 1.5)) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 0, size = size_plot), # optional settings for x-axis
        axis.text.y = element_text(size = size_plot), # optional settings for y-axis
        axis.title.x = element_text(size = size_plot, vjust = -vjust_plot), # optional settings for x-axis
        axis.title.y = element_text(size = size_plot, vjust = vjust_plot), # optional settings for y-axis
        legend.key.size = unit(0.5, "cm"),
        legend.position = c(0.22, 0.75),
        legend.text = element_text(size = size_plot),
        legend.title = element_text(size = size_plot),
        plot.margin = unit(x = c(1, 1, 1, 0.5), units = "cm"), # optional settings for margins
        plot.title = element_text(hjust = 0.5, size = size_plot)) +
  ggtitle("Hospitalized SARI cases tested for SARS-CoV-2 in Brazil") 


