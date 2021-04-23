rm(list=ls())

### packages
library(dplyr)
library(tidyr)
library(ggplot2)


### the data to generate plot in (3) may be:
### (1) downloaded from the Fig_12_2.rds file; or
### (2) generated from the John Hopkins repository


### (1) download data from "rds" file
t0 = as.Date('2020-03-01')
t = as.Date('2020-05-31')
# covid_country <- readRDS("Figure_12.2.rds")
covid_country <- readRDS("Part2/Chapter6/data/Figure_12.2.rds")



### (2) generate plot data from the John Hopkins repository
# set repository addres
baseURL  <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"
# download countries files
covidworld <- read.csv(file.path(baseURL ,"time_series_covid19_confirmed_global.csv"), check.names=FALSE, stringsAsFactors=FALSE)
# transform countries files
covid19 <- covidworld %>% select(-Lat,-Long) %>%
  pivot_longer (-(1:2), names_to="date", values_to="confirmed") %>% 
  mutate(date=as.Date(date , format="%m/%d/%y")) %>%
  rename(country='Country/Region', state='Province/State')
# dates range
t0 = as.Date('2020-03-01')
t = as.Date('2020-05-31')
# plot object
covid_country <- covid19 %>% filter(country=='Spain') %>%
  group_by(date) %>% summarise(n = sum(confirmed, na.rm=T)) %>%
  arrange(date) %>% mutate(cases = n - lag(n, default=0)) %>%
  select(date, cases) %>%
  filter(date >= t0 , date <= t)


### (3) plot data from "covid_country"
Sys.setlocale("LC_TIME", "C")
size_plot = 12
plt = ggplot(data = covid_country, mapping = aes(x=date,y=cases)) + theme_minimal() +
  scale_x_date(breaks = seq(t0,t,by=7), date_labels="%d/%b/%Y", limits = c(t0,t)) +
  scale_y_continuous(breaks = seq(-1e4, 1e4, by = 2e3)) +
  theme_bw() +
  theme(axis.title.y = element_text(size = size_plot),
        axis.text.y = element_text(size = size_plot),
        axis.text.x = element_text(angle = 90, size = size_plot),
        plot.title = element_text(hjust = 0.5, size = size_plot)) +
  geom_hline(yintercept=0, color='gray60') +
  geom_line(color = "gray30") + geom_point(color = "gray30", size=.9) +
  theme(plot.title = element_text(hjust=0.5,size=12, color='gray30')) +
  labs(x="Date", y="Number of confirmed cases")
plt


