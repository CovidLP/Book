rm(list=ls())



### packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)



# Dates
t0 = as.Date("2020-03-01")
t <- as.Date("2020-10-16")



### The data to generate plot in (3) may be:
### (1) downloaded from the "rds" files; or
### (2) downloaded from the John Hopkins repository



# (1) Download data from the "rds" files
# data_rds <- paste0("C:/_covid_T5/2020-07-18/")
data_rds <- paste0("Part2/Chapter6/data/")
data_US <- readRDS(paste0(data_rds,"US_n2_azzalini.rds"))
covid19country <- data_US$Yobs



# (2) Download US data from the John Hopkins repository
baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

# Function to load the US data (cases)
loadDataUS_cases = function(fileName, columnName) {
  data = read.csv(file.path(baseURL, fileName), check.names=FALSE, stringsAsFactors=FALSE) %>%
    filter(iso2=="US") %>%
    select(-UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Lat, -Long_, -Combined_Key) %>%
    pivot_longer(-(1:2), names_to="date", values_to=columnName) %>%
    mutate(date=as.Date(date, format="%m/%d/%y")) %>%
    rename(country = Country_Region, state = Province_State)
  data = data %>% group_by(state, country, date) %>% summarise(confirmed=sum(confirmed))
  save(data, file=fileName)  
  return(data)
}

# Function to load the US data (deaths)
loadDataUS_deaths = function(fileName, columnName) {
  data = read.csv(file.path(baseURL, fileName), check.names=FALSE, stringsAsFactors=FALSE) %>%
    filter(iso2=="US") %>%
    select(-UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Lat, -Long_, -Combined_Key) %>%
    pivot_longer(-(1:3), names_to="date", values_to=columnName) %>%
    mutate(date=as.Date(date, format="%m/%d/%y")) %>%
    rename(country = Country_Region, state = Province_State, pop = Population)
  data = data %>% group_by(state, country, date) %>% summarise(deaths=sum(deaths), pop=sum(pop))
  save(data, file=fileName)
  return(data)
}

covid19_confirm <- loadDataUS_cases("time_series_covid19_confirmed_US.csv", "confirmed")
covid19_deaths <- loadDataUS_deaths("time_series_covid19_deaths_US.csv", "deaths")
covid19 <- left_join(covid19_confirm,covid19_deaths, by=c('state','country','date'))

covid19country <- covid19 %>% mutate(state = "US") %>%
  group_by(date) %>% summarise(n = sum(confirmed, na.rm=T), d = sum(deaths, na.rm=T)) %>%
  mutate(n_new = n - lag(n, default=0),  d_new = d - lag(d, default=0)) %>%
  select(date, n, d, n_new, d_new) %>% arrange(date) %>% filter(date>="2020-03-01")



### (3) Figure 6.8

# Data objects
obs_US  <- data.frame(date = seq(t0,max(covid19country$date),by="day"))
obs_US[["US"]][which(obs_US$date %in% covid19country$date)] <- covid19country$n_new

# Graphic objects
plot_obs_US <- reshape2::melt(obs_US %>% dplyr::select(date,US), id.vars='date', variable.name='series')

# Graphic
Sys.setlocale("LC_TIME", "C")
size_plot = 10

ggplot() + ylab("New cases per day") + xlab("") + ggtitle("") +
  xlim(t0,t) + ylim(0,1e5) + labs(x="Date", y="Number of confirmed cases") + 
  scale_x_date(breaks = seq(t0,t,by=7), date_labels="%d/%b/%Y", limits = c(t0,t)) +
  scale_y_continuous(breaks = seq(0, 1e5, by = 2e4)) +
  theme_bw() +
  theme(axis.title.y = element_text(size = size_plot),
        axis.text.y = element_text(size = size_plot),
        axis.text.x = element_text(angle = 90, size = size_plot),
        axis.title.x = element_text(size = size_plot),
        strip.text = element_text(size = size_plot),
        plot.title = element_text(hjust = 0.5, size = size_plot)) +
  geom_line(aes(x=date,y=value,group=series), data=plot_obs_US %>% filter(date<t), linetype="solid", color="gray50", size=1)

