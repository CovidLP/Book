#########################################################
## Preparing World data
#########################################################
library(dplyr)
library(tidyr)
country_name <- "US"

#########################################################
#### Listing 9.1 ####
#########################################################
# set repository address
baseURL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

# download countries files
covidworld <- read.csv(file.path(baseURL,"time_series_covid19_confirmed_global.csv"), check.names=FALSE, 
                       stringsAsFactors=FALSE)

#########################################################
#### Listing 9.3 ####
#########################################################
# For a given country, create the data frame by
# cleaning, renaming and setting initial and final dates

# prepare the data of confirmed cases 
covid19_confirm <- covidworld %>%  select(-Lat, -Long) %>%
  pivot_longer(-(1:2), names_to="date", values_to="confirmed") %>%
  mutate(date=as.Date(date, format="%m/%d/%y")) %>%
  rename(country=`Country/Region`, state=`Province/State`)

# prepare the data of confirmed deaths
covid19_deaths <- read.csv(file.path(baseURL,"time_series_covid19_deaths_global.csv"), check.names=FALSE, stringsAsFactors=FALSE) %>%
  select(-Lat, -Long) %>%
  pivot_longer(-(1:2), names_to="date", values_to="deaths") %>%
  mutate(date=as.Date(date, format="%m/%d/%y")) %>%
  rename(country=`Country/Region`, state=`Province/State`)

# join the confirmed and death data into one data frame
covid19 <- left_join(covid19_confirm, covid19_deaths,
                     by = c("state", "country", "date"))

# create the object for one country
Y <- covid19 %>% filter(country==country_name) %>%
  mutate(confirmed_new = confirmed - lag(confirmed, default=0),
         deaths_new = deaths - lag(deaths, default=0)) %>%
  arrange(date,state) %>% group_by(date) %>%
  summarize(cases = sum(confirmed, na.rm=T),
            deaths = sum(deaths, na.rm=T),
            new_cases = sum(confirmed_new, na.rm=T),
            new_deaths = sum(deaths_new, na.rm=T)) %>%
  arrange(date) %>% filter(date>='2020-01-23')

#########################################################
#### Listing 9.5 - Adpated for world ####
#########################################################
#set repository path
popURL = "https://raw.githubusercontent.com/CovidLP/app_COVID19/master/R/pop"

#load the world and Brazilian population files
country_pop <- read.csv(file.path(popURL,"pop_WR.csv"))

#set the pop variable for a country
pop <- country_pop$pop[which(country_pop$country==country_name)]

#########################################################
#### Listing 9.6 ####
#########################################################
# performed the count fix for confirmed cases
while(any(Y$new_cases < 0)){
  pos <- which(Y$new_cases < 0)
  for(j in pos){
    Y$new_cases[j-1] = Y$new_cases[j] + Y$new_cases[j-1]
    Y$new_cases[j] = 0
    Y$cases[j-1] = Y$cases[j]
  }
}

# performed the count fix for confirmed deaths
while(any(Y$new_deaths < 0)){
  pos <- which(Y$new_deaths < 0)
  for(j in pos){
    Y$new_deaths[j-1] = Y$new_deaths[j] + Y$new_deaths[j-1]
    Y$new_deaths[j] = 0
    Y$deaths[j-1] = Y$deaths[j]
  }
}