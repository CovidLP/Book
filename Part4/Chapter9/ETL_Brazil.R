#########################################################
## Preparing Brazilian data
#########################################################
library(dplyr)
last_date <- '2020-10-01'
state_name <- "MG"

#########################################################
#### Listing 9.2 ####
#########################################################
# set repository address 
baseURLbr <- "https://raw.githubusercontent.com/covid19br/covid19br.github.io/master/dados"

# download states files 
covidbr <- read.csv(file.path(baseURLbr,"EstadosCov19.csv"), check.names=FALSE, stringsAsFactors=FALSE)

#########################################################
#### Listing 9.4 ####
#########################################################
# For a given state, create the data frame by
# cleaning, renaming and setting initial and final dates 
Y <- covidbr %>%
  rename(name=estado, date=data,
         cases=casos.acumulados, deaths=obitos.acumulados,
         new_cases=novos.casos, new_deaths=obitos.novos) %>%
  mutate(date = as.Date(date)) %>%
  arrange(name, date) %>% filter(date >= '2020-01-23' & date <= last_date & name == state_name) %>%
  select(date, cases, deaths, new_cases, new_deaths)

#########################################################
#### Listing 9.5 - Adpated for Brazil ####
#########################################################
#set repository path
popURL = "https://raw.githubusercontent.com/CovidLP/app_COVID19/master/R/pop"

#load the world and Brazilian population files
br_pop <- read.csv(file.path(popURL,"pop_BR.csv"))

#set the pop variable for a state in Brazil
pop <- as.numeric(br_pop$pop[which(br_pop$uf == state_name)])

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