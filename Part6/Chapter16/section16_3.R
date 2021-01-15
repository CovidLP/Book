## Section 16.3 ##

#require PandemicLP package
library(PandemicLP)

#necessary to load 'indiaData':
indiaData <- load_covid(country_name = "india",
                        last_date = "2020-10-31")
#Run this to load the data
#indiaData <- readRDS("data/indiaData.rds")

#### Listing 16.6 ####
#specifying initial values for the four chains:
init4 <- list()
for(i in 1:4){
  init4[[i]] <- list(a = 42.02+(i-1), b = 0.497+(i-1)/10,
                     c = 0.013+(i-1)/100 , f = 12.25-(i-1))
}
#fitting model
fitindia_chains <- pandemic_model(Y = indiaData,
                                  case_type = "deaths", warmup = 6000,
                                  chains = 4, init = init4, seed = 123)
#Run this to load a pre-executed fit
#fitINDIA_chains <- readRDS("results/fitindia_chains.rds")

#### Listing 16.7 ####
summary(fitindia_chains)

#### Listing 16.8: it generates Figure 16.2 ####
traceplot(fitindia_chains)
density(fitindia_chains)
stan_ac(fitindia_chains$fit, pars = c("a", "b", "c", "f"))

