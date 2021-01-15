## Section 16.2 ##

#require PandemicLP package
library(PandemicLP)

#### Listing 16.1 ####
#loading data:
indiaData <- load_covid(country_name = "india",
                        last_date = "2020-10-31")
#Run this to load the data
#indiaData <- readRDS("data/indiaData.rds")

#fitting:
fitindia_random <- pandemic_model(Y = indiaData,
                                  case_type = "deaths",
                                  warmup = 6000, seed = 123)
#Run this to load a pre-executed fit
#fitindia_random <- readRDS("results/fitindia_random.rds")

#including initial values for chains:
fitindia_init <- pandemic_model(Y = indiaData,
                                case_type = "deaths",
                                warmup = 6000, seed = 123,
                                init = list(list(a = 42.02, b = 0.497, c = 0.013, f = 12.25)))
#Run this to load a pre-executed fit
#fitindia_init <- readRDS("results/fitindia_init.rds")

#### Listing 16.3 ####
#necessary to run: (Listing 15.17)
data <- covid19BH
names(data) <- c("date", "new_cases", "new_deaths", "cases",
                 "deaths", "population")
data$date <- as.Date(data$date)
data <- data[order(data$date, decreasing = FALSE), ]

pop <- data$population[1]
dataBH <- list(data = data, name = "Belo Horizonte/MG",
               population = pop)  #the appropriate list
#Run this to load the data
#dataBH <- readRDS("data/dataBH.rds")

fitBH_control <- pandemic_model(Y = dataBH, seed=123,
                                control = list(adapt_delta = 0.95))
#Run this to load a pre-executed fit
#fitBH_control <- readRDS("results/fitBH_control.rds")

#### Listing 16.4 ####
COLdata <- load_covid(country_name = "colombia",
                      last_date = "2020-07-15")
#Run this to load the data
#COLdata <- readRDS("data/COLdata.rds")

fitCOL <- pandemic_model(Y = COLdata, case_type = "deaths",
                         warmup = 6000, seed = 123,
                         control = list(max_treedepth = 40, adapt_delta = 0.95))
#Run this to load a pre-executed fit
#fitCOL <- readRDS("results/fitCOL.rds")

fitCOL_10 <- pandemic_model(Y = COLdata, case_type = "deaths",
                            warmup = 6000, thin = 10, seed = 123,
                            control = list(max_treedepth = 40, adapt_delta = 0.95))
#Run this to load a pre-executed fit
#fitCOL_10 <- readRDS("results/fitCOL_10.rds")

#### Listing 16.5:  it generates Figure 16.1 ####
#convergence diagnostics for fitCOL
traceplot(fitCOL) + theme(legend.position = " ")
stan_ac(fitCOL$fit, pars = c("a", "b", "c", "f"))
#convergence diagnostics for fitCOL_10
traceplot(fitCOL_10)
stan_ac(fitCOL_10$fit, pars = c("a", "b", "c", "f"))
