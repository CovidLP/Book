## Section 16.4 ##

#require PandemicLP package
library(PandemicLP)

#### Listing 16.9: it generates Figure 16.3 and 16.4 ####
#loading data:
USdata <- load_covid(country_name = "us",
                     last_date = "2020-03-20")
#Run this to load the data
#USdata <- readRDS("data/USdata.rds")

#fitting model
fitUS_p1 <- pandemic_model(Y = USdata, p = 1, warmup = 6000,
                           seed = 123,
                           control = list(max_treedepth = 50, adapt_delta = 0.99))
#Run this to load a pre-executed fit
#fitUS_p1 <- readRDS("results/fitUS_p1.rds")

fitUS_p08 <- pandemic_model(Y = USdata, warmup = 6000,
                            seed = 123,
                            control = list(max_treedepth = 50, adapt_delta = 0.99))
#Run this to load a pre-executed fit
#fitUS_p08 <- readRDS("results/fitUS_p08.rds")

#posterior sample, long and short-term predictions and plot: generates Figure 16.4
predUS_p1 <- posterior_predict(fitUS_p1)
plot(predUS_p1)
predUS_p08 <- posterior_predict(fitUS_p08)
plot(predUS_p08)

#generates Figure 16.3
plot(USdata)

