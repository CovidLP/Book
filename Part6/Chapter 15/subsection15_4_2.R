## Subsection 15.4.2 ##

#require PandemicLP package
library(PandemicLP)

#### Listing 15.6: it generates Figure 15.4 (including deaths time series) ####
dataMG <- load_covid(country_name = "Brazil",  state_name = "MG",
                     last_date = "2020-10-15")
#Run this to load the data
#dataMG <- readRDS("data/dataMG.rds")

plot(x = dataMG) #new confirmed/deaths

#### Listing 15.7 ####
fitMG <- pandemic_model(Y = dataMG,
                        seasonal_effect = c("sunday", "monday"),
                        covidLPconfig = TRUE, seed = 123)
#Run this to load a pre-executed fit
#fitMG <- readRDS("results/fitMG.rds")

fitMG

#### Listing 15.8: it generates Figure 15.5 ####
traceplot(fitMG) + theme(legend.position = "")
stan_ac(fitMG$fit, pars = c("a","b","c","f","d_1","d_2"))
density(fitMG)

#### Listing 15.9 ####
predMG <- posterior_predict(fitMG, horizonLong = 100)
predMG

#### Listing 15.10 ####
statsMG <- pandemic_stats(predMG)
statsMG

##### Listing 15.11: it generates Figures 15.6 e 15.7 ####
plot(x = predMG)  #default=plot long term predictions
plot(x = predMG, term = "short") #plot short term predictions
