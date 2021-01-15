## Section 15.6 ##

#require PandemicLP package
library(PandemicLP)

#### Listing 15.16 ####
head(covid19BH)   # the first six lines
tail(covid19BH)    # the last six lines

#### Listing 15.17 ####
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

#### Listing 15.18: it generates Figure 15.12 ####
class(dataBH) <- "pandemicData"
plot(x = dataBH) #new confirmed/deaths

#### Listing 15.19 ####FIT
fitBH <- pandemic_model(Y = dataBH, seed = 123)
#Run this to load a pre-executed fit
#fitBH <- readRDS("results/fitBH.rds")

#### Listing 15.20 ####
fitBHconfig < -pandemic_model(Y = dataBH, seed = 123,
                              covidLPconfig = TRUE)
#Run this to load a pre-executed fit
#fitBHconfig <- readRDS("results/fitBHconfig.rds")

fitBHconfig

#### Listing 15.21: it generates Figures 15.13 to 15.15 ####
#posterior sample, short e long term predictions and useful statistics:
predBH <- posterior_predict(fitBHconfig)
predBH

statsBH <- pandemic_stats(predBH)
statsBH

#plot of the results:
plotBH <- plot(x = predBH, term = "both")

plotBH$long    #plot long term predictions
plotBH$short   #plot short term predictions

#convergence diagnostics graphics:
traceplot(fitBHconfig) + theme(legend.position = "")
density(fitBHconfig)
stan_ac(fitBHconfig$fit, pars = c("a","b","c","f")) #autocorrelation plot



