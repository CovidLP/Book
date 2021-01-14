#### Subsection 15.4.1 ####


#### Listing 15.3: generated Figure 15.2 ####
library(PandemicLP)

india <- load_covid(country_name = "india", last_date = "2020-10-12")
#Run this to load the data
#india <- readRDS("data/india.rds")

plot(x = india, cases = "new")


#### Listing 15.4 ####
fitindia <- pandemic_model(Y = india, case_type =  "deaths",
                           n_waves = 1, seed = 123, covidLPconfig = TRUE)
#Run this to load a pre-executed fit
#fitindia <- readRDS("results/fitindia.rds")
fitindia


#### Listing 15.5: generated Figure 15.3 ####
predindia <- posterior_predict(object = fitindia, horizonLong = 365,
                               horizonShort = 7)

statsindia <- pandemic_stats(object = predindia)
statsindia

plotindia <- plot(x = predindia, term = "long")
plotindia$long
