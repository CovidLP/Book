library(PandemicLP)
library(dplyr)
library(plotly)
setwd("~/Documents/COVID19/Livro/")
filepath <- "figs/newplot.svg"

#### Figure 15.6 ####
us2W_15.6 = load_covid("US", last_date="2020-07-18")
# us2W_15.6 = readRDS("data/1506_us.rds") # Run this to load the data from a file

g = plot(us2W_15.6)
g

#### Figure 15.7 ####
us2W_est = pandemic_model(us2W_15.6,n_waves=2, covidLPconfig = TRUE, seed=123)
saveRDS(us2W_est,"../Livro/supplementary/part2/cap15/results/1506_us.rds")
# us2W_est = readRDS("results/1506_us.rds") # Run this to load the data from a file

us2W_pred = posterior_predict(us2W_est, horizonLong = 100)
plot(us2W_pred, summary = FALSE)$long