###############################################
#### Listing 13.1 ####
###############################################

library(PandemicLP)
library(foreach)
library(doMC)

# list of countries provided by the monitoring team
countrylist <- c("Argentina","Bolivia","Chile","China",
                 "Colombia","Ecuador","France","Greece","India",
                 "Ireland","Korea, South","Mexico","Netherlands",
                 "New Zealand","Norway","Peru","Paraguay",
                 "Russia","South Africa","Uruguay","Sweden",
                 "Switzerland","Turkey","Venezuela","Costa Rica",
                 "Morocco","Panama","Philippines","Ukraine",
                 "Ethiopia","Guatemala","Honduras","Indonesia",
                 "Iraq")                    

# register cores
registerDoMC(cores = min(63,length(countrylist)))  # number of countries or maximum number of cores

# parallel loop over the countries to fit the model 
obj <- foreach(s = 1:length(countrylist)) %dopar% {
  
  # select country
  country_name <- countrylist[s]
  
  # load data
  covid_country <- load_covid(country_name = country_name) 
  
  # fit the model
  mod <- pandemic_model(covid_country, case_type = "confirmed",
                        n_waves = 1, covidLPconfig = TRUE)  
  
  # perform model prediction
  pred <- posterior_predict(mod, horizonLong = 500, 
                            horizonShort = 14) 
  
  # calculate summary statistics
  stats <- pandemic_stats(pred) 
  stats[[1]] <- NULL # removing the data from 1st position 
  # (the app uses the data coming from other object)
  
  # rename the lists and data.frame accordingly for the app
  names(stats) <- c("df_predict", "lt_predict", 
                    "lt_summary", "mu_plot")
  names(stats$df_predict) <- c("date", "q25", "med", "q975", "m") 
  names(stats$lt_predict) <- c("date", "q25", "med", "q975", "m") 
  names(stats$lt_summary) <- c("NTC25", "NTC500", "NTC975",
                               "high.dat.low", "high.dat.med",
                               "high.dat.upper", "end.dat.low",
                               "end.dat.med","end.dat.upper") 
  
  # prepare the list to be saved
  list_out <- list(df_predict = stats$df_predict,
                   lt_predict=stats$lt_predict,
                   lt_summary=stats$lt_summary, 
                   mu_plot = stats$mu_plot)
  name.to.save <- gsub(" ", "-", country_name)
  
  # saveRDS
  results_directory <- "/home/CovidLP/Covid/app_COVID19/STpredictions/"
  names(covid_country$data) <- c("date","n","d","n_new","d_new")
  name.file <- paste0(results_directory,name.to.save,'_',
                      colnames(covid_country$data)[2],'.rds') 
  saveRDS(list_out, file=name.file)
  
}
