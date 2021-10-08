###############################################
#### Listing 10.7 ####
###############################################

library(PandemicLP)
library(foreach)
library(doMC)

# get the Brazilian state list
states <- state_list()
state_list <- states$state_abb

# register cores
registerDoMC(cores = min(63,length(state_list)))  # number of states or maximum number of cores

# parallel loop over the countries to fit the model 
obj <- foreach(s = 1:length(state_list)) %dopar% {
  
  # set state name
  state_name <- state_list[s]
  
  # load data
  covid_state <- load_covid(country_name = "Brazil",
                            state_name = state_name) 
  
  # fit the model
  mod <- pandemic_model(covid_state, case_type = "confirmed",
                        n_waves = 1,
                        seasonal_effect = c("sunday","monday"),
                        covidLPconfig = TRUE)
  
  # perform model prediction
  pred <- posterior_predict(mod, horizonLong = 500,
                            horizonShort = 14)
  
  # calculate summary statistics
  stats <- pandemic_stats(pred) 
  stats[[1]] <- NULL # removing the data (the app uses the data coming from other object)
  
  # rename the lists and data.frame accordingly for the app
  names(stats) <- c("df_predict","lt_predict","lt_summary",
                    "mu_plot")
  names(stats$df_predict) <- c("date", "q25", "med", "q975", "m") 
  names(stats$lt_predict) <- c("date", "q25", "med", "q975", "m") 
  names(stats$lt_summary) <- c("NTC25", "NTC500", "NTC975",
                               "high.dat.low", "high.dat.med",
                               "high.dat.upper", "end.dat.low",
                               "end.dat.med", "end.dat.upper") 
  
  list_out <- list(df_predict = stats$df_predict,
                   lt_predict=stats$lt_predict,
                   lt_summary=stats$lt_summary, 
                   mu_plot = stats$mu_plot)
  
  # saveRDS - Summary files
  results_directory = "/home/CovidLP/Covid/app_COVID19/STpredictions/"
  names(covid_state$data) <- c("date","n","d","n_new","d_new")
  file_id <- paste0(state_list[s],'_',
                    colnames(covid_state$data)[2],'e')
  saveRDS(list_out,
          file=paste0(results_directory,'Brazil_',file_id,'.rds'))
  
  # saveRDS - The posterior predict chains (necessary to create the aggregate model of Brazil
  results_directory = "/home/CovidLP/Covid/app_COVID19/STpredictions/"
  file_id <- paste0(state_list[s],'_posterior_predict_',
                    colnames(covid_state$data)[2],'e')
  saveRDS(pred, file = paste0(results_directory,file_id,'.rds'))
}
