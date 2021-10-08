###############################################
#### Listing 10.10 ####
###############################################

library(PandemicLP)

# get the Brazilian state list
states <- state_list()
uf <- states$state_abb

# directory where the data is stored
dir_rds <- "/home/CovidLP/Covid/app_COVID19/STpredictions" 

# read the posterior file for the first state
state_nm <- paste0(dir_rds,'/',uf[1],"_posterior_predict_ne.rds")
data_base <- readRDS(state_nm)
uf_2 <- uf[-1]

# remove posterior_predict file of the first state from directory (to save server space)
file.remove(state_nm)

# get the mean sample and set it to be dates x mcmc sample
mu_t <- t(data_base$pastMu)

# include the dates in the data frame
mu_final <- data.frame(data = data_base$data$date,mu_t)
names_mu <- names(data_base$pastMu)

# get hidden objects (necessary for the pandemic_stats function)
hidden_short_total <- methods::slot(data_base$fit,"sim")$fullPred$thousandShortPred 
hidden_long_total <- methods::slot(data_base$fit,"sim")$fullPred$thousandLongPred 
hidden_mu_total <- methods::slot(data_base$fit,"sim")$fullPred$thousandMus

# loop for each state - starting with the second one 
for (u in uf_2) {
  
  # rds import for the selected state
  state_nm <- paste0(dir_rds,'/',u,"_posterior_predict_ne.rds")
  data_uf <- readRDS(state_nm)
  
  # sum the variables predictive_Long, predictive_Short and futMu 
  data_base$predictive_Long <- data_base$predictive_Long +
    data_uf$predictive_Long
  data_base$predictive_Short <- data_base$predictive_Short +
    data_uf$predictive_Short
  data_base$futMu <- data_base$futMu + data_uf$futMu
  
  # create a large data frame by concatenating samples for current state in the mean data frame
  mu_t <- t(data_uf$pastMu)
  mu_2 <- data.frame(data = data_uf$data$date,mu_t)
  names_mu <- c(names_mu,names(data_uf$pastMu))
  mu_final <- rbind(mu_final,mu_2)
  
  # merge datasets by date since they can differ on start
  data_base$data <- merge(data_base$data,data_uf$data, 
                          by = "date", all = TRUE)
  data_base$data[is.na(data_base$data)] = 0
  data_base$data$cases.x = data_base$data$cases.x +
    data_base$data$cases.y
  data_base$data$deaths.x = data_base$data$deaths.x +
    data_base$data$deaths.y
  data_base$data$new_cases.x = data_base$data$new_cases.x +
    data_base$data$new_cases.y
  data_base$data$new_deaths.x = data_base$data$new_deaths.x +
    data_base$data$new_deaths.y
  data_base$data <- data_base$data[,-c(6:9)]
  names(data_base$data) <- c("date","cases","deaths",
                             "new_cases","new_deaths")
  
  # sum hidden objects (necessary for the pandemic_stats function) 
  hidden_short_uf <- methods::slot(data_uf$fit,"sim")$fullPred$thousandShortPred 
  hidden_short_total <- hidden_short_total + hidden_short_uf
  hidden_long_uf <- methods::slot(data_uf$fit,"sim")$fullPred$thousandLongPred 
  hidden_long_total <- hidden_long_total + hidden_long_uf
  hidden_mu_uf <- methods::slot(data_uf$fit,"sim")$fullPred$thousandMus
  hidden_mu_total <- hidden_mu_total + hidden_mu_uf
  
  # remove posterior_predict file from directory (to save server space)
  file.remove(state_nm)
}

# create hidden object (necessary for the pandemic_stats function)
methods::slot(data_base$fit,"sim")$fullPred$thousandShortPred <- hidden_short_total
methods::slot(data_base$fit,"sim")$fullPred$thousandLongPred <- hidden_long_total
methods::slot(data_base$fit,"sim")$fullPred$thousandMus <- hidden_mu_total

# aggregate the mean samples
mu_final <- aggregate(. ~ data, data=mu_final, FUN=sum)
mu_final <- mu_final[,-1]
mu_final <- t(mu_final)
names_mu <- unique(names_mu)
colnames(mu_final) <- names_mu
data_base$pastMu <- mu_final

# calculate summary statistics
stats <- pandemic_stats(data_base) # calculate stats
stats[[1]] <- NULL # removing the data (the app use the data coming from other object)

# rename the lists and data.frame accordingly for the app
names(stats) <- c("df_predict","lt_predict","lt_summary",
                  "mu_plot")
names(stats$df_predict) <- c("date", "q25",  "med",  "q975", "m") 
names(stats$lt_predict) <- c("date", "q25",  "med",  "q975", "m") 
names(stats$lt_summary) <- c("NTC25","NTC500","NTC975",
                             "high.dat.low","high.dat.med",
                             "high.dat.upper","end.dat.low",
                             "end.dat.med","end.dat.upper") 

# prepare the list to be saved
list_out <- list(df_predict = stats$df_predict,
                 lt_predict=stats$lt_predict,
                 lt_summary=stats$lt_summary,
                 mu_plot = stats$mu_plot)

i = 4 # for cases
# saveRDS - aggregated by the states
results_directory = "/home/CovidLP/Covid/app_COVID19/STpredictions/"
names(data_base$data) <- c("date","n","d","n_new","d_new")
file_id <- colnames(data_base$data)[i-2]
saveRDS(list_out, 
        file=paste0(results_directory,'Brazil_',file_id,'.rds'))
