rm(list=ls())



### packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(data.table)
library(matrixStats)
library(rstan)




### Dates and states
t0 = as.Date("2020-03-01")
t <- as.Date("2020-10-16")
t1 = as.Date("2021-08-30")



### Next contents
#
# (1) Generate Figure 6.9 from the "rds" files available in the Github path "Part2/Chapter6/data/"
# (2) Download US data from John Hopkins repository
# (3) Run generalised logistic model with 2 curves using Stan and generate the "lt_predict" object used in Figure 6.9
#
# The codes in (2) and (3) generate "rds" files with objects to generate Figure 6.9. These "rds" files
# are reduced when compared to the ones in "Part2/Chapter6/data/", but have enough information to
# generate Figure 6.9 with the codes in (1). 



### (1) Generate Figure 6.9

# "rds" file
data_rds <- paste0("C:/_covid_T7/",t)
data_US <- readRDS(paste0(data_rds,"/US_n3_azzalini.rds"))
# data_rds <- paste0("Part2/Chapter6/data/")
# data_US <- readRDS(paste0(data_rds,"US_n3_gen.rds"))

# Data objects
covid19obs <- data_US$Yobs %>% filter(date>=t0,date<=t)
obs_US  <- data.frame(date = seq(t0,t ,by="day"))
med_US  <- data.frame(date = seq(t0,t1,by="day"))
mu_US <- data_US$mu_plot

obs_US[["US"]][which(obs_US$date %in% covid19obs$date)] <- covid19obs$n_new
med_US$US = NA_real_
med_US$USq25 = NA_real_
med_US$USq975 = NA_real_
med_US[["US"]][which(med_US$date %in% data_US$lt_predict$date)] <- data_US$lt_predict$med
med_US[["USq25"]][which(med_US$date %in% data_US$lt_predict$date)] <- data_US$lt_predict$q25
med_US[["USq975"]][which(med_US$date %in% data_US$lt_predict$date)] <- data_US$lt_predict$q975

# Graphic objects
plot_obs_US <- reshape2::melt(obs_US %>% dplyr::select(date,US), id.vars='date', variable.name='series')
plot_mu_US  <- reshape2::melt(mu_US %>% select(date,mu50), id.vars='date', variable.name='series')
plot_med_US  <- reshape2::melt(med_US %>% dplyr::select(date,US) %>% filter(date>t), id.vars='date', variable.name='series')

# Figure 6.9
Sys.setlocale("LC_TIME", "C")
size_plot = 10

ggplot() + ylab("New cases per day") + xlab("") +
  
  ggtitle("") + xlim(t0,t) + ylim(0,1e5) + labs(x="Date", y="New cases per day") + 
  
  scale_x_date(breaks = seq(t0,t+180,by=21), date_labels="%d/%b/%Y", limits = c(t0,t+180)) +
  scale_y_continuous(breaks = seq(0, 1e5, by = 2e4)) +
  theme_bw() +
  theme(axis.title.y = element_text(size = size_plot),
        axis.text.y = element_text(size = size_plot),
        axis.text.x = element_text(angle = 90, size = size_plot),
        axis.title.x = element_text(size = size_plot),
        strip.text = element_text(size = size_plot),
        plot.title = element_text(hjust = 0.5, size = size_plot)) +

  geom_ribbon(aes(x=date,ymin=USq25,ymax=USq975), data=med_US, alpha=.8, colour = NA, fill='gray70') +
  geom_line(aes(x=date,y=value,group=series), data=plot_obs_US %>% filter(date<t), linetype="solid", color="#999999", size=1) +
  geom_vline(xintercept=t, linetype="solid", size=0.1) +
  geom_line(aes(x=date,y=value,group=series), data=plot_mu_US, linetype="solid", color="gray30", size=1)





### (2) Download US data from the John Hopkins repository
baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"
# Function to load the US data (cases)
loadDataUS_cases = function(fileName, columnName) {
  data = read.csv(file.path(baseURL, fileName), check.names=FALSE, stringsAsFactors=FALSE) %>%
    filter(iso2=="US") %>%
    select(-UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Lat, -Long_, -Combined_Key) %>%
    pivot_longer(-(1:2), names_to="date", values_to=columnName) %>%
    mutate(date=as.Date(date, format="%m/%d/%y")) %>%
    rename(country = Country_Region, state = Province_State)
  data = data %>% group_by(state, country, date) %>% summarise(confirmed=sum(confirmed))
  save(data, file=fileName)  
  return(data)
}
# Function to load the US data (deaths)
loadDataUS_deaths = function(fileName, columnName) {
  data = read.csv(file.path(baseURL, fileName), check.names=FALSE, stringsAsFactors=FALSE) %>%
    filter(iso2=="US") %>%
    select(-UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Lat, -Long_, -Combined_Key) %>%
    pivot_longer(-(1:3), names_to="date", values_to=columnName) %>%
    mutate(date=as.Date(date, format="%m/%d/%y")) %>%
    rename(country = Country_Region, state = Province_State, pop = Population)
  data = data %>% group_by(state, country, date) %>% summarise(deaths=sum(deaths), pop=sum(pop))
  save(data, file=fileName)
  return(data)
}

covid19_confirm <- loadDataUS_cases("time_series_covid19_confirmed_US.csv", "confirmed")
covid19_deaths <- loadDataUS_deaths("time_series_covid19_deaths_US.csv", "deaths")
covid19 <- left_join(covid19_confirm,covid19_deaths, by=c('state','country','date'))

covid19country <- covid19 %>% mutate(state = "US") %>%
  group_by(date) %>% summarise(n = sum(confirmed, na.rm=T), d = sum(deaths, na.rm=T)) %>%
  mutate(n_new = n - lag(n, default=0),  d_new = d - lag(d, default=0)) %>%
  select(date, n, d, n_new, d_new) %>% arrange(date) %>% filter(date>="2020-03-01")





### (3) Run generalised logistic model using Stan
# Stan model
model = "
// Stan model to evaluated the cases of Covid-19
// Observational model: Poisson
// curve: Logistic (3 curves)

data {
  // observed data
  int<lower=1> n; // number of observations
  int<lower=0> y[n]; // counts of new cases
  int<lower=0> pop;
  real<lower=0,upper=1> perPop;
}

parameters {
  real b1_1;
  real b1_2;
  real b1_3;
  real<lower=0, upper=pop*perPop*exp(b1_1)> a1;
  real<lower=0, upper=pop*perPop*exp(b1_2)> a2;
  real<lower=0, upper=pop*perPop*exp(b1_3)> a3;
  real<lower=0> alpha1;
  real<lower=0> alpha2;
  real<lower=0> alpha3;
  real delta1;
  real delta2;
  real delta3;
  real<lower=0> c1;
  real<lower=0> c2;
  real<lower=0> c3;
}

transformed parameters{
  
  real<lower=0> b1 = exp(b1_1);
  real<lower=0> b2 = exp(b1_2);
  real<lower=0> b3 = exp(b1_3);
  vector<lower=0>[n] mu1;
  vector<lower=0>[n] mu2;
  vector<lower=0>[n] mu3;
  vector<lower=0>[n] mu;

  for(t in 1:n){
    mu1[t] = exp(log(a1)+log(c1)-(c1*t)-2*log( b1+exp(-c1*t) ));
    mu2[t] = exp(log(a2)+log(c2)-(c2*t)-2*log( b2+exp(-c2*t) ));
    mu3[t] = exp(log(a3)+log(c3)-(c3*t)-2*log( b3+exp(-c3*t) ));
    mu[t] = normal_cdf(alpha1*(t-delta1),0,1) * mu1[t]
          + normal_cdf(alpha2*(t-delta2),0,1) * mu2[t]
          + normal_cdf(alpha3*(t-delta3),0,1) * mu3[t];
  }
}

model {
  // observational model
  y ~ poisson(mu); // observed model
  // prior distributions
  a1 ~ gamma(0.1, 0.1);
  a2 ~ gamma(0.1, 0.1);
  a3 ~ gamma(0.1, 0.1);
  c1 ~ gamma(2,9);
  c2 ~ gamma(2,9);
  c3 ~ gamma(2,9);
  alpha1 ~ gamma(0.01,0.01);   // shape, scale
  alpha2 ~ gamma(0.01,0.01);   // shape, scale
  alpha3 ~ gamma(0.01,0.01);   // shape, scale
  delta1 ~ normal(0,100);
  delta2 ~ normal(0,100);
  delta3 ~ normal(0,100);
  b1_1 ~ normal(0, sqrt(20));  // sqrt(1/0.1)
  b1_2 ~ normal(0, sqrt(20));  // sqrt(1/0.1)
  b1_3 ~ normal(0, sqrt(20));  // sqrt(1/0.1)
}"


# Compile Stan model
mod <- rstan::stan_model(model_code = model,verbose=F)


# Data configurations
Y <- covid19country
while(any(Y$n_new <0)){
  pos <- which(Y$n_new <0)
  for(j in pos){
    Y$n_new[j-1] = Y$n_new[j] + Y$n_new[j-1]
    Y$n_new[j] = 0
    Y$n[j-1] = Y$n[j]
  }
}

pop <- sum((covid19 %>% dplyr::distinct(state,pop))$pop)

i = 4 # (2: confirmed, 3: deaths, 4: new confirmed, 5: new deaths)
L = 300
Yobs <- Y %>% dplyr::select(date,names(Y)[i],names(Y)[i-2])
Y <- Y %>% filter(date<=t)
t = dim(Y)[1]


# Stan configurations
params = c("a1","b1","c1","alpha1","delta1", "a2","b2","c2","alpha2","delta2","a3","b3","c3","alpha3","delta3","mu")
burn_in = 1e4
lag = 5
sample_size = 1e3
number_iterations= burn_in + lag*sample_size
number_chains = 1
pp = 0.08
P = pop*pp


# Run Stan
data_stan = list(y=Y[[i]], n=t, L=L, pop=pop, perPop=pp)
ctrl_mt=20;
ctrl_ad=0.95
mod_sim <- rstan::sampling(object = mod, data = data_stan,
                           pars = params,
                           chains = number_chains,
                           iter = number_iterations, warmup = burn_in, thin = lag, 
                           control = list(max_treedepth = ctrl_mt, adapt_delta=ctrl_ad),
                           verbose = FALSE,
                           open_progress = FALSE,
                           show_messages = TRUE)

# Stan results
mod_chain = as.data.frame(mod_sim)

a1_pos = "a1"
b1_pos = "b1"
c1_pos = "c1"
alpha1_pos = "alpha1"
delta1_pos = "delta1"
a2_pos = "a2"
b2_pos = "b2"
c2_pos = "c2"
alpha2_pos = "alpha2"
delta2_pos = "delta2"
a3_pos = "a3"
b3_pos = "b3"
c3_pos = "c3"
alpha3_pos = "alpha3"
delta3_pos = "delta3"
mu_pos = paste0("mu[",1:t,"]")

# "predL3_azzalini" function: generate a sample from the predictive distribution
predL3_azzalini <- function(L=100,B,pop,casos,
                            a1,b1,c1,alpha1,delta1,
                            a2,b2,c2,alpha2,delta2,
                            a3,b3,c3,alpha3,delta3){
  M <- length(a1)
  Max <- 2*pop
  y.fut <- mu <- mu1 <- mu2 <- mu3 <- matrix(-Inf, ncol=L,nrow=M)

  for(i in 1:L){
    mu1[,i] <- exp(log(a1)+log(c1)-(c1*(B+i))-2*log( b1+exp(-c1*(B+i)) ))
    mu2[,i] <- exp(log(a2)+log(c2)-(c2*(B+i))-2*log( b2+exp(-c2*(B+i)) ))
    mu3[,i] <- exp(log(a3)+log(c3)-(c3*(B+i))-2*log( b3+exp(-c3*(B+i)) ))
    mu[,i] <-  pnorm(alpha1*((B+i)-delta1),0,1) * mu1[,i] +
      pnorm(alpha2*((B+i)-delta2),0,1) * mu2[,i] +
      pnorm(alpha3*((B+i)-delta3),0,1) * mu3[,i];
    y.fut[,i] <- rpois(M,mu[,i])
    pos <- which(is.na(y.fut[,i]))
    if(length(pos) > 0) y.fut[pos,i] <- Max
  }
  pos <- which(rowSums(mu)+casos > pop)
  if(length(pos) > 0 && length(pos) < 0.05*M) out <- list(y.fut = y.fut[-pos,], mu.fut = mu[-pos,], pos=pos)
  else out <- list(y.fut = y.fut, mu.fut = mu, pos=NULL)
  return(out)
}

fut <- predL3_azzalini(L=L,t,pop*pp,Y[[i-2]][t],
                       c(mod_chain[[a1_pos]]),c(mod_chain[[b1_pos]]),c(mod_chain[[c1_pos]]),c(mod_chain[[alpha1_pos]]),c(mod_chain[[delta1_pos]]),
                       c(mod_chain[[a2_pos]]),c(mod_chain[[b2_pos]]),c(mod_chain[[c2_pos]]),c(mod_chain[[alpha2_pos]]),c(mod_chain[[delta2_pos]]),
                       c(mod_chain[[a3_pos]]),c(mod_chain[[b3_pos]]),c(mod_chain[[c3_pos]]),c(mod_chain[[alpha3_pos]]),c(mod_chain[[delta3_pos]]))
mod_chain_y = fut$y.fut
mod_chain_cumy = rowCumsums(mod_chain_y) + Y[[i-2]][t]


### list output
lt_predict <- NULL
L0 = 300

# Quantil curves
if(Y[[i-2]][t] > 1000){
  lowquant <- colQuantiles(mod_chain_y[,1:L0], prob=.025)
  medquant <- colQuantiles(mod_chain_y[,1:L0], prob=.5)
  highquant <- colQuantiles(mod_chain_y[,1:L0], prob=.975)
} else{
  lowquant <- c(Y[[i-2]][t],colQuantiles(mod_chain_cumy[,1:L0], prob=.025))
  lowquant <- (lowquant-lag(lowquant,default=0))[-1]
  medquant <- c(Y[[i-2]][t],colQuantiles(mod_chain_cumy[,1:L0], prob=.5))
  medquant <- (medquant-lag(medquant,default=0))[-1]
  highquant <- c(Y[[i-2]][t],colQuantiles(mod_chain_cumy[,1:L0], prob=.975))
  highquant <- (highquant-lag(highquant,default=0))[-1]
}

dat.vec <- as.Date((max(Y$date)+1):(max(Y$date)+L0), origin="1970-01-01")
dat.full <- c(Y[[1]],dat.vec)

if (length(fut$pos) > 0) {
  mod_chain_mu <- cbind(as.matrix(mod_chain[mu_pos])[-fut$pos,],fut$mu.fut)
} else {
  mod_chain_mu <- cbind(as.matrix(mod_chain[mu_pos]),fut$mu.fut)
}

mu50 <- apply(mod_chain_mu,2,quantile, probs=0.5)

lt_predict <- data.frame( date = dat.vec,
                          q25  = lowquant,
                          med  = medquant,
                          q975 = highquant,
                          m    = colMeans(mod_chain_y[,1:L0]))     
row.names(lt_predict) <- NULL

muplot <- data.frame(date = dat.full, mu50 = mu50[1:(t+L0)])
list_out <- list(lt_predict=lt_predict, mu_plot=muplot, Yobs = Yobs, yfut = mod_chain_y)

# At this moment, save "list_out" object as a "rds" file using "saveRDS" function.
# saveRDS(list_out, file=...)







