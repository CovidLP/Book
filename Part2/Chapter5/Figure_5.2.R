rm(list=ls())



### packages
library(dplyr)
library(ggplot2)
library(reshape2)
library(data.table)
library(matrixStats)
library(rstan)




### Dates and states
t0 = as.Date("2020-02-01")
t = as.Date("2020-07-31")
t1 = as.Date("2021-09-30")
uf = data.frame(state=c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA",
                        "PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO"))



### Next contents
#
# (1) Generate Figure 5.2 from the "rds" files available in the Github path "Part2/Chapter5/data/"
# (2) Download Brazilian data from John Hopkins repository
# (3) Run generalised logistic model using Stan and generate the "lt_predict" object used in Figure 5.2
#
# The codes in (2) and (3) generate "rds" files with objects to generate Figure 5.2. These "rds" files
# are reduced when compared to the ones in "Part2/Chapter5/data/", but have enough information to
# generate Figure 5.2 with the codes in (1). 




### (1) Generate Figure 5.2

# "rds" files
# data_rds <- paste0("C:/_covid_T4/",t)
# data_BR <- readRDS(paste0(data_rds,"/Brazil_d.rds"))
data_rds <- paste0("Part2/Chapter5/data/")
data_BR <- readRDS(paste0(data_rds,"Brazil_d.rds"))



# Data objects
med_UF  <- data.frame(date = seq(t0,t1,by="day"))
q25_UF  <- data.frame(date = seq(t0,t1,by="day"))
q975_UF <- data.frame(date = seq(t0,t1,by="day"))
mu_UF   <- data.frame(date = seq(t0,t1,by="day"))
obs_UF  <- data.frame(date = seq(t0,t,by="day"))
mu_obs   <- data.frame(date = seq(t0,t,by="day"))



for (u in uf$state) {
  
  # rds import
  data_uf <- readRDS(paste0(data_rds,"/Brazil_",u,"_de.rds"))
  
  # create yfut_UF
  dates <- as.Date((t+1):max(data_uf$lt_predict$date), origin="1970-01-01")
  yfut_UF0 <- data.frame(date = dates, UF = u)
  if (u == uf$state[1]) {
    yfut_UF <- yfut_UF0 %>% bind_cols(as_tibble(t(data_uf$yfut)))
  } else {
    yfut_UF <- yfut_UF %>% bind_rows(yfut_UF0 %>% bind_cols(as_tibble(t(data_uf$yfut))))
  }
  
  # create country/state column
  eval(parse(text=paste0("med_UF$`",u,"`  = NA_real_")))
  eval(parse(text=paste0("q25_UF$`",u,"`  = NA_real_")))
  eval(parse(text=paste0("q975_UF$`",u,"` = NA_real_")))
  eval(parse(text=paste0("mu_UF$`",u,"`   = NA_real_")))
  eval(parse(text=paste0("obs_UF$`",u,"`  = NA_real_")))
  eval(parse(text=paste0("mu_obs$`",u,"`  = NA_real_")))
  
  # insert med
  med_UF[[u]][which(med_UF$date %in% data_uf$lt_predict$date)] <- data_uf$lt_predict$med
  
  # insert percentiles
  q25_UF[[u]][which(q25_UF$date %in% data_uf$lt_predict$date)]   <- data_uf$lt_predict$q25
  q975_UF[[u]][which(q975_UF$date %in% data_uf$lt_predict$date)] <- data_uf$lt_predict$q975
  
  # insert mu
  mu_UF[[u]][which(mu_UF$date %in% data_uf$lt_predict$date)] <- data_uf$lt_predict$m
  mu_obs[[u]][which(mu_obs$date %in% data_uf$mu_plot$date)] <- (data_uf$mu_plot %>% filter(date>=t0,date<=t) %>% select(mu50))$mu50
  
  # insert obs
  obs_filter <- data_uf$Yobs %>% filter(date<=t)
  obs_UF[[u]][which(obs_UF$date %in% obs_filter$date)] <- obs_filter$d_new
}

yfut_UF_df <- data.table(yfut_UF %>% dplyr::select(-UF))
yfut_BR <- yfut_UF_df[, lapply(.SD, sum, na.rm=T), by=date]
mu_obs_sum <- data.frame(date = mu_obs$date, mu_sum = rowSums(as.matrix(mu_obs[,-1]),na.rm = T))

obs_filter <- data_BR$Yobs %>% filter(date<=t)
obs_UF[["BR"]][which(obs_UF$date %in% obs_filter$date)] <- obs_filter$d_new

med_UF$BR = NA_real_
med_UF$BRq25 = NA_real_
med_UF$BRq975 = NA_real_
med_UF[["BR"]][which(med_UF$date %in% data_BR$lt_predict$date)] <- data_BR$lt_predict$med
med_UF[["BRq25"]][which(med_UF$date %in% data_BR$lt_predict$date)] <- data_BR$lt_predict$q25
med_UF[["BRq975"]][which(med_UF$date %in% data_BR$lt_predict$date)] <- data_BR$lt_predict$q975

mu_UF$BR = NA_real_
mu_UF[["BR"]][which(mu_UF$date %in% data_BR$lt_predict$date)] <- data_BR$lt_predict$m

mu_obs_sum$BR = NA_real_
mu_obs_sum[["BR"]][which(mu_obs$date %in% data_BR$mu_plot$date)] <- (data_BR$mu_plot %>% filter(date>=t0,date<=t) %>% select(mu50))$mu50

BRsum <- data.frame(date = yfut_BR$date,
                    q25  = rowQuantiles(as.matrix(yfut_BR[,-1]), prob=.025, na.rm=T),
                    q975 = rowQuantiles(as.matrix(yfut_BR[,-1]), prob=.975, na.rm=T),
                    med  = rowQuantiles(as.matrix(yfut_BR[,-1]), prob=.5, na.rm=T),
                    m   = rowMeans(as.matrix(yfut_BR[,-1]), na.rm=T))


# Graphic objects
plot_obs_UF <- reshape2::melt(obs_UF %>% dplyr::select(-BR), id.vars='date', variable.name='series')
plot_obs_BR <- reshape2::melt(obs_UF %>% dplyr::select(date,BR), id.vars='date', variable.name='series')

plot_med_UF    <- reshape2::melt(med_UF %>% dplyr::select(-BR,-BRq25,-BRq975) %>% filter(date>t), id.vars='date', variable.name='series')
plot_med_BRsum <- reshape2::melt(BRsum  %>% dplyr::select(date,med)           %>% filter(date>t), id.vars='date', variable.name='series')
plot_med_BR    <- reshape2::melt(med_UF %>% dplyr::select(date,BR)            %>% filter(date>t), id.vars='date', variable.name='series')
plot_med_BRq   <- reshape2::melt(med_UF %>% dplyr::select(date,BRq25,BRq975)  %>% filter(date>t), id.vars='date', variable.name='series')

plot_mu_UF    <- reshape2::melt(mu_UF %>% dplyr::select(-BR)     %>% filter(date>t), id.vars='date', variable.name='series')
plot_mu_BRsum <- reshape2::melt(BRsum %>% dplyr::select(date,m)  %>% filter(date>t), id.vars='date', variable.name='series')
plot_mu_BR    <- reshape2::melt(mu_UF %>% dplyr::select(date,BR) %>% filter(date>t), id.vars='date', variable.name='series')




### Figure 5.2
Sys.setlocale("LC_TIME", "C")
size_plot = 10

ggplot() + ylab("New deaths per day") + xlab("") +  ggtitle("Brazilian states") +
  
  labs(x="Date", y="New deaths per day") + 
  
  # scale_x_date(breaks = seq(t0, t, by = 21), date_labels = "%b/%d/%Y") +
  scale_x_date(breaks = seq(t0+29,t+180,by=21), date_labels="%d/%b/%Y", limits = c(t0+29,t+180)) +
  scale_y_continuous(breaks = seq(0, 2e3, by = 5e2)) +
  theme_bw() +
  
  # Credibile intervals
  geom_ribbon(aes(x=date,ymin=BRq25,ymax=BRq975), data=med_UF, alpha=.8, colour = NA, fill='gray50') +
  geom_ribbon(aes(x=date,ymin=q25,ymax=q975), data=BRsum, alpha=.4, colour = NA, fill='gray50') +
  
  # Brazil
  geom_line(aes(x=date,y=value,group=series), data=plot_med_BR, linetype="dashed", color="black", size=1.1) +
  geom_line(aes(x=date,y=BR), data=mu_obs_sum %>% filter(date>=min(data_uf$mu_plot$date)), linetype="dashed", color="black", size=1.1) +
  
  # Brazil (states sum)
  geom_line(aes(x=date,y=value,group=series), data=plot_med_BRsum, linetype="solid", color="black", size=1.1) +
  geom_line(aes(x=date,y=mu_sum), data=mu_obs_sum %>% filter(date>=min(data_uf$mu_plot$date)), linetype="solid", color="black", size=1.1) +
  
  # Brazil (data)
  scale_color_manual(values="gray20") +
  geom_line(aes(x=date,y=value,group=series), data=plot_obs_BR %>% filter(date<=t), linetype="solid", color="gray50", size=0.5) +
  geom_point(aes(x=date,y=value,group=series), data=plot_obs_BR %>% filter(date<=t), color="gray50", size=1) +
  geom_vline(xintercept=t, linetype="solid", size=0.2) +
  
  theme(axis.title.y = element_text(size = size_plot),
        axis.text.y = element_text(size = size_plot),
        axis.text.x = element_text(angle = 90, size = size_plot),
        axis.title.x = element_text(size = size_plot),
        strip.text = element_text(size = size_plot),
        plot.title = element_text(hjust = 0.5, size = size_plot))







### (2) Download Brazilian data

baseURLbr = "https://raw.githubusercontent.com/covid19br/covid19br.github.io/master/dados"

covid19uf <- read.csv(file.path(baseURLbr,"EstadosCov19.csv"), check.names=FALSE, stringsAsFactors=FALSE) %>%
  rename(state = estado,
         date = data,
         n = casos.acumulados,
         d = obitos.acumulados,
         n_new = novos.casos,
         d_new = obitos.novos) %>%
  mutate(date = as.Date(date)) %>%
  dplyr::select(date, -n, -d, n_new, d_new, state) %>%
  na.omit() %>%
  arrange(state,date) %>% filter(date>='2020-01-23',date<=t)

covid19br <- read.csv(file.path(baseURLbr,"BrasilCov19.csv"), check.names=FALSE, stringsAsFactors=FALSE) %>%
  mutate(state = 'BR') %>%
  rename(date = data,
         n = casos.acumulados,
         d = obitos.acumulados,
         n_new = novos.casos,
         d_new = obitos.novos) %>%
  mutate(date = as.Date(date)) %>%
  dplyr::select(date, -n, -d, n_new, d_new, state) %>%
  na.omit() %>%
  arrange(date) %>% filter(date>='2020-01-23',date<=t)

covid19 <- bind_rows(covid19uf,covid19br)

br_pop = read.csv("Part2/Chapter5/data/pop_BR.csv")





### (3) Run generalised logistic model using Stan

# Stan model
model = "
// Stan model to evaluated the cases of Covid-19
// Observational model: Poisson
// curve: Generalized Logistic

data {
  // observed data
  int<lower=1> n; // number of observations
  int<lower=0> y[n]; // counts of new cases
  int<lower=1> L;     // number of predictions
  real pop;
  real<lower=0,upper=1> perPop;
}

parameters { 
  real<lower=1> f;
  real<lower=-30> b1;
  real<lower=0, upper=perPop*pop*exp(f*b1)> a; 
  real<lower=0> c;
}

transformed parameters{
  real<lower=0> b;
  real<lower=0, upper=pop> mu[n];
  b = exp(b1);
  for(t in 1:n){
    mu[t] = exp(log(f)+log(a)+log(c)-(c*t)-(f+1)*log( b+exp(-c*t) ) );
  }
}

model {
  // observational model
  y ~ poisson(mu);
  // prior distributions
  a ~ gamma(0.1, 0.1);
  c ~ gamma(2,9);
  f ~ gamma(0.01,0.01);   // shape, scale 
  b1 ~ normal(0, sqrt(20));  // sqrt(1/0.1)
}"

# Compile Stan model
mod <- rstan::stan_model(model_code = model,verbose=F)


# Run stan and compute the results for each Brazilian state
for (s in 1:dim(uf)[1]) { # BEGIN FOR
  
  # State data
  Y <- covid19 %>% filter(state==uf$state[s]) %>%
    mutate(n = cumsum(n_new),
           d = cumsum(d_new)) %>%
    dplyr::select(date, n, d, n_new, d_new, state) %>%
    arrange(date) %>% filter(date>='2020-02-01')
  
  while(any(Y$d_new <0)){
    pos <- which(Y$d_new <0)
    for(j in pos){
      Y$d_new[j-1] = Y$d_new[j] + Y$d_new[j-1]
      Y$d_new[j] = 0
      Y$d[j-1] = Y$d[j]
    }
  }
  
  pop <- br_pop$pop[which(br_pop$uf == uf$state[s])]
  
  i = 5 # (2: confirmed, 3: deaths, 4: new confirmed, 5: new deaths)
  L = 300
  Yobs <- Y %>% dplyr::select(date,names(Y)[i],names(Y)[i-2],state)
  Y <- Y %>% filter(date<=t)
  t = dim(Y)[1]

  
  # Stan configurations
  params = c("a","b","c","f","mu")
  burn_in = 10e3 #5e3 #4e3 #2e3
  lag = 5 #3 
  sample_size = 1e3
  number_iterations = burn_in + lag*sample_size
  number_chains = 1 #3
  pp = 0.08
  P = pop*0.25*pp
  data_stan = list(y=Y[[i]], n=t, L=L, pop=pop*0.25, perPop=pp)
  
  # Stan initial values 
  a0 = 100; b0 = 1; c0 = 0.5; f0 = 1.01
  init <- list(a = a0, b1 = log(b0), c = c0, f = f0)
  
  # Run Stan
  mod_sim <- rstan::sampling(object = mod, data = data_stan,
                             pars = params,
                             chains = number_chains,
                             init = init,
                             iter = number_iterations, warmup = burn_in, thin = lag, 
                             control = list(max_treedepth = 50, adapt_delta=0.999),
                             verbose = FALSE, open_progress=FALSE, show_messages=FALSE)
  
  
  # Stan results
  mod_chain = as.data.frame(mod_sim)


  # Predict
  a_pos = "a"
  b_pos = "b"
  c_pos = "c"
  f_pos = "f"
  mu_pos = paste0("mu[",1:t,"]")
  
  # "predL" function: generate a sample from the predictive distribution
  predL <- function(L=100,B,pop,casos,a,b,c,f){
    M <- length(a)
    Max <- 2*pop
    y.fut <- mu <- matrix(-Inf, ncol=L,nrow=M)
    
    for(i in 1:L){
      mu[,i] <-  exp(log(f)+log(a)+log(c)-(c*(B+i))-(f+1)*log(b+exp(-c*(B+i)) ) )
      y.fut[,i] <- rpois(M,mu[,i])
      pos <- which(is.na(y.fut[,i]))
      if(length(pos) > 0) y.fut[pos,i] <- Max
    }
    pos <- which(rowSums(mu)+casos > pop)
    if(length(pos) > 0 && length(pos) < 0.05*M) out <- list(y.fut = y.fut[-pos,], mu.fut = mu[-pos,], pos=pos)
    else out <- list(y.fut = y.fut, mu.fut = mu, pos=NULL)
    return(out)
  }
  
  fut <- predL(L=L,t,pop*pp*0.25,Y[[i-2]][t],c(mod_chain[[a_pos]]),c(mod_chain[[b_pos]]),c(mod_chain[[c_pos]]),c(mod_chain[[f_pos]]))
  mod_chain_y = fut$y.fut
  mod_chain_cumy = rowCumsums(mod_chain_y) + Y[[i-2]][t]

  
  # "list_out" object
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

  lt_predict <- data.frame( date = dat.vec, m = colMeans(mod_chain_y[,1:L0]))
  row.names(lt_predict) <- NULL
  muplot <- data.frame(date = dat.full, mu50 = mu50[1:(t+L0)])
  
  list_out <- list(lt_predict=lt_predict, mu_plot=muplot, Yobs = Yobs, yfut = mod_chain_y)
  
  # At this moment, save "list_out" object as a "rds" file using "saveRDS" function.
  # saveRDS(list_out, file=...)
  
}






