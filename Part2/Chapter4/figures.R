library(PandemicLP)
library(dplyr)
library(ggplot2)
Sys.setlocale("LC_TIME", "C") # dates in english
height = 18;width = 35
th = theme(axis.title.y = element_text(size=90),
           axis.text.y = element_text(size=87),
           axis.text.x = element_text(size=90,angle=90),
           axis.title.x = element_text(size=90))
muFun = function(t,a,b,c,f=1) exp(log(f)+log(a)+log(c)-(c*t)-(f+1)*log( b+exp(-c*t) ) )
negbin1 = stan_model(model_code = "
// Stan model to evaluated the cases of Covid-19 - Negative Binomial model
// data mean: generalised static logistic


data {
  
  //-----------------------------
  // observed data
  int<lower=1> n; // number of observations
  int<lower=0> y[n]; // counts of new cases
  int<lower=1> pop;
  real<lower=0, upper=1> perPop;
  real<lower=0> phiTrunc;
  //-----------------------------
}


parameters { 
  real b1;
  real<lower=1> f;
  real<lower=0, upper=perPop*pop*exp(f*b1)> a;
  vector<lower=0>[n] lambda;
  real<lower=phiTrunc> phi;

  real<lower=0> c;
}

transformed parameters{
  
  real<lower=0> b;
  vector<lower=0, upper=pop>[n] mu;

  b = exp(b1);

  for(t in 1:n){
    mu[t] = exp(log(f)+log(a)+log(c)-(c*t)-(f+1)*log( b+exp(-c*t) ));
    //mu[t] = f*a*c*exp(-c*t)/ (b+exp(-c*t))^(f+1);
  }
  
}


model {
  //----------------------------
  // likelihood function
    y ~ poisson(lambda); // observed model
    lambda ~ gamma(mu * phi,phi);
  //----------------------
   // prior distributions
   a ~ gamma(0.1, 0.1);
   c ~ gamma(2,9);
   f ~ gamma(0.1,0.1);
   phi ~ gamma(0.1,0.1);
  b1 ~ normal(0, sqrt(20));
}
")
negbin2 = stan_model(model_code = "
// Stan model to evaluated the cases of Covid-19 - Negative binomial model
// data mean: generalised static logistic


data {
  
  //-----------------------------
  // observed data
  int<lower=1> n; // number of observations
  int<lower=0> y[n]; // counts of new cases
  int<lower=1> pop;
  real<lower=0, upper=1> perPop;
  real<lower=0> phiTrunc;
  //-----------------------------
}


parameters { 
  real b1;
  real<lower=1> f;
  real<lower=0, upper=perPop*pop*exp(f*b1)> a;
  vector<lower=0>[n] lambda;
  real<lower=phiTrunc> phi;

  real<lower=0> c;
}

transformed parameters{
  
  real<lower=0> b;
  vector<lower=0, upper=pop>[n] mu;

  b = exp(b1);

  for(t in 1:n){
    mu[t] = exp(log(f)+log(a)+log(c)-(c*t)-(f+1)*log( b+exp(-c*t) ));
    //mu[t] = f*a*c*exp(-c*t)/ (b+exp(-c*t))^(f+1);
  }
  
}


model {
  //----------------------------
  // likelihood function
    y ~ poisson(lambda); // observed model
    lambda ~ gamma(phi,phi ./ mu);
  //----------------------
   // prior distributions
   a ~ gamma(0.1, 0.1);
   c ~ gamma(2,9);
   f ~ gamma(0.1,0.1);
   phi ~ gamma(0.1,0.1);
  b1 ~ normal(0, sqrt(20)); 
}
")


#### Figure 4.1 ####
ni_4.1 = load_covid("Nigeria",last_date = "2020-10-15")
# ni_4.1 = readRDS("data/401_ni.rds") # Run this to load the data from a file

ni_est = pandemic_model(ni_4.1,covidLPconfig = TRUE,seed=123) # Run this to fit the model
# ni_est = readRDS("results/401_ni.rds") # Run this to load a pre-executed fit

pars = as.data.frame(ni_est$fit) %>% select(a,b,c,f) %>% colMeans
dd = ni$data$date
mus = muFun(1:length(dd),pars[1],pars[2],pars[3],pars[4])
yy = rpois(1:length(dd),mus)
maxy = max(yy,ni$data$new_cases)
g = data.frame(date = dd, real = ni$data$new_cases, mu = mus) %>%
  ggplot(aes(date))+
  geom_line(aes(y=real),colour="darkgrey",size=1)+
  geom_line(aes(y=mu),colour="black",size=1)+theme_bw()+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "4 weeks")+#th+
  ylab("New cases per day")+ylim(0,maxy)
g
# ggsave("tempExampleNigeria.pdf",plot=g,device="pdf",
#        height = height,
#        width = width)

g = data.frame(date = dd, real = ni$data$new_cases, mu = mus) %>%
  ggplot(aes(date))+
  geom_line(aes(y=real-mu),colour="black",size=1)+theme_bw()+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "4 weeks")+#th+
  ylab("Residuals")
g
# ggsave("tempExampleNigeriaResiduals.pdf",plot=g,device="pdf",
#        height = height,
#        width = width)

#### Figure 4.2 ####
br_4.2 = load_covid("Brazil",last_date = "2020-10-15")
ge_4.2 = load_covid("Germany",last_date = "2020-05-14")
ni_4.2 = load_covid("Nigeria",last_date = "2020-10-15")
# br_4.2 = readRDS("data/402_br.rds") # Run this to load the data from a file
# ge_4.2 = readRDS("data/402_ge.rds") # Run this to load the data from a file
# ni_4.2 = readRDS("data/402_ni.rds") # Run this to load the data from a file

br_est = pandemic_model(br_4.2,covidLPconfig = TRUE,seed=1) # 123 gets stuck # Run this to fit the model
ge_est = pandemic_model(ge_4.2,covidLPconfig = TRUE,seed=123) # Run this to fit the model
ni_est = pandemic_model(ni_4.2,covidLPconfig = TRUE,seed=123) # Run this to fit the model
# br_est = readRDS("results/402_br.rds") # Run this to load a pre-executed fit
# ge_est = readRDS("results/402_ge.rds") # Run this to load a pre-executed fit
# ni_est = readRDS("results/402_ni.rds") # Run this to load a pre-executed fit

# NI
set.seed(123)
pars = as.data.frame(ni_est$fit) %>% select(a,b,c,f) %>% colMeans
dd = ni_4.2$data$date
yy = rpois(1:length(dd),muFun(1:length(dd),pars[1],pars[2],pars[3],pars[4]))
maxy = max(yy,ni_4.2$data$new_cases)
g = data.frame(date = dd, newC = yy) %>%
  ggplot(aes(date,newC))+
  geom_line(colour="black",size=1)+theme_bw()+
  scale_x_date(limits = ,date_labels = "%d/%b/%y",date_breaks = "4 weeks")+#th+
  ylab("New cases per day")+ylim(0,maxy)
g
# ggsave("poissonExample.pdf",plot=g,device="pdf",
#        height = height,
#        width = width)

g = ni_4.2$data %>%
  ggplot(aes(date,new_cases))+
  geom_line(colour="black",size=1)+theme_bw()+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "4 weeks")+#th+
  ylab("New cases per day")+ylim(0,maxy)
g
# ggsave("Nigeria.pdf",plot=g,device="pdf",height = height,
#        width = width)

# BR
set.seed(123)
pars = as.data.frame(br_est$fit) %>% select(a,b,c,f) %>% colMeans
dd = br_4.2$data$date
yy = rpois(1:length(dd),muFun(1:length(dd),pars[1],pars[2],pars[3],pars[4]))
maxy = max(yy,br_4.2$data$new_cases)
g = data.frame(date = dd, newC = yy) %>%
    ggplot(aes(date,newC))+
    geom_line(colour="black",size=1)+theme_bw()+
    scale_x_date(date_labels = "%d/%b/%y",date_breaks = "3 weeks")+#th+
    ylab("New cases per day")+ylim(0,maxy)
g
# ggsave("brazilFitted.pdf",plot=g,device="pdf",height = height,
#        width = width)

g = br_4.2$data %>%
  ggplot(aes(date,new_cases))+
  geom_line(colour="black",size=1)+theme_bw()+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "3 weeks")+#th+
  ylab("New cases per day")+ylim(0,maxy)
g
# ggsave("brazilExample.pdf",plot=g,device="pdf",height = height,
#        width = width)

# GE
set.seed(123)
pars = as.data.frame(ge_est$fit) %>% select(a,b,c,f) %>% colMeans
dd = ge_4.2$data$date
yy = rpois(1:length(dd),muFun(1:length(dd),pars[1],pars[2],pars[3],pars[4]))
maxy = max(yy,ge_4.2$data$new_cases)
g = data.frame(date = dd, newC = yy) %>%
  ggplot(aes(date,newC))+
  geom_line(colour="black",size=1)+theme_bw()+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "4 weeks")+#th+
  ylab("New cases per day")+ylim(0,maxy)
g
# ggsave("germanyFitted.pdf",plot=g,device="pdf",height = height,
#        width = width)
#ggsave("../germanyFitted.pdf",plot=g,device="pdf",height = height,width = width)

g = ge_4.2$data %>%
  ggplot(aes(date,new_cases))+
  geom_line(colour="black",size=1)+theme_bw()+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "4 weeks")+#th+
  ylab("New cases per day")+ylim(0,maxy)
g
# ggsave("germany.pdf",plot=g,device="pdf",height = height,
#        width = width)
#ggsave("../germany.pdf",plot=g,device="pdf",height = height,width = width)

#### Figures 4.3 and 4.4 ####
ge_4.3 = load_covid("Germany",last_date = "2020-05-14")
# ge_4.3 = readRDS("data/403_ge.rds") # Run this to load the data from a file

g = ge_4.3$data %>%
  ggplot(aes(date,new_cases))+
  geom_line(colour="black",size=1)+theme_bw()+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "4 weeks")+#th+
  ylab("New cases per day")
g
# ggsave("germany.pdf",plot=g,device="pdf",height = height,
#        width = width)

data_stan = list(y = ge_4.3$data$new_cases,n = nrow(ge_4.3$data), pop = ge_4.3$population, perPop = 0.08, phiTrunc = 0)
params = c("a","b","c","f","phi","mu")
initialPar = 'random'
nchain = 1; burn_in = 7e3; lag = 3; sSize = 1000; iter = burn_in + lag*sSize
ge_nb1<- sampling(object = negbin1, data = data_stan,
                  pars = params,
                  chains = nchain,
                  init = initialPar,
                  iter = burn_in+lag*sSize,warmup = burn_in, thin = lag,
                  control = list(max_treedepth = 15, adapt_delta=0.995),
                  verbose = FALSE, open_progress=FALSE, show_messages=FALSE,seed=1234) # 123 doesn't converge
ge_nb2<- sampling(object = negbin2, data = data_stan,
                  pars = params,
                  chains = nchain,
                  init = initialPar,
                  iter = burn_in+lag*sSize,warmup = burn_in, thin = lag,
                  control = list(max_treedepth = 15, adapt_delta=0.995),
                  verbose = FALSE, open_progress=FALSE, show_messages=FALSE,seed=1234)
# ge_nb1 = readRDS("results/402_ge_nb1.rds") # Run this to load a pre-executed fit
# ge_nb2 = readRDS("results/402_ge_nb2.rds") # Run this to load a pre-executed fit

# 4.3
set.seed(123)
pars = as.data.frame(ge_nb1) %>% select(a,b,c,f,phi) %>% colMeans
dd = ge_4.3$data$date
maxy = max(ge_4.3$data$new_cases)
for (i in 1:4){
  yy = rpois(1:length(dd),rgamma(1:length(dd),muFun(1:length(dd),pars[1],pars[2],pars[3],pars[4])*pars[5],pars[5]))
  maxy = max(maxy,yy)
}
set.seed(123)
for (i in 1:4){
  yy = rpois(1:length(dd),rgamma(1:length(dd),muFun(1:length(dd),pars[1],pars[2],pars[3],pars[4])*pars[5],pars[5]))
  g = data.frame(date = dd, newC = yy) %>%
    ggplot(aes(date,newC))+
    geom_line()+theme_bw()+
    scale_x_date(date_labels = "%d/%b/%y",date_breaks = "4 weeks")+#th+
    ylab("New cases per day")+ylim(0,maxy)
  print(g)
  #ggsave(paste0("germanyFitted_nb1_",i,".pdf"),plot=g,device="pdf",width=width,height=height)
}
g = ge_4.3$data %>%
  ggplot(aes(date,new_cases))+
  geom_line(colour="black",size=1)+theme_bw()+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "4 weeks")+#th+
  ylab("New cases per day")+ylim(0,maxy)
g
# ggsave("germanyG.pdf",plot=g,device="pdf",height = height,
#        width = width)

# 4.4
set.seed(123)
pars = as.data.frame(ge_nb2) %>% select(a,b,c,f,phi) %>% colMeans
dd = ge_4.3$data$date
maxy = max(ge_4.3$data$new_cases)
for (i in 1:4){
  yy = rpois(1:length(dd),rgamma(1:length(dd),pars[5],pars[5]/muFun(1:length(dd),pars[1],pars[2],pars[3],pars[4])))
  maxy = max(maxy,yy)
}
set.seed(123)
for (i in 1:4){
  yy = rpois(1:length(dd),rgamma(1:length(dd),pars[5],pars[5]/muFun(1:length(dd),pars[1],pars[2],pars[3],pars[4])))
  g = data.frame(date = dd, newC = yy) %>%
    ggplot(aes(date,newC))+
    geom_line()+theme_bw()+
    scale_x_date(date_labels = "%d/%b/%y",date_breaks = "4 weeks")+#th+
    ylab("New cases per day")+ylim(0,maxy)
  print(g)
  # ggsave(paste0("germanyFitted_nb2_",i,".pdf"),plot=g,device="pdf",width=width,height=height)
}
g = ge_4.3$data %>%
  ggplot(aes(date,new_cases))+
  geom_line(colour="black",size=1)+theme_bw()+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "4 weeks")+#th+
  ylab("New cases per day")+ylim(0,maxy)
g
# ggsave("germanyD.pdf",plot=g,device="pdf",height = height,
#        width = width)

#### Figure 4.5 ####
# Argentina
arg_4.5 = load_covid("Argentina",last_date = "2020-10-17")
# arg_4.5 = readRDS("data/405_arg.rds") # Run this to load the data from a file

g = arg_4.5$data %>%
  ggplot(aes(date,new_cases))+
  geom_line(colour="black",size=1)+theme_bw()+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "4 weeks")+#th+
  ylab("New cases per day")
g
# ggsave("argentina.pdf",plot=g,device="pdf",height = height,
#        width = width)

# Armenia
arm_4.5 = load_covid("Armenia",last_date = "2020-08-25")
# arm_4.5 = readRDS("data/405_arm.rds") # Run this to load the data from a file

g = arm_4.5$data %>%
  ggplot(aes(date,new_cases))+
  geom_line(colour="black",size=1)+theme_bw()+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "4 weeks")+#th+
  ylab("New cases per day")
g
# ggsave("armenia.pdf",plot=g,device="pdf",height = height,
#        width = width)

# Japan
jap_4.5 = load_covid("Japan",last_date = "2020-05-04")
# jap_4.5 = readRDS("data/405_jap.rds") # Run this to load the data from a file

g = jap_4.5$data %>%
  ggplot(aes(date,new_cases))+
  geom_line(colour="black",size=1)+theme_bw()+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "4 weeks")+#th+
  ylab("New cases per day")
g
# ggsave("japan.pdf",plot=g,device="pdf",height = height,
#        width = width)

# Luxembourg
lux_4.5 = load_covid("Luxembourg",last_date = "2020-04-16")
# lux_4.5 = readRDS("data/405_lux.rds") # Run this to load the data from a file

g = lux_4.5$data %>%
  ggplot(aes(date,new_cases))+
  geom_line(colour="black",size=1)+theme_bw()+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "4 weeks")+#th+
  ylab("New cases per day")
g
# ggsave("luxembourg.pdf",plot=g,device="pdf",height = height,
#        width = width)

#### Figure 4.6 ####
it_4.6 = load_covid("Italy",last_date = "2020-03-13")
# it_4.6 = readRDS("data/406_it.rds") # Run this to load the data from a file
saveRDS(it_4.6,"../Livro/supplementary/part2/cap4/data/406_it.rds")

it_est = pandemic_model(it_4.6,covidLPconfig = TRUE)
# it_est = readRDS("results/406_it.rds") # Run this to load a pre-executed fit

data_stan = list(y = it_4.6$data$new_cases,n = nrow(it_4.6$data), pop = it_4.6$population, perPop = 0.08, phiTrunc = 0)
params = c("a","b","c","f","phi","mu")
initialPar = 'random'
nchain = 1; burn_in = 7e3; lag = 3; sSize = 1000; iter = burn_in + lag*sSize
it_nb1<- sampling(object = negbin1, data = data_stan,
                  pars = params,
                  chains = nchain,
                  init = initialPar,
                  iter = burn_in+lag*sSize,warmup = burn_in, thin = lag,
                  control = list(max_treedepth = 15, adapt_delta=0.995),
                  verbose = FALSE, open_progress=FALSE, show_messages=FALSE,seed=123)
it_nb2<- sampling(object = negbin2, data = data_stan,
                  pars = params,
                  chains = nchain,
                  init = initialPar,
                  iter = burn_in+lag*sSize,warmup = burn_in, thin = lag,
                  control = list(max_treedepth = 15, adapt_delta=0.995),
                  verbose = FALSE, open_progress=FALSE, show_messages=FALSE,seed=123)
# it_nb1 = readRDS("results/406_it_nb1.rds") # Run this to load a pre-executed fit
# it_nb2 = readRDS("results/406_it_nb2.rds") # Run this to load a pre-executed fit

d0 = it_4.6$data$date[1]-1
tFut = nrow(it_4.6$data)+1:100
muFut_poisson = sapply(tFut,function(t) muFun(t,as.data.frame(it_est$fit)$a,as.data.frame(it_est$fit)$b,as.data.frame(it_est$fit)$c,as.data.frame(it_est$fit)$f))
muFut_nb1 = sapply(tFut,function(t) muFun(t,as.data.frame(it_nb1)$a,as.data.frame(it_nb1)$b,as.data.frame(it_nb1)$c,as.data.frame(it_nb1)$f))
muFut_nb2 = sapply(tFut,function(t) muFun(t,as.data.frame(it_nb2)$a,as.data.frame(it_nb2)$b,as.data.frame(it_nb2)$c,as.data.frame(it_nb2)$f))
maxy = max(do.call(c,lapply(list(muFut_poisson,muFut_nb1,muFut_nb2),function(x) apply(x,2,quantile,0.975))))
futData = load_covid("Italy",last_date = max(it_4.6$data$date)+100)$data[-(1:nrow(it_4.6$data)),]

g = ggplot(NULL,aes(x=date))+theme_bw()+
  geom_line(aes(y=new_cases),data=it_4.6$data,colour="darkgray",size=1)+
  geom_line(aes(y=new_cases),data=futData,colour="black",size=1)+
  geom_point(aes(y=new_cases),data=it_4.6$data,size=0.3,colour="darkgray")+
  geom_point(aes(y=new_cases),data=futData,colour="black",size=0.3)+
  geom_ribbon(aes(ymin=muFut_lower,ymax=muFut_upper),
              data=data.frame(date = d0+tFut,muFut_lower = apply(muFut_poisson,2,quantile,0.025),
                              muFut_upper = apply(muFut_poisson,2,quantile,0.975)),
              fill = "gray60", alpha = 0.60)+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "3 weeks")+
  ylab("New cases per day")+ylim(0,maxy)+#th+
  geom_vline(data=data.frame(date=max(it_4.6$data$date)),aes(xintercept=date),linetype="dotted",size=0.5)
g
# ggsave("italy_poisson_0313.pdf",plot=g,device="pdf",height = height,width = width)  

g = ggplot(NULL,aes(x=date))+theme_bw()+
  geom_line(aes(y=new_cases),data=it_4.6$data,colour="darkgray",size=1)+
  geom_line(aes(y=new_cases),data=futData,colour="black",size=1)+
  geom_point(aes(y=new_cases),data=it_4.6$data,size=0.3,colour="darkgray")+
  geom_point(aes(y=new_cases),data=futData,colour="black",size=0.3)+
  geom_ribbon(aes(ymin=muFut_lower,ymax=muFut_upper),
              data=data.frame(date = d0+tFut,muFut_lower = apply(muFut_nb1,2,quantile,0.025),
                              muFut_upper = apply(muFut_nb1,2,quantile,0.975)),
              fill = "gray60", alpha = 0.60)+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "3 weeks")+
  ylab("New cases per day")+ylim(0,maxy)+#th+
  geom_vline(data=data.frame(date=max(it_4.6$data$date)),aes(xintercept=date),linetype="dotted",size=0.5)
g
#ggsave("italy_negbinG_0313.pdf",plot=g,device="pdf",height = height,width = width)  

g = ggplot(NULL,aes(x=date))+theme_bw()+
  geom_line(aes(y=new_cases),data=it_4.6$data,colour="darkgray",size=1)+
  geom_line(aes(y=new_cases),data=futData,colour="black",size=1)+
  geom_point(aes(y=new_cases),data=it_4.6$data,size=0.3,colour="darkgray")+
  geom_point(aes(y=new_cases),data=futData,colour="black",size=0.3)+
  geom_ribbon(aes(ymin=muFut_lower,ymax=muFut_upper),
              data=data.frame(date = d0+tFut,muFut_lower = apply(muFut_nb2,2,quantile,0.025),
                              muFut_upper = apply(muFut_nb2,2,quantile,0.975)),
              fill = "gray60", alpha = 0.60)+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "3 weeks")+
  ylab("New cases per day")+ylim(0,maxy)+#th+
  geom_vline(data=data.frame(date=max(it_4.6$data$date)),aes(xintercept=date),linetype="dotted",size=0.5)
g
#ggsave("italy_negbinD_0313.pdf",plot=g,device="pdf",height = height,width = width)  

#### Figure 4.7 ####
it_4.7 = load_covid("Italy",last_date = "2020-03-13")
# it_4.7 = readRDS("data/407_it.rds") # Run this to load the data from a file


params = c("a","b","c","f","phi","mu")
initialPar = 'random'
nchain = 1; burn_in = 7e3; lag = 3; sSize = 1000; iter = burn_in + lag*sSize
data_stan = list(y = it_4.7$data$new_cases,n = nrow(it_4.7$data), pop = it_4.7$population, perPop = 0.08, phiTrunc = 2)
it_nb2_02 <- sampling(object = negbin2, data = data_stan,
                  pars = params,
                  chains = nchain,
                  init = initialPar,
                  iter = burn_in+lag*sSize,warmup = burn_in, thin = lag,
                  control = list(max_treedepth = 15, adapt_delta=0.995),
                  verbose = FALSE, open_progress=FALSE, show_messages=FALSE,seed=123)
data_stan = list(y = it_4.7$data$new_cases,n = nrow(it_4.7$data), pop = it_4.7$population, perPop = 0.08, phiTrunc = 3)
it_nb2_03 <- sampling(object = negbin2, data = data_stan,
                  pars = params,
                  chains = nchain,
                  init = initialPar,
                  iter = burn_in+lag*sSize,warmup = burn_in, thin = lag,
                  control = list(max_treedepth = 15, adapt_delta=0.995),
                  verbose = FALSE, open_progress=FALSE, show_messages=FALSE,seed=123)
data_stan = list(y = it_4.7$data$new_cases,n = nrow(it_4.7$data), pop = it_4.7$population, perPop = 0.08, phiTrunc = 5)
it_nb2_05 <- sampling(object = negbin2, data = data_stan,
                  pars = params,
                  chains = nchain,
                  init = initialPar,
                  iter = burn_in+lag*sSize,warmup = burn_in, thin = lag,
                  control = list(max_treedepth = 15, adapt_delta=0.995),
                  verbose = FALSE, open_progress=FALSE, show_messages=FALSE,seed=123)
data_stan = list(y = it_4.7$data$new_cases,n = nrow(it_4.7$data), pop = it_4.7$population, perPop = 0.08, phiTrunc = 10)
it_nb2_10 <- sampling(object = negbin2, data = data_stan,
                  pars = params,
                  chains = nchain,
                  init = initialPar,
                  iter = burn_in+lag*sSize,warmup = burn_in, thin = lag,
                  control = list(max_treedepth = 15, adapt_delta=0.995),
                  verbose = FALSE, open_progress=FALSE, show_messages=FALSE,seed=123)
# it_nb2_02 = readRDS("results/407_it_nb2_02.rds") # Run this to load a pre-executed fit
# it_nb2_03 = readRDS("results/407_it_nb2_03.rds") # Run this to load a pre-executed fit
# it_nb2_05 = readRDS("results/407_it_nb2_05.rds") # Run this to load a pre-executed fit
# it_nb2_10 = readRDS("results/407_it_nb2_10.rds") # Run this to load a pre-executed fit

d0 = it_4.7$data$date[1]-1
tFut = nrow(it_4.7$data)+1:100
muFut_nbT1 = sapply(tFut,function(t) muFun(t,as.data.frame(it_nb2_02)$a,as.data.frame(it_nb2_02)$b,as.data.frame(it_nb2_02)$c,as.data.frame(it_nb2_02)$f))
muFut_nbT2 = sapply(tFut,function(t) muFun(t,as.data.frame(it_nb2_03)$a,as.data.frame(it_nb2_03)$b,as.data.frame(it_nb2_03)$c,as.data.frame(it_nb2_03)$f))
muFut_nbT3 = sapply(tFut,function(t) muFun(t,as.data.frame(it_nb2_05)$a,as.data.frame(it_nb2_05)$b,as.data.frame(it_nb2_05)$c,as.data.frame(it_nb2_05)$f))
muFut_nbT4 = sapply(tFut,function(t) muFun(t,as.data.frame(it_nb2_10)$a,as.data.frame(it_nb2_10)$b,as.data.frame(it_nb2_10)$c,as.data.frame(it_nb2_10)$f))
maxy = max(apply(muFut_nbT1,2,quantile,0.975),apply(muFut_nbT2,2,quantile,0.975),apply(muFut_nbT3,2,quantile,0.975),apply(muFut_nbT4,2,quantile,0.975))

g = ggplot(NULL,aes(x=date))+theme_bw()+
  geom_line(aes(y=new_cases),data=it_4.7$data,colour="darkgray",size=1)+
  geom_line(aes(y=new_cases),data=futData,colour="black",size=1)+
  geom_point(aes(y=new_cases),data=it_4.7$data,size=0.3,colour="darkgray")+
  geom_point(aes(y=new_cases),data=futData,colour="black",size=0.3)+
  geom_ribbon(aes(ymin=muFut_lower,ymax=muFut_upper),
              data=data.frame(date = d0+tFut,muFut_lower = apply(muFut_nbT1,2,quantile,0.025),
                              muFut_upper = apply(muFut_nbT1,2,quantile,0.975)),
              fill = "gray60", alpha = 0.60)+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "3 weeks")+
  ylab("New cases per day")+ylim(0,maxy)+#th+
  geom_vline(data=data.frame(date=max(it_4.7$data$date)),aes(xintercept=date),linetype="dotted",size=0.5)
g
#ggsave("italy_negbinT1_0313.pdf",plot=g,device="pdf",height = height,width = width)  

g = ggplot(NULL,aes(x=date))+theme_bw()+
  geom_line(aes(y=new_cases),data=it_4.7$data,colour="darkgray",size=1)+
  geom_line(aes(y=new_cases),data=futData,colour="black",size=1)+
  geom_point(aes(y=new_cases),data=it_4.7$data,size=0.3,colour="darkgray")+
  geom_point(aes(y=new_cases),data=futData,colour="black",size=0.3)+
  geom_ribbon(aes(ymin=muFut_lower,ymax=muFut_upper),
              data=data.frame(date = d0+tFut,muFut_lower = apply(muFut_nbT2,2,quantile,0.025),
                              muFut_upper = apply(muFut_nbT2,2,quantile,0.975)),
              fill = "gray60", alpha = 0.60)+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "3 weeks")+
  ylab("New cases per day")+ylim(0,maxy)+#th+
  geom_vline(data=data.frame(date=max(it_4.7$data$date)),aes(xintercept=date),linetype="dotted",size=0.5)
g
#ggsave("italy_negbinT2_0313.pdf",plot=g,device="pdf",height = height,width = width)  

g = ggplot(NULL,aes(x=date))+theme_bw()+
  geom_line(aes(y=new_cases),data=it_4.7$data,colour="darkgray",size=1)+
  geom_line(aes(y=new_cases),data=futData,colour="black",size=1)+
  geom_point(aes(y=new_cases),data=it_4.7$data,size=0.3,colour="darkgray")+
  geom_point(aes(y=new_cases),data=futData,colour="black",size=0.3)+
  geom_ribbon(aes(ymin=muFut_lower,ymax=muFut_upper),
              data=data.frame(date = d0+tFut,muFut_lower = apply(muFut_nbT3,2,quantile,0.025),
                              muFut_upper = apply(muFut_nbT3,2,quantile,0.975)),
              fill = "gray60", alpha = 0.60)+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "3 weeks")+
  ylab("New cases per day")+ylim(0,maxy)+#th+
  geom_vline(data=data.frame(date=max(it_4.7$data$date)),aes(xintercept=date),linetype="dotted",size=0.5)
g
#ggsave("italy_negbinT3_0313.pdf",plot=g,device="pdf",height = height,width = width)  

g = ggplot(NULL,aes(x=date))+theme_bw()+
  geom_line(aes(y=new_cases),data=it_4.7$data,colour="darkgray",size=1)+
  geom_line(aes(y=new_cases),data=futData,colour="black",size=1)+
  geom_point(aes(y=new_cases),data=it_4.7$data,size=0.3,colour="darkgray")+
  geom_point(aes(y=new_cases),data=futData,colour="black",size=0.3)+
  geom_ribbon(aes(ymin=muFut_lower,ymax=muFut_upper),
              data=data.frame(date = d0+tFut,muFut_lower = apply(muFut_nbT4,2,quantile,0.025),
                              muFut_upper = apply(muFut_nbT4,2,quantile,0.975)),
              fill = "gray60", alpha = 0.60)+
  scale_x_date(date_labels = "%d/%b/%y",date_breaks = "3 weeks")+
  ylab("New cases per day")+ylim(0,maxy)+#th+
  geom_vline(data=data.frame(date=max(it_4.7$data$date)),aes(xintercept=date),linetype="dotted",size=0.5)
g
#ggsave("italy_negbinT4_0313.pdf",plot=g,device="pdf",height = height,width = width)  
