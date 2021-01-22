###---------------------------------------------------------------###
###   Building a Platform for Data-Driven Pandemic Prediction:    ###
###   From Data Modelling to Visualization - the CovidLP Project  ###
###                                                               ###
###   Chapter 3: Basic epidemiological features                   ###
###                                                               ###
###                                                               ###
###                                                               ###
###   Author: the CovidLP Team                                    ###
###---------------------------------------------------------------###

###--------Packages
require(ggplot2)
require(tidyr)


#--------- Function mu(t),M(t) e R(t)
mu_M_R = function(a,b,c,f,Q,TT){
  t = seq(0,TT)
  mu.t = a*c*f*exp(-c*t)/(b+exp(-c*t))^(f+1)
  M.t = a/(b+exp(-c*t))^(f)
  den = M.t-Q*M.t
  R=NULL
  for(j in 2:(TT+1)){
    R[j] = mu.t[j]/(den[j-1])
  }
  res = list(mu.t=mu.t,M.t=M.t,R=R)
  return(res)
}

a = c(1,4);b=c(0.02,0.45);c=c(0.02,0.03);f=c(0.5,1,10)
tt=300;Q=.9

result.1=res.mu=res.M=res.R=NULL
for(k in f){
  for(j in c){
    for(l in b){
      for(i in a){
        aux.1 = data.frame(A = paste0("a=",i),
                           B = paste0("b=",l),
                           C = paste0("c=",j),
                           f = paste0("f=", k))
        result.1 = rbind(result.1,aux.1)
        res = mu_M_R(a=i,b=l,c=j,f=k,TT=tt,Q = Q)
        aux.mu = matrix(c(res$mu.t,0:tt),ncol=2)
        res.mu = rbind(res.mu,aux.mu)
        aux.M = matrix(c(res$M.t,0:tt),ncol=2)
        res.M = rbind(res.M,aux.M)
        aux.R = matrix(c(res$R,1:(tt+1)),ncol=2)
        res.R = rbind(res.R,aux.R)
      }
    }
  }
}

aux.t = nrow(res.mu)/nrow(result.1)
data = data.frame(res.mu,res.M[,-2],apply(result.1,2,rep,each=aux.t))
colnames(data) = c("mu","id","M","a","b","c","f")
head(data)

#-----------------
# Figure 3.3
#-----------------

plot.mu= data%>%dplyr::group_split(f)%>% 
  purrr::map(
    ~ggplot(.) + 
      geom_line(aes(x=id,y=round(mu,5)))+
      scale_x_continuous(limits = c(0,tt))+xlab("Time")+
      ylab(expression(mu(t)))+
      facet_grid(a+b~f+c,scale="free_y") +
      theme_bw()+
      theme(axis.text.x=element_text(angle=90))
    
  ) %>%
  cowplot::plot_grid(plotlist = ., align = 'hv',ncol=3)

plot.mu


#-----------------
# Figure 3.4
#-----------------

plot.M= data%>%dplyr::group_split(f)%>% 
  purrr::map(
    ~ggplot(.) + 
      geom_line(aes(x=id,y=round(M,5)))+
      scale_x_continuous(limits = c(0,tt))+xlab("Time")+
      ylab(expression(M(t)))+
      facet_grid(a+b~f+c,scale="free_y") +
      theme_bw()+
      theme(axis.text.x=element_text(angle=90))
    
  ) %>%
  cowplot::plot_grid(plotlist = ., align = 'hv',ncol=3)

plot.M


#-----------------
# Figure 3.5
#-----------------

R = data.frame(res.R,apply(result.1,2,rep,each=aux.t))
colnames(R) = c("R","id","a","b","c","f")

plot.R= R%>%dplyr::group_split(f)%>% 
  purrr::map(
    ~ggplot(.) + 
      geom_line(aes(x=id,y=round(R,5)))+
      scale_x_continuous(limits = c(1,tt))+xlab("Time")+
      ylab(expression(R(t)))+
      facet_grid(a+b~f+c,scale="free_y") +
      geom_hline(yintercept = 1,linetype="dashed",size=1)+
      theme_bw()+
      theme(axis.text.x=element_text(angle=90))
    
  ) %>%
  cowplot::plot_grid(plotlist = ., align = 'hv',ncol=3)
plot.R


