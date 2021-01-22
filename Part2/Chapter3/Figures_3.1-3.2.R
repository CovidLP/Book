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


#--------- Function mu(t),M(t)
mu_M = function(a,b,c,f,TT){
  t = seq(0,TT)
  mu.t = a*c*f*exp(-c*t)/(b+exp(-c*t))^(f+1)
  M.t = a/(b+exp(-c*t))^(f)
  res = list(mu.t=mu.t,M.t=M.t)
  return(res)
}

a = c(4);b=c(0.02);c=c(0.036);f=c(1)
tt=300

result.1=res.mu=res.M=NULL
for(k in f){
  for(j in c){
    for(l in b){
      for(i in a){
        aux.1 = data.frame(A = paste0("a=",i),
                           B = paste0("b=",l),
                           C = paste0("c=",j),
                           f = paste0("f=",k))
        result.1 = rbind(result.1,aux.1)
        res = mu_M(a=i,b=l,c=j,f=k,TT=tt)
        aux.mu = matrix(c(res$mu.t,0:tt),ncol=2)
        res.mu = rbind(res.mu,aux.mu)
        aux.M = matrix(c(res$M.t,0:tt),ncol=2)
        res.M = rbind(res.M,aux.M)
      }
    }
  }
}

aux.t = nrow(res.mu)/nrow(result.1)
data = data.frame(res.mu,res.M[,-2],apply(result.1,2,rep,each=aux.t))
colnames(data) = c("mu","id","M","a","b","c","f")
head(data)

#-----------------
# Figure 3.1 (a)
#-----------------

plot_M_3.1a= data%>%dplyr::group_split(f)%>% 
  purrr::map(
    ~ggplot(.) + 
      geom_line(aes(x=id,y=round(M,5)))+
      scale_x_continuous(limits = c(0,tt))+xlab("Time")+
      ylab(expression(M(t)))+
      facet_grid(a+b~c,scale="free_y") +
      theme_bw()+
      theme(axis.text.x=element_text(angle=90))
    
  ) %>%
  cowplot::plot_grid(plotlist = ., align = 'hv',ncol=1)

plot_M_3.1a

#-----------------
# Figure 3.1 (b)
#-----------------

plot_M_3.1b= data%>%dplyr::group_split(f)%>% 
  purrr::map(
    ~ggplot(.) + 
      geom_line(aes(x=id,y=round(M,5)))+
      scale_y_log10()+
      scale_x_continuous(limits = c(0,tt))+xlab("Time")+
      ylab(expression(M(t)))+
      facet_grid(a+b~c,scale="free_y") +
      theme_bw()+
      theme(axis.text.x=element_text(angle=90))
    
  ) %>%
  cowplot::plot_grid(plotlist = ., align = 'hv',ncol=1)

plot_M_3.1b

#-----------------
# Figure 3.2 (a)
#-----------------

plot_mu_3.1a= data%>%dplyr::group_split(f)%>% 
  purrr::map(
    ~ggplot(.) + 
      geom_line(aes(x=id,y=round(mu,5)))+
      scale_x_continuous(limits = c(0,tt))+xlab("Time")+
      ylab(expression(M(t)))+
      facet_grid(a+b~c,scale="free_y") +
      theme_bw()+
      theme(axis.text.x=element_text(angle=90))
    
  ) %>%
  cowplot::plot_grid(plotlist = ., align = 'hv',ncol=1)

plot_mu_3.1a

#-----------------
# Figure 3.2 (b)
#-----------------

plot_mu_3.2b= data%>%dplyr::group_split(f)%>% 
  purrr::map(
    ~ggplot(.) + 
      geom_line(aes(x=id,y=round(mu,5)))+
      scale_y_log10()+
      scale_x_continuous(limits = c(0,tt))+xlab("Time")+
      ylab(expression(mu(t)))+
      facet_grid(a+b~c,scale="free_y") +
      theme_bw()+
      theme(axis.text.x=element_text(angle=90))
    
  ) %>%
  cowplot::plot_grid(plotlist = ., align = 'hv',ncol=1)

plot_mu_3.2b

