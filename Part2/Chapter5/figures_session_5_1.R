library(PandemicLP)
library(dplyr)
library(ggplot2)
require(metR)
require(scales)

Sys.setlocale("LC_TIME", "C") # datas em ingl?s
#gr = (sqrt(5)+1)/2
height = 32;width = 38
muFun = function(t,a,b,c,f=1) exp(log(f)+log(a)+log(c)-(c*t)-(f+1)*log( b+exp(-c*t) ) )

#### Figure 5.1 ####
SAf_5.1 = load_covid("South Africa",last_date = "2020-09-01")
# SAf_5.1 = readRDS("data/501_SAf.rds") # Run this to load the data from a file

SAf_est = pandemic_model(SAf_5.1,covidLPconfig = TRUE)
# SAf_est = readRDS("data/501_SAf.rds") # Run this to load the data from a file

pars = as.data.frame(SAf_est$fit) %>% select(a,b,c,f) %>% colMeans
as = seq(pars[1]*0.9,pars[1]*1.1,len=100)
bs = seq(pars[2]*0.9,pars[2]*1.1,len=100)
cs = seq(pars[3]*0.9,pars[3]*1.1,len=100)
times = 1:nrow(SAf_5.1$data)
contoursAB = rep(0,100*100)
contoursAC = rep(0,100*100)
contoursBC = rep(0,100*100)
for (i in 1:100)
  for (j in 1:100){
    contoursAB[(i-1)*100 + j] = sum(dpois(SAf_5.1$data$new_cases,muFun(times,as[i],bs[j],pars[3]),log=TRUE))
    contoursAC[(i-1)*100 + j] = sum(dpois(SAf_5.1$data$new_cases,muFun(times,as[i],pars[2],cs[j]),log=TRUE))
    contoursBC[(i-1)*100 + j] = sum(dpois(SAf_5.1$data$new_cases,muFun(times,pars[1],bs[i],cs[j]),log=TRUE))
  }

angle=90

th = theme_bw()#+theme(legend.key.size = unit(7,"cm"),
                      #legend.position = "none",
                      # legend.text = element_text(size = 130),
                      # legend.title = element_text(size = 135),
                      # axis.title.y = element_text(size=150),
                      # axis.text.y = element_text(size=170),
                      # axis.text.x = element_text(size=170,angle=angle),
                      # axis.title.x = element_text(size=150))


ab = expand.grid(as,bs)
g1 = data.frame(a = ab[,1], b = ab[,2], likelihood = contoursAB) %>%
  ggplot(aes(x=a,y=b,z=likelihood))+
  geom_contour_fill()+
  geom_contour(color="white")+
  scale_x_continuous(expression(a),expand = c(0,0))+ 
  scale_y_continuous(expression(b),expand = c(0,0))+
  th+
  scale_fill_gradient((as.expression(paste("l(a,b;y)")))
                      ,low="black", high="gray",oob=squish,na.value = "white")




g1
# ggsave(paste0(path,"\\likelihood_ab.pdf"),plot=g1,device="pdf",height = height,
#        width = width)




ac = expand.grid(as,cs)
g2 = data.frame(a = ac[,1], c = ac[,2], likelihood = contoursAC) %>%
  ggplot(aes(x=a,y=round(c,3),z=likelihood))+
  geom_contour_fill()+ 
  scale_x_continuous(expression(a),expand = c(0,0))+ 
  scale_y_continuous(expression(c),expand = c(0,0),breaks = c(0.064,0.068,0.070,0.074))+
  geom_contour(color="white")+
  th+
  scale_fill_gradient((as.expression(paste("l(a,c;y)")))
                      ,low="black", high="gray",oob=squish,na.value = "white")

g2
# ggsave(paste0(path,"\\likelihood_ac.pdf"),plot=g2,device="pdf",height = height,
#        width = width)



bc = expand.grid(bs,cs)
g3=data.frame(b = bc[,1], c = bc[,2], likelihood = contoursBC) %>%
  ggplot(aes(x=round(c,3),y=b,z=likelihood))+
  geom_contour_fill()+ 
  geom_contour(color="white")+
  scale_x_continuous(expression(c),expand = c(0,0),breaks = c(0.064,0.068,0.070,0.074))+ 
  scale_y_continuous(expression(b),expand = c(0,0))+
  th+
  scale_fill_gradient((as.expression(paste("l(b,c;y)")))
                      ,low="black", high="gray",oob=squish,na.value = "white")

g3
# ggsave(paste0(path,"\\likelihood_bc.pdf"),plot=g3,device="pdf",height = height,
#        width = width)

