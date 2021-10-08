###############################################
#### Listing 10.4 ####
###############################################
#!/bin/bash
cd /home/CodidLP/Covid/app_COVID19/
  
git pull https://username:passwd@github.com/CovidLP/app_COVID19 master

Rscript /home/CodidLP/Covid/R/STAN/world_prediction.R

git pull https://username:passwd@github.com/CovidLP/app_COVID19 master
git add /home/CodidLP/Covid/app_COVID19/STpredictions/*
  
datesub=$(date +%m_%d_%Y_%T)
git commit -m "update results_$datesub"

git push https://username:passwd@github.com/CovidLP/app_COVID19 master                   
