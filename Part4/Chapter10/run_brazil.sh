###############################################
#### Listing 10.8 ####
###############################################
#!/bin/bash
cd /home/CodidLP/Covid/app_COVID19/

git pull https://username:passwd@github.com/CovidLP/app_COVID19 master

Rscript /home/CodidLP/Covid/R/STAN/brazil_prediction.R
Rscript /home/CodidLP/Covid/R/STAN/aggregate_brazil.R

git pull https://username:passwd@github.com/CovidLP/app_COVID19 master
git add /home/CodidLP/Covid/app_COVID19/STpredictions/*

datesub=$(date +%m_%d_%Y_%T)
git commit -m "update results_$datesub"

git push https://username:passwd@github.com/CovidLP/app_COVID19 master                   
