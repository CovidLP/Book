# Instructions for files in this folder

This folder contains the supplementary material for part 2 of the book **Building a Platform for Data-Driven Pandemic Prediction: From Data Modelling to Visualisation - The CovidLP Project**.

## .R files
Each folder has a multitude of .R files containing the R code that can be used to reproduce the figures in the book. In some cases the code can take a long time to run, so alternatives that load pre-executed data are available.

### Pre-requisite
Some R packages are required to run the code. If a package, say 'ppppp', is needed, it will be explicited by the code 'library(ppppp)'. To install a package execute the command

```R
install.packages("ppppp")
```

in the R console, where 'ppppp' is the name of the package required.

## data folders
The data folders contain *.rda* files that contain the necessary raw data to run the various models in the .R files. They are available to avoid having to rely on online repositories which may be offline when the code is run.

There are also *.rds* files that contain the output of the MCMC procedures. In some cases, MCMC can take a long time to run or *stan* code can fail to compile in the computer if it does not have much available memory. Therefore, the option to skip the fitting step of the statistical inference is present, and the user can load a pre-executed fit.
