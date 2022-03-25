## ---------------------------
##
## Script name: run-analysis.R
##
## Purpose of script: runs analysis
##
## Author: Dr Lewis Jones
##
## Date Created: 2022-03-04
##
## Copyright (c) Lewis Jones, 2022
## Email: LewisA.Jones@outlook.com
##
#---------Analyses-----------

#prepare climate data
source("./R/subscripts/prepare-climate-data.R")
rm(list = ls())

#create sampling windows
source("./R/subscripts/sampling-window.R")
rm(list = ls())

#generate virtual species (run on CESGA)
#source("./R/subscripts/virtual-species-random.R")
#rm(list = ls())

#sample species' distributions
source("./R/subscripts/sample-data.R")
rm(list = ls())

#run ecospat analyses
source("./R/subscripts/ecospat-analysis.R")
rm(list = ls())

#run ENMTools analyses
source("./R/subscripts/ENMTools-analysis.R")
rm(list = ls())