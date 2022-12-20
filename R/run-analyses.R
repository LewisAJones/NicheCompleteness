##--------------------------------------------------------------------------#
##
## Script name: run-analysis.R
##
## Purpose of script: runs analysis
##
## Author: Dr Lewis Jones
##
## Date Created: 2022-12-19
##
# Set-up ------------------------------------------------------------------
## Vector of required packages
#pkgs <- c("ade4", "dismo", "dplyr", "ecospat", "ENMTools", "geosphere", 
#          "ncdf4", "palaeoverse", "pbmcapply", "raster", "rJava", "stringr")
## Install required packages
#install.packages(pkgs)
## Check if all packages are installed
#pkgs %in% .packages(all.available = TRUE)
## Load packages
#lapply(pkgs, require, character.only = TRUE)
# Analyses-------------------------------------------------------------------
# Update working directory if using CESGA
setwd("/mnt/netapp2/Store_uni/home/uvi/ba/ljo/NicheCompleteness/")

# Prepare climate data
#source("./R/subscripts/prepare-climate-data.R")
#rm(list = ls())

# Prepare fossil sampling windows
#source("./R/subscripts/sampling-window.R")
#rm(list = ls())

# Generate distance matrices
#source("./R/subscripts/distance-matrix-gen.R")
#rm(list = ls())

# Generate virtual species
source("./R/subscripts/virtual-species-gen.R")
rm(list = ls())

# Sample species' distributions
source("./R/subscripts/sample-data.R")
rm(list = ls())

# Richness calculation
source("./R/subscripts/richness-calc.R")
rm(list = ls())

#run ecospat analyses
source("./R/subscripts/ecospat-analysis.R")
rm(list = ls())

##run dismo analyses
source("./R/subscripts/dismo-analysis.R")
rm(list = ls())

# Figures-----------------------------------------------------------------------

# Generate figures
#source("./R/figures/fig-1.R")
#rm(list = ls())

#source("./R/figures/fig-2.R")
#rm(list = ls())