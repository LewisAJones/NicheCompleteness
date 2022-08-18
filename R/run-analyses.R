## -----------------------------------------------------------------------------
##
## Script name: run-analysis.R
##
## Purpose of script: runs analysis
##
## Author: Dr Lewis Jones
##
## Date Created: 2022-03-04
##
# Analyses----------------------------------------------------------------------

# Prepare climate data
source("./R/subscripts/prepare-climate-data.R")
rm(list = ls())

# Create sampling windows
source("./R/subscripts/sampling-window.R")
rm(list = ls())

# Generate virtual species (run on CESGA)
source("./R/subscripts/virtual-species-random.R")
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

##run ENMTools analyses
#source("./R/subscripts/ENMTools-analysis.R")
#rm(list = ls())#

# Generate figures
source("./R/figures/niche-plot.R")
rm(list = ls())

source("./R/figures/confusion-matrix.R")
rm(list = ls())