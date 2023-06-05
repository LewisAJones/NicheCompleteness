# Header ----------------------------------------------------------------
# Project: NicheCompleteness
# File name: run-analyses.R
# Last updated: 2023-03-16
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/NicheCompleteness
# Analyses---------------------------------------------------------------
# Prepare climate data
source("./R/subscripts/prepare-climate-data.R")
rm(list = ls())

# Prepare fossil sampling windows
source("./R/subscripts/sampling-window.R")
rm(list = ls())

# Sample climate data
source("./R/subscripts/sample-climate-data.R")
rm(list = ls())

# Generate distance matrices
source("./R/subscripts/distance-matrix-gen.R")
rm(list = ls())

# Generate virtual species
source("./R/subscripts/virtual-species-gen.R")
rm(list = ls())

# Sample species' distributions
source("./R/subscripts/sample-data.R")
rm(list = ls())

# Richness calculation
source("./R/subscripts/richness-calc.R")
rm(list = ls())

# Run ecospat analyses
source("./R/subscripts/ecospat-analysis.R")
rm(list = ls())

# Summarise ecospat analyses
source("./R/subscripts/ecospat-summary.R")
rm(list = ls())

# Run predicts analyses
source("./R/subscripts/predicts-analysis.R")
rm(list = ls())

# Summarise predicts analyses
source("./R/subscripts/predicts-summary.R")
rm(list = ls())

# Figures-----------------------------------------------------------------------
source("./R/figures/niche-schematic.R")
rm(list = ls())

source("./R/figures/study-site.R")
rm(list = ls())

source("./R/figures/sampled-climate.R")
rm(list = ls())

source("./R/figures/ecospat-violin.R")
rm(list = ls())

source("./R/figures/predicts-violin.R")
rm(list = ls())

source("./R/figures/confusion-matrix.R")
rm(list = ls())
