# Header ----------------------------------------------------------------
# Project: NicheCompleteness
# File name: var-plots.R
# Last updated: 2023-03-16
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/NicheCompleteness
# Load packages ---------------------------------------------------------
library(terra)
library(ggplot2)
library(tidyr)
source("./R/functions/climate-plot.R")
# Load rasters ----------------------------------------------------------
sant <- rast(list.files(paste0("./data/climate/sant/"), 
                        pattern = ".tiff", full.names = TRUE))
camp <- rast(list.files(paste0("./data/climate/camp/"), 
                        pattern = ".tiff", full.names = TRUE))
maas <- rast(list.files(paste0("./data/climate/maas/"), 
                        pattern = ".tiff", full.names = TRUE))
# Plot raster -----------------------------------------------------------
climate_plot(x = sant, var = "max_temp", main = "Maximum temperature")
