# -----------------------------------------------------------------------
# Project: NicheCompleteness
# File name: distance-matrix-gen.R
# Last updated: 2023-02-25
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/NicheCompleteness
# Load libraries --------------------------------------------------------
library(terra)
library(geosphere)
source("./R/options.R")
# Generate directory
dir.create("./data/distances/", showWarnings = FALSE)
# Simulation ------------------------------------------------------------
#run for loop across intervals
for (int in params$stage) {
  # Load data -----------------------------------------------------------
  
  # Create directory
  dir.create(paste0("./data/distances/", int), showWarnings = FALSE)
  
  # Get file paths
  files <- list.files(paste0("./data/climate/", int, "/"), 
                      pattern = ".tiff", full.names = TRUE)
  
  # Stack rasters
  stk <- terra::rast(files)
  
  # Create background raster
  background <- stk$max_precip
  names(background) <- "land"
  background[!is.na(background)] <- 0

  # Generate distance matrix ----------------------------------------------
  
  # Convert raster to xy
  xy <- as.data.frame(background, xy = TRUE, na.rm = TRUE)
  
  # Extract cell indexes
  xy$cell <- cellFromXY(object = background, xy = xy[, c("x", "y")])
  
  # Calculate distance matrix (this takes a while as it is global, but saves
  # repeating this step for each species saving computational time in the end)
  dist_m <- distm(x = xy[, c("x", "y")], fun = distGeo)
  
  # Add row and column names for indexing
  rownames(dist_m) <- xy$cell
  colnames(dist_m) <- xy$cell
  
  # Update 0 values to NAs
  dist_m[which(dist_m == 0)] <- NA
  
  # Save matrix
  saveRDS(object = dist_m, 
          file = paste0("./data/distances/", int, "/dist.RDS"),
          compress = "xz")
}
# Finish ------------------------------------------------------------------
beepr::beep(sound = 4)