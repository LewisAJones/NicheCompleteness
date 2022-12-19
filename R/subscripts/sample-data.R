## -------------------------------------------------------------------------#
##
## Script name: sample-data.R
##
## Purpose of script: sample species' distributions
##
## Author: Dr Lewis Jones
##
## Last updated: 2022-12-17
##
# Load packages--------------------------------------------------------------
library(dplyr)
library(raster)
library(geosphere)
source("./R/options.R")
# Analyses-------------------------------------------------------------------
# Create directory
dir.create("./results/virtual-species/sampled/", showWarnings = FALSE)
# Intervals for analyses
intervals <- c("sant", "camp", "maas")
# Counter for how many species unsampled
n <- 0
for (i in intervals) {
  species_files <- list.files(paste0("./results/virtual-species/",
                                     i,
                                     "/"), 
                              full.names = TRUE)
  sampling_window <- readRDS(paste0("./results/sampling-window/xy_coords_",
                                    i,
                                    ".RDS"))[, c("p_lng", "p_lat")]
  
  # Record which samples have been sampled
  indx <- vector()
  # Run for loop over species files
  for (j in species_files) {
    # Load data
    df <- readRDS(j)
    # Extract distribution raster
    background <- df$distribution_ras
    r <- background
    background[background == 1] <- 0
    # Replace 0s (absences) with NAs for masking
    r[r == 0] <- NA
    # Convert raster to sf
    xy <- as.data.frame(rasterToPoints(r))[, c("x", "y")]
    # Extract points based on buffer distance
    dist_m <- distm(x = sampling_window,
                    y = xy, fun = distGeo)
    # Get column indexes (i.e. xy coordinates)
    col_ids <- unique(which(dist_m <= params$buffer, arr.ind = TRUE)[, "col"])
    # Subset sampled points
    xy <- xy[col_ids, ]
    # Add to species object
    if (nrow(xy) == 0) {
      df$sampled_distribution_ras <- background
      df$sampled_distribution_xy <- c("No sampled data")
      n <- n + 1
    }
    else { 
      r <- raster::rasterize(x = xy, y = background, update = TRUE)
      r[r >= 1] <- 1
      df$sampled_distribution_ras <- r
      df$sampled_distribution_xy <- xy
      indx <- append(indx, j)
    }
    saveRDS(df, j)
  }
  saveRDS(indx, paste0("./results/virtual-species/sampled/", i, ".RDS"))
}
# Finish --------------------------------------------------------------------
message(paste0("There are ", n, " out of ",
             (params$n_species)*length(params$stage),
" species not sampled."))
beepr::beep(sound = 4)
