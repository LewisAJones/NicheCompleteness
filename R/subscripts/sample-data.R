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
library(terra)
library(geosphere)
library(pbmcapply)
source("./R/options.R")
# Analyses-------------------------------------------------------------------
# Create directory
dir.create("./results/virtual-species/sampled/", showWarnings = FALSE)
# Counter for how many species unsampled
n <- 0
for (i in params$stage) {
  species_files <- list.files(paste0("./results/virtual-species/",
                                     i,
                                     "/"), 
                              full.names = TRUE)
  sampling_window <- readRDS(paste0("./results/sampling-window/xy_coords_",
                                    i,
                                    ".RDS"))[, c("p_lng", "p_lat")]
  
  # Run for loop over species files
  indx <- pbmclapply(species_files, function(j) {
    # Load data
    df <- readRDS(j)
    # Extract distribution raster
    background <- rasterize(x = as.matrix.data.frame(df$distribution_xy[, c("x", "y")]),
                            y = rast(res = params$res),
                            value = c(df$distribution_xy[, "layer"]))
    r <- background
    background[background == 1] <- 0
    # Replace 0s (absences) with NAs for masking
    r[r == 0] <- NA
    # Convert raster to sf
    xy <- as.data.frame(r, df = TRUE, xy = TRUE, na.rm = TRUE)[, c("x", "y")]
    # Extract points based on buffer distance
    dist_m <- distm(x = sampling_window,
                    y = xy, fun = distGeo)
    # Get column indexes (i.e. xy coordinates)
    col_ids <- unique(which(dist_m <= params$buffer, arr.ind = TRUE)[, "col"])
    # Subset sampled points
    xy <- xy[col_ids, ]
    # Add to species object
    if (nrow(xy) == 0) {
      df$sampled_distribution_xy <- c("No sampled data")
      saveRDS(df, j)
    }
    else { 
      df$sampled_distribution_xy <- xy
      tmp <- j
      saveRDS(df, j)
      return(tmp)
    }
  }, mc.cores = detectCores()-1, mc.preschedule = FALSE, mc.cleanup = TRUE)
  indx <- unique(unlist(indx))
  saveRDS(indx, paste0("./results/virtual-species/sampled/", i, ".RDS"))
  n <- n + length(indx)
}
# Finish --------------------------------------------------------------------
message(paste0("There are ", n, " out of ",
             (params$n_species)*length(params$stage),
" species sampled."))
beepr::beep(sound = 4)
