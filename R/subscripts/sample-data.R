## -----------------------------------------------------------------------------
##
## Script name: sample-data.R
##
## Purpose of script: sample species' distributions
##
## Author: Dr Lewis Jones
##
## Last updated: 2022-03-08
##
# Load packages-----------------------------------------------------------------
library(dplyr)
library(raster)
source("./R/options.R")
# Analyses----------------------------------------------------------------------
# Create directory
dir.create("./results/virtual-species/sampled/", showWarnings = FALSE)
# Intervals for analyses
intervals <- c("sant", "camp", "maas")
# Set-up sampling windows
sant_samp <- raster("./results/sampling-window/sampling_raster_sant.grd")
sant_samp[sant_samp == 0] <- NA
sant_samp[!is.na(sant_samp)] <- 1
camp_samp <- raster("./results/sampling-window/sampling_raster_camp.grd")
camp_samp[camp_samp == 0] <- NA
camp_samp[!is.na(camp_samp)] <- 1
maas_samp <- raster("./results/sampling-window/sampling_raster_maas.grd")
maas_samp[maas_samp == 0] <- NA
maas_samp[!is.na(maas_samp)] <- 1
# Get species files
sant_files <- list.files("./results/virtual-species/sant/", full.names = TRUE)
camp_files <- list.files("./results/virtual-species/camp/", full.names = TRUE)
maas_files <- list.files("./results/virtual-species/maas/", full.names = TRUE)
# Counter for how many species unsampled
n <- 0
# Santonian --------------------------------------------------------------------
# Record which samples have been sampled
indx <- vector()
for (i in sant_files) {
  # Load data
  df <- readRDS(i)
  # Rasterize data for masking
  r <- rasterize(x = df$distribution, y = sant_samp, field = 1)
  # Mask data by sampling window
  r <- mask(x = r, mask = sant_samp)
  # Convert raster to spatial points
  xy <- as.data.frame(rasterToPoints(r))
  # Add to species object
  if (nrow(xy) == 0) {
    df$sampled_distribution <- c("No sampled data")
    n <- n + 1
  }
  else { 
    df$sampled_distribution <- xy[, c("x", "y")]
    indx <- append(indx, i)
  }
  saveRDS(df, i)
}
saveRDS(indx, "./results/virtual-species/sampled/sant.RDS")
# Campanian --------------------------------------------------------------------
# Record which samples have been sampled
indx <- vector()
for (i in camp_files) {
  # Load data
  df <- readRDS(i)
  # Rasterize data for masking
  r <- rasterize(x = df$distribution, y = sant_samp, field = 1)
  # Mask data by sampling window
  r <- mask(x = r, mask = sant_samp)
  # Convert raster to spatial points
  xy <- as.data.frame(rasterToPoints(r))
  # Add to species object
  if (nrow(xy) == 0) {
    df$sampled_distribution <- c("No sampled data")
    n <- n + 1
  }
  else { 
    df$sampled_distribution <- xy[, c("x", "y")]
    indx <- append(indx, i)
  }
  saveRDS(df, i)
}
saveRDS(indx, "./results/virtual-species/sampled/camp.RDS")
# Maastrichtian-----------------------------------------------------------------
# Record which samples have been sampled
indx <- vector()
for (i in maas_files) {
  # Load data
  df <- readRDS(i)
  # Rasterize data for masking
  r <- rasterize(x = df$distribution, y = sant_samp, field = 1)
  # Mask data by sampling window
  r <- mask(x = r, mask = sant_samp)
  # Convert raster to spatial points
  xy <- as.data.frame(rasterToPoints(r))
  # Add to species object
  if (nrow(xy) == 0) {
    df$sampled_distribution <- c("No sampled data")
    n <- n + 1
  }
  else { 
    df$sampled_distribution <- xy[, c("x", "y")]
    indx <- append(indx, i)
  }
  saveRDS(df, i)
}
saveRDS(indx, "./results/virtual-species/sampled/maas.RDS")
# Finish -----------------------------------------------------------------------
message(paste0("There are ", n, " out of ",
             length(sant_files) +
              length(camp_files) +
               length(maas_files),
" species not sampled."))
