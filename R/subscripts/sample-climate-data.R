# -----------------------------------------------------------------------
# Project: NicheCompleteness
# File name: sample-climate-data.R
# Last updated: 2023-02-25
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/NicheCompleteness
# Load libraries --------------------------------------------------------
library(dplyr)
library(terra)
library(geosphere)
source("./R/options.R")
# -----------------------------------------------------------------------
# Create emtpy dataframe
df <- data.frame()
for (i in params$stage) { 
  # Stack climate rasters
  stk <- rast(list.files(paste0("./data/climate/", i), 
                           pattern = ".tiff", full.names = TRUE))
  # Load sampling window
  sampling_window <- readRDS(paste0("./results/sampling-window/xy_coords_",
                                    i,
                                    ".RDS"))[, c("p_lng", "p_lat")]
  # Convert raster to sf
  xy <- as.data.frame(stk, xy = TRUE)[, c("x", "y")]
  # Extract points based on buffer distance
  dist_m <- distm(x = sampling_window,
                  y = xy, fun = distGeo)
  # Get column indexes (i.e. xy coordinates)
  col_ids <- unique(which(dist_m <= params$buffer, arr.ind = TRUE)[, "col"])
  # Extract climate data
  full <- terra::extract(x = stk, y = xy, df = TRUE)
  full$sampled <- "Available"
  # Subset sampled points
  xy <- xy[col_ids, ]
  # Extract climate data
  sampled <- raster::extract(x = stk, y = xy, df = TRUE)
  sampled$sampled <- "Sampled"
  # Bind data
  full <- rbind.data.frame(full, sampled)
  # Add interval
  full$interval <- i
  # Bind to df
  df <- rbind.data.frame(df, full)
}
# Update names
df[which(df$interval == "sant"), c("interval")] <- c("Santonian")
df[which(df$interval == "camp"), c("interval")] <- c("Campanian")
df[which(df$interval == "maas"), c("interval")] <- c("Maastrichtian")
df$interval = factor(df$interval, 
                     levels=c("Santonian", "Campanian", "Maastrichtian"))
# Save ---------------------------------------------------------------------
dir.create("./results/climate", showWarnings = FALSE)
saveRDS(df, "./results/climate/sampled-climate.RDS")
# Finish -------------------------------------------------------------------
beepr::beep(sound = 4)