## -----------------------------------------------------------------------------
##
## Script name: sampling-window.R
##
## Purpose of script: generate sampling window
##
## Author: Dr Lewis Jones
##
## Last updated: 2022-03-08
##
# Load packages ----------------------------------------------------------------
library(dplyr)
library(sf)
library(chronosphere)
source("./R/options.R")
# Create directory for results
dir.create("./results/sampling-window/", showWarnings = FALSE)
# Get data ---------------------------------------------------------------------
# Download fossil collections from PBDB
# Download only includes Santonian-Maastrichtian data
# Download only include N. America data
# Download excludes marine data
collections <- read.csv(
  paste0("https://paleobiodb.org/data1.2/colls/list.csv?",
  "interval=Santonian,Maastrichtian&envtype=!marine&show=loc,timebins,geo"))
# Define collection mid age
collections$mid_ma <- (collections$max_ma+collections$min_ma) / 2
# Max and min ages of Santonian, Campanian and Maastrichtian
max_ma <- c(86.3, 83.6, 72.1)
min_ma <- c(83.6, 72.1, 66)
intervals <- c("sant", "camp", "maas")
# Assign interval
collections$interval <- NA
collections[which(
  collections$mid_ma > min_ma[1] & collections$mid_ma < max_ma[1]),
  c("interval")] <- intervals[1]
collections[which(
  collections$mid_ma > min_ma[2] & collections$mid_ma < max_ma[2]),
  c("interval")] <- intervals[2]
collections[which(
  collections$mid_ma > min_ma[3] & collections$mid_ma < max_ma[3]),
  c("interval")] <- intervals[3]

# Reduce dataframe to interest columns
collections <- collections[,c("lng", "lat", "interval")] 
# Reduce to unique instances for sampling window
collections <- unique(collections)
# Add age of plate rotation model used for palaeoDEMs (Scotese & Wright, 2018)
collections$rot_age <- NA
collections[which(collections$interval == "sant"),c("rot_age")] <- 85
collections[which(collections$interval == "camp"),c("rot_age")] <- 80
collections[which(collections$interval == "maas"),c("rot_age")] <- 70
# Rotate data-------------------------------------------------------------------
# Get plate rotation model
pm <- fetch("paleomap", "model", datadir = "./data/raw-data/rot-model/")

for(i in intervals){
  # Subset data
  tmp <- subset(collections, interval == i)
  # Rotate data
  sampling_window <- chronosphere::reconstruct(
    x = tmp[,c("lng", "lat")], #coordinates of data
    age = unique(tmp$rot_age), #rotation age of data
    model = pm, #plate model
    dir = "./data/raw-data/rot-model/", #dir of plate model
    cleanup = TRUE,
    verbose = FALSE) 

  # Convert to dataframe
  sampling_window <- data.frame(sampling_window)
  # Generate raster for desired resolution using climate data as base
  r <- raster(paste0("./data/climate/", i, "/max_temp.grd"))
  r[!is.na(r)] <- 0
  # Rasterize sampling window data
  ras <- rasterize(x = sampling_window, y = r, background = 0)
  # Mask data
  ras <- mask(x = ras, mask = r, updatevalue = NA)
  # Save xy data
  saveRDS(sampling_window, 
          paste0("./results/sampling-window/xy_coords_", i, ".RDS"))
  # Save raster data
  writeRaster(ras,
              paste0("./results/sampling-window/sampling_raster_", i, ".grd"),
              overwrite = TRUE)
}

