## ----------------------------------------------------------------------##
##
## Script name: sampling-window.R
##
## Purpose of script: Generate sampling window
##
## Author: Dr Lewis Jones
##
## Last update: 2022-12-17
##
## ----------------------------------------------------------------------##
# Load packages -----------------------------------------------------------
library(dplyr)
library(palaeoverse)
source("./R/options.R")
# Create directory for results
dir.create("./results/sampling-window/", showWarnings = FALSE)
# Get data ----------------------------------------------------------------
# Download fossil collections from PBDB
# Download only includes Santonian-Maastrichtian data
# Download excludes marine data
collections <- read.csv(
  paste0("https://paleobiodb.org/data1.2/colls/list.csv?",
  "interval=Santonian,Maastrichtian&envtype=!marine&show=loc,timebins,geo"))
# Define collection midpoint age
collections$mid_ma <- (collections$max_ma + collections$min_ma) / 2
# Max and min ages of Santonian, Campanian and Maastrichtian
max_ma <- c(86.3, 83.6, 72.1)
min_ma <- c(83.6, 72.1, 66)
intervals <- c("sant", "camp", "maas")
# Assign interval
collections$interval <- NA
for (i in 1:3) {
  collections[which(
    collections$mid_ma > min_ma[i] & collections$mid_ma < max_ma[i]),
    c("interval")] <- intervals[i]
}
# Remove data not fitting in any bin
collections <- collections[-which(is.na(collections$interval)), ]
# Reduce dataframe to columns of interest
collections <- collections[, c("lng", "lat", "interval")] 
# Reduce to unique instances for sampling window
collections <- unique(collections)
# Add age of plate rotation model used for palaeoDEMs (Scotese & Wright, 2018)
collections$rot_age <- NA
collections[which(collections$interval == "sant"),c("rot_age")] <- 85
collections[which(collections$interval == "camp"),c("rot_age")] <- 80
collections[which(collections$interval == "maas"),c("rot_age")] <- 70
# Rotate data---------------------------------------------------------------
collections <- palaeorotate(occdf = collections,
                            lng = "lng",
                            lat = "lat",
                            age = "rot_age",
                            method = "point",
                            model = "PALEOMAP")

# Drop collections without coordinates
collections <- collections[-which(is.na(collections$p_lat)), ]
# Save data
for(i in intervals){
  # Subset data
  xy <- subset(collections, interval == i)
  # Save xy data
  saveRDS(xy, 
          paste0("./results/sampling-window/xy_coords_", i, ".RDS"))
}
# Finish ------------------------------------------------------------------
beepr::beep(sound = 4)

