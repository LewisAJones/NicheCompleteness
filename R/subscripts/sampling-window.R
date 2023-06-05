# Header ----------------------------------------------------------------
# Project: NicheCompleteness
# File name: sampling-window.R
# Last updated: 2023-03-15
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/NicheCompleteness
# Load packages ----------------------------------------------------------
library(dplyr)
library(terra)
library(raster)
library(palaeoverse)
# Load options
source("./R/options.R")
# Create directory for results
dir.create("./results/sampling-window/", showWarnings = FALSE)
# Get data ---------------------------------------------------------------
# Download fossil collections from PBDB
# Download only includes Santonian-Maastrichtian data
# Download excludes marine data
collections <- read.csv(
  paste0("https://paleobiodb.org/data1.2/colls/list.csv?",
  "interval=Santonian,Maastrichtian&envtype=!marine&show=loc,timebins,geo"))

# Interval name cleaning and assignment ----------------------------------
# Remove interval name prefix
# Early
collections$early_interval <- gsub(pattern = "Early ",
                              replacement = "",
                              x = collections$early_interval)
collections$late_interval <- gsub(pattern = "Early ",
                             replacement = "",
                             x = collections$late_interval)
# Middle
collections$early_interval <- gsub(pattern = "Middle ",
                              replacement = "",
                              x = collections$early_interval)
collections$late_interval <- gsub(pattern = "Middle ",
                             replacement = "",
                             x = collections$late_interval)
# Late
collections$early_interval <- gsub(pattern = "Late ",
                              replacement = "",
                              x = collections$early_interval)
collections$late_interval <- gsub(pattern = "Late ",
                             replacement = "",
                             x = collections$late_interval)
# remove late interval if the same as early interval
collections$late_interval[
  which(collections$early_interval == collections$late_interval)] <- ""
# Remove uncertain assignments (the presence of a late interval indicates
# occurrence ranges through two intervals)
collections <- collections[which(collections$late_interval == ""), ]
# Assign interval
collections$interval <- NA
collections$interval[which(collections$early_interval == "Santonian")] <- "sant"
collections$interval[which(collections$early_interval == "Campanian")] <- "camp"
collections$interval[which(collections$early_interval == "Maastrichtian")] <- "maas"
collections$interval[which(collections$early_interval == "Lancian")] <- "maas"
collections$interval[which(collections$early_interval == "Judithian")] <- "camp"
# Remove unreliable stage assignments
rm <- c("Cretaceous", "Edmontonian", "Senonian")
collections <- collections[-which(collections$early_interval %in% rm), ]
# Reduce dataframe to columns of interest
collections <- collections[, c("lng", "lat", "interval")] 
# Reduce to unique instances for sampling window
collections <- unique(collections)
# Add age of plate rotation model used for palaeoDEMs (Scotese & Wright, 2018)
collections$age <- NA
collections[which(collections$interval == "sant"),c("age")] <- 85
collections[which(collections$interval == "camp"),c("age")] <- 80
collections[which(collections$interval == "maas"),c("age")] <- 70
# Rotate data-------------------------------------------------------------
collections <- palaeorotate(occdf = collections,
                            lng = "lng",
                            lat = "lat",
                            age = "age",
                            method = "point",
                            model = "PALEOMAP")

# Drop collections without coordinates
collections <- collections[-which(is.na(collections$p_lat)), ]
# Remove data outside of climate layers ---------------------------------
tmp <- data.frame()
intervals <- c("sant", "camp", "maas")
for (i in intervals) {
  file <- paste0("./data/climate/", i, "/max_temp.tiff")
  r <- rast(file)
  xy <- subset(collections, interval == i)
  xy$val <- raster::extract(x = r, y = xy[, c("p_lng", "p_lat")])$max_temp
  tmp <- rbind.data.frame(tmp, xy)
}
collections <- subset(tmp, !is.na(val))
# Save data -------------------------------------------------------------
for (i in intervals) {
  # Subset data
  xy <- subset(collections, interval == i)
  # Save xy data
  saveRDS(xy, 
          paste0("./results/sampling-window/xy_coords_", i, ".RDS"))
}
# Finish -----------------------------------------------------------------
beepr::beep(sound = 4)

