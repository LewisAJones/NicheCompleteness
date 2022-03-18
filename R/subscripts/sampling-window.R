## ---------------------------
##
## Script name: sampling-window.R
##
## Purpose of script: generate sampling window
##
## Author: Dr Lewis Jones
##
## Date Created: 2022-03-08
##
## Copyright (c) Lewis Jones, 2022
## Email: LewisA.Jones@outlook.com
##
#---------Load packages-----------
library(dplyr)
library(sf)
library(chronosphere)
source("./R/options.R")
#-----------Get data--------------
#download fossil collections from PBDB
#download only includes Santonian-Maastrichtian data
#download only include N. America data
#download excludes marine data
collections <- read.csv("https://paleobiodb.org/data1.2/colls/list.csv?interval=Santonian,Maastrichtian&envtype=!marine&show=loc,timebins,geo")
#define collection mid age
collections$mid_ma <- (collections$max_ma+collections$min_ma)/2
#max and min ages of Santonian, Campanian and Maastrichtian
max_ma <- c(86.3, 83.6, 72.1)
min_ma <- c(83.6, 72.1, 66)
intervals <- c("sant", "camp", "maas")
#assign interval
collections$interval <- NA
collections[which(collections$mid_ma > min_ma[1] & collections$mid_ma < max_ma[1]),c("interval")] <- intervals[1]
collections[which(collections$mid_ma > min_ma[2] & collections$mid_ma < max_ma[2]),c("interval")] <- intervals[2]
collections[which(collections$mid_ma > min_ma[3] & collections$mid_ma < max_ma[3]),c("interval")] <- intervals[3]

#reduce dataframe to interest columns
collections <- collections[,c("lng", "lat", "interval")] 
#reduce to unique instances for sampling window
collections <- unique(collections)
#add age of climate simulation for rotating data
collections$rot_age <- NA
collections[which(collections$interval == "sant"),c("rot_age")] <- 86.7
collections[which(collections$interval == "camp"),c("rot_age")] <- 80.8
collections[which(collections$interval == "maas"),c("rot_age")] <- 69.0
#-----------Rotate data--------------
#get plate rotation model
pm <- fetch("paleomap", "model", datadir = "./data/raw-data/rot-model/")

for(i in intervals){
  tmp <- subset(collections, interval == i)
  #rotate data
  sampling_window <- reconstruct(x = tmp[,c("lng", "lat")], #coordinates of data
                        age = unique(tmp$rot_age), #rotation age of data
                        model = pm, #plate model
                        dir = "./data/raw-data/rot-model/", #directory of plate model
                        cleanup = TRUE,
                        verbose = FALSE) 

  #convert to dataframe
  sampling_window <- data.frame(sampling_window)
  #convert to sf
  sampling_window <- sampling_window %>% 
    st_as_sf(coords = 1:2, crs = gcrs) %>%
    st_transform(crs = prj)
  #generate empty raster for desired resolution
  r <- raster(res = 1)
  r <- projectRaster(from = r, crs = prj, res = res)
  #rasterize sampling window data
  r <- rasterize(x = sampling_window, y = r)
  #save xy data
  saveRDS(sampling_window, paste0("./results/sampling-window/xy_coords_", i, ".RDS"))
  #save raster data
  writeRaster(r, paste0("./results/sampling-window/sampling_raster_", i, ".grd"), overwrite = TRUE)
}

