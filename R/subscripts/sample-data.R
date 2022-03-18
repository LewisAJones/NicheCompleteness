## ---------------------------
##
## Script name: sample-data.R
##
## Purpose of script: sample species distributions
##
## Author: Dr Lewis Jones
##
## Date Created: 2022-03-04
##
## Copyright (c) Lewis Jones, 2022
## Email: LewisA.Jones@outlook.com
##
#---------Load packages-----------
library(dplyr)
source("./R/options.R")
#-----------Analyses--------------
#intervals for analyses
intervals <- c("sant", "camp", "maas")

#sampling windows
sant_samp <- raster("./results/sampling-window/sampling_raster_sant.grd")
sant_samp[!is.na(sant_samp)] <- 1
camp_samp <- raster("./results/sampling-window/sampling_raster_camp.grd")
camp_samp[!is.na(camp_samp)] <- 1
maas_samp <- raster("./results/sampling-window/sampling_raster_maas.grd")
maas_samp[!is.na(maas_samp)] <- 1

#species files
sant_files <- list.files("./results/virtual-species/sant/", full.names = TRUE)
camp_files <- list.files("./results/virtual-species/camp/", full.names = TRUE)
maas_files <- list.files("./results/virtual-species/maas/", full.names = TRUE)

#counter for how many species unsampled
n <- 0
#-----------Santonian--------------
for(i in sant_files){
  #load data
  df <- readRDS(i)
  #rasterize data for masking
  r <- rasterize(x = df$distribution, y = sant_samp, field = 1)
  #mask data by sampling window
  r <- mask(x = r, mask = sant_samp)
  #convert raster to spatial points
  xy <- as.data.frame(rasterToPoints(r))
  #add to species object
  if(nrow(xy)==0){df$sampled_distribution <- c("No sampled data")
  n <- n + 1}
  else{df$sampled_distribution <- xy}
  saveRDS(df, i)
}
#-----------Campanian--------------
for(i in camp_files){
  #load data
  df <- readRDS(i)
  #rasterize data for masking
  r <- rasterize(x = df$distribution, y = camp_samp, field = 1)
  #mask data by sampling window
  r <- mask(x = r, mask = sant_samp)
  #convert raster to spatial points
  xy <- as.data.frame(rasterToPoints(r))
  #add to species object
  if(nrow(xy)==0){df$sampled_distribution <- c("No sampled data")
  n <- n + 1}
  else{df$sampled_distribution <- xy}
  saveRDS(df, i)
}
#-----------Maastrichtian--------------
for(i in maas_files){
  #load data
  df <- readRDS(i)
  #rasterize data for masking
  r <- rasterize(x = df$distribution, y = maas_samp, field = 1)
  #mask data by sampling window
  r <- mask(x = r, mask = sant_samp)
  #convert raster to spatial points
  xy <- as.data.frame(rasterToPoints(r))
  #add to species object
  if(nrow(xy)==0){df$sampled_distribution <- c("No sampled data")
  n <- n + 1}
  else{df$sampled_distribution <- xy[,c("x", "y")]}
  saveRDS(df, i)
}
#-----------Finish--------------
message(paste0("There are ", n, " out of ",
             length(sant_files) +
              length(camp_files) +
               length(maas_files),
" species not sampled."))
