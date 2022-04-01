## ---------------------------
##
## Script name: richness_calc.R
##
## Purpose of script: richness maps
##
## Author: Dr Lewis Jones
##
## Date Created: 2022-03-23
##
## Copyright (c) Lewis Jones, 2022
## Email: LewisA.Jones@outlook.com
##
#---------Load packages-----------
library(raster)
#---------------------------------
#species files
sant_files <- list.files("./results/virtual-species/sant/", full.names = TRUE)
camp_files <- list.files("./results/virtual-species/camp/", full.names = TRUE)
maas_files <- list.files("./results/virtual-species/maas/", full.names = TRUE)

#sampling windows
sant_samp <- raster("./results/sampling-window/sampling_raster_sant.grd")
sant_samp[!is.na(sant_samp)] <- 1
camp_samp <- raster("./results/sampling-window/sampling_raster_camp.grd")
camp_samp[!is.na(camp_samp)] <- 1
maas_samp <- raster("./results/sampling-window/sampling_raster_maas.grd")
maas_samp[!is.na(maas_samp)] <- 1
#-----------Santonian--------------
#initial raster
r <- readRDS(sant_files[1])$ras_distribution
r[r > 0] <- 0
#for loop (add all species distributions for diversity map)
for(i in sant_files){
  tmp <- readRDS(i)$ras_distribution
  r <- r + tmp
}
plot(r)
writeRaster(r, "./results/virtual-species/misc/sant_div.grd", overwrite = TRUE)
r <- mask(x = r, mask = sant_samp)
writeRaster(r, "./results/virtual-species/misc/sant_div_samp.grd", overwrite = TRUE)
#-----------Campanian--------------
#initial raster
r <- readRDS(camp_files[1])$ras_distribution
r[r > 0] <- 0
#for loop (add all species distributions for diversity map)
for(i in camp_files){
  tmp <- readRDS(i)$ras_distribution
  r <- r + tmp
}
plot(r)
writeRaster(r, "./results/virtual-species/misc/camp_div.grd", overwrite = TRUE)
r <- mask(x = r, mask = sant_samp)
writeRaster(r, "./results/virtual-species/misc/camp_div_samp.grd", overwrite = TRUE)
#-----------Maastrichtian--------------
#initial raster
r <- readRDS(maas_files[1])$ras_distribution
r[r > 0] <- 0
#for loop (add all species distributions for diversity map)
for(i in maas_files){
  tmp <- readRDS(i)$ras_distribution
  r <- r + tmp
}
plot(r)
writeRaster(r, "./results/virtual-species/misc/maas_div.grd", overwrite = TRUE)
r <- mask(x = r, mask = maas_samp)
writeRaster(r, "./results/virtual-species/misc/maas_div_samp.grd", overwrite = TRUE)
#-----------FINISH--------------
beepr::beep(sound = 2)
