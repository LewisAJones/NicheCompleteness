## ---------------------------
##
## Script name: richness_calc.R
##
## Purpose of script: richness maps
##
## Author: Dr Lewis Jones
##
## Last updated: 2022-08-17
##
# Load packages-----------------------------------------------------------------
library(raster)
#-------------------------------------------------------------------------------
# Create directory
dir.create("./results/virtual-species/richness/", showWarnings = FALSE)
# Species files
sant_files <- list.files("./results/virtual-species/sant/", full.names = TRUE)
camp_files <- list.files("./results/virtual-species/camp/", full.names = TRUE)
maas_files <- list.files("./results/virtual-species/maas/", full.names = TRUE)

# Sampling windows
sant_samp <- raster("./results/sampling-window/sampling_raster_sant.grd")
sant_samp[sant_samp > 0] <- 1
camp_samp <- raster("./results/sampling-window/sampling_raster_camp.grd")
camp_samp[camp_samp > 0] <- 1
maas_samp <- raster("./results/sampling-window/sampling_raster_maas.grd")
maas_samp[maas_samp > 0] <- 1
# Plotting format
par(mfrow=c(1,3))
# Santonian --------------------------------------------------------------------
# Initial raster
# Potential distribution
# For loop (add all species distributions for diversity map)
r <- readRDS(sant_files[1])$pa.raster
r[r > 0] <- 0
for(i in sant_files){
  tmp <- readRDS(i)$pa.raster
  r <- r + tmp
}
# Save raster
writeRaster(r, "./results/virtual-species/richness/sant_potential.grd", 
            overwrite = TRUE)
# Plot raster
plot(r)
# Initial raster
# Realized and sampled distributions
r <- readRDS(sant_files[1])$ras_distribution
r[r > 0] <- 0
# For loop (add all species distributions for diversity map)
for(i in sant_files){
  tmp <- readRDS(i)$ras_distribution
  r <- r + tmp
}
# Plot raster
plot(r)
# Save raster
writeRaster(r, "./results/virtual-species/richness/sant_div.grd", 
            overwrite = TRUE)
# Mask data
r <- mask(x = r, mask = sant_samp, updatevalue = 0, maskvalue = 0)
# Plot raster
plot(r)
# Save raster
writeRaster(r, "./results/virtual-species/richness/sant_div_samp.grd", 
            overwrite = TRUE)
# Campanian --------------------------------------------------------------------
# Initial raster
# Potential distribution
# For loop (add all species distributions for diversity map)
r <- readRDS(camp_files[1])$pa.raster
r[r > 0] <- 0
for(i in camp_files){
  tmp <- readRDS(i)$pa.raster
  r <- r + tmp
}
# Save raster
writeRaster(r, "./results/virtual-species/richness/camp_potential.grd", 
            overwrite = TRUE)
# Plot raster
plot(r)
# Initial raster
# Realized and sampled distributions
r <- readRDS(camp_files[1])$ras_distribution
r[r > 0] <- 0
# For loop (add all species distributions for diversity map)
for(i in camp_files){
  tmp <- readRDS(i)$ras_distribution
  r <- r + tmp
}
# Plot raster
plot(r)
# Save raster
writeRaster(r, "./results/virtual-species/richness/camp_div.grd", 
            overwrite = TRUE)
# Mask data
r <- mask(x = r, mask = camp_samp, updatevalue = 0, maskvalue = 0)
# Plot raster
plot(r)
# Save raster
writeRaster(r, "./results/virtual-species/richness/camp_div_samp.grd", 
            overwrite = TRUE)
# Maastrichtian-----------------------------------------------------------------
# Initial raster
# Potential distribution
# For loop (add all species distributions for diversity map)
r <- readRDS(maas_files[1])$pa.raster
r[r > 0] <- 0
for(i in maas_files){
  tmp <- readRDS(i)$pa.raster
  r <- r + tmp
}
# Save raster
writeRaster(r, "./results/virtual-species/richness/maas_potential.grd", 
            overwrite = TRUE)
# Plot raster
plot(r)
# Initial raster
# Realized and sampled distributions
r <- readRDS(maas_files[1])$ras_distribution
r[r > 0] <- 0
# For loop (add all species distributions for diversity map)
for(i in maas_files){
  tmp <- readRDS(i)$ras_distribution
  r <- r + tmp
}
# Plot raster
plot(r)
# Save raster
writeRaster(r, "./results/virtual-species/richness/maas_div.grd", 
            overwrite = TRUE)
# Mask data
r <- mask(x = r, mask = maas_samp, updatevalue = 0, maskvalue = 0)
# Plot raster
plot(r)
# Save raster
writeRaster(r, "./results/virtual-species/richness/maas_div_samp.grd", 
            overwrite = TRUE)
#-----------FINISH--------------
beepr::beep(sound = 2)
