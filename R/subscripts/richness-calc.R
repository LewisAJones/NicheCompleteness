## -------------------------------------------------------------------------#
##
## Script name: richness_calc.R
##
## Purpose of script: richness maps
##
## Author: Dr Lewis Jones
##
## Last updated: 2022-12-18
##
# Load packages--------------------------------------------------------------
library(raster)
# Run script-----------------------------------------------------------------
# Create directory
dir.create("./results/virtual-species/richness/", showWarnings = FALSE)
# Intervals
intervals <- c("sant", "camp", "maas")
# Run for loop over intervals
for (i in intervals) {
  # Output directory
  od <- paste0("./results/virtual-species/richness/", i)
  # Get species files
  species_files <- list.files(paste0("./results/virtual-species/", i, "/"),
                              full.names = TRUE)
  # Get landscape for interval
  r <- readRDS(species_files[1])$potential_ras
  r[r > 0] <- 0
  
  # Potential distribution diversity ----------------------------------------
  for (j in species_files) {
    tmp <- readRDS(j)$potential_ras
    r <- r + tmp
  }
  # Save raster
  writeRaster(r, paste0(od, "_potential.grd"), 
              overwrite = TRUE)
  
  # Realised distribution diversity -----------------------------------------
  # Reset background
  r[r > 0] <- 0
  # For loop (add all species distributions for diversity map)
  for (j in species_files) {
    tmp <- readRDS(j)$distribution_ras
    r <- r + tmp
  }
  # Save raster
  writeRaster(r, paste0(od, "_distribution.grd"), 
              overwrite = TRUE)
  
  # Sampled distribution diversity ------------------------------------------
  # Reset background
  r[r > 0] <- 0
  # For loop (add all species distributions for diversity map)
  for (j in species_files){
    tmp <- readRDS(j)$sampled_distribution_ras
    r <- r + tmp
  }
  # Save raster
  writeRaster(r, paste0(od, "_sampled.grd"), 
              overwrite = TRUE)
}
# Finish --------------------------------------------------------------------
beepr::beep(sound = 4)
