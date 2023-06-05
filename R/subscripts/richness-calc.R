# Header ----------------------------------------------------------------
# Project: NicheCompleteness
# File name: richness-calc.R
# Last updated: 2023-03-16
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/NicheCompleteness
# Load packages----------------------------------------------------------
library(terra)
source("./R/options.R")
# Run script-------------------------------------------------------------
# Create directory
dir.create("./results/virtual-species/richness/", showWarnings = FALSE)
# Run for loop over intervals
for (i in params$stage) {
  # Output directory
  od <- paste0("./results/virtual-species/richness/", i)
  # Get species files
  species_files <- list.files(paste0("./results/virtual-species/", i, "/"),
                              full.names = TRUE)
  # Get landscape for interval
  r <- readRDS(species_files[1])
  r <- rasterize(x = as.matrix.data.frame(r$potential_xy[, c("x", "y")]),
                 y = rast(res = params$res),
                 value = c(r$potential_xy[, "layer"]))
  r[r > 0] <- 0
  
  # Potential distribution diversity ----------------------------------------
  for (j in species_files) {
    tmp <- readRDS(j)
    tmp <- tmp$potential_xy[which(tmp$potential_xy$layer == 1), c("x", "y")]
    tmp <- cellFromXY(object = r, xy = tmp)
    r[tmp] <- unlist(r[tmp]) + 1
  }
  # Save raster
  writeRaster(r, paste0(od, "_potential.tiff"), 
              overwrite = TRUE)
  
  # Realised distribution diversity -----------------------------------------
  # Reset background
  r[r > 0] <- 0
  # For loop (add all species distributions for diversity map)
  for (j in species_files) {
    tmp <- readRDS(j)
    tmp <- tmp$distribution_xy[which(tmp$distribution_xy$layer == 1), c("x", "y")]
    tmp <- cellFromXY(object = r, xy = tmp)
    r[tmp] <- unlist(r[tmp]) + 1
  }
  # Save raster
  writeRaster(r, paste0(od, "_distribution.tiff"), 
              overwrite = TRUE)
  
  # Sampled distribution diversity ---------------------------------------
  # Reset background
  r[r > 0] <- 0
  # For loop (add all species distributions for diversity map)
  for (j in species_files){
    tmp <- readRDS(j)
    if (!is.data.frame(tmp$sampled_distribution_xy)) {next}
    tmp <- cellFromXY(object = r, xy = tmp$sampled_distribution_xy)
    r[tmp] <- unlist(r[tmp]) + 1
  }
  # Save raster
  writeRaster(r, paste0(od, "_sampled.tiff"), 
              overwrite = TRUE)
}
# Finish -----------------------------------------------------------------
beepr::beep(sound = 4)
