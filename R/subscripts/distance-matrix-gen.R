## ----------------------------------------------------------------------##
##
## Script name: distance-matrix-gen.R
##
## Purpose of script: Generate distance matrix
##
## Author: Dr Lewis Jones
##
## Last update: 2022-12-17
##
## ----------------------------------------------------------------------##
# Load packages -----------------------------------------------------------
library(raster)
library(geosphere)
# Generate directory
dir.create("./data/distances/", showWarnings = FALSE)
# Simulation ---------------------------------------------------------------
#intervals for analyses
intervals <- c("sant", "camp", "maas")
#run for loop across intervals
for (int in intervals) {
  # Load data ----------------------------------------------------------------
  
  # Create directory
  dir.create(paste0("./data/distances/", int), showWarnings = FALSE)
  
  # Get file paths
  files <- list.files(paste0("./data/climate/", int, "/"), 
                      pattern = ".grd", full.names = TRUE)
  
  # Stack rasters
  stk <- raster::stack(files)
  
  # Create background raster
  background <- stk$max_precip
  names(background) <- "land"
  background[!is.na(background)] <- 0

  # Generate distance matrix ----------------------------------------------
  
  # Convert raster to xy
  xy <- as.data.frame(background, xy = TRUE)
  
  # Extract cell indexes
  xy$cell <- cellFromXY(object = background, xy = xy[, c("x", "y")])
  
  # Remove ocean cells (NAs)
  xy <- xy[-which(is.na(xy$land)), ]
  
  # Calculate distance matrix (this takes a while as it is global, but saves
  # repeating this step for each species saving computational time in the end)
  dist_m <- distm(x = xy[, c("x", "y")], fun = distGeo)
  
  # Add row and column names for indexing
  rownames(dist_m) <- xy$cell
  colnames(dist_m) <- xy$cell
  
  # Update 0 values to NAs
  dist_m[which(dist_m == 0)] <- NA
  
  # Save matrix
  saveRDS(object = dist_m, 
          file = paste0("./data/distances/", int, "/dist.RDS"),
          compress = "xz")
}
# Finish ------------------------------------------------------------------
beepr::beep(sound = 4)