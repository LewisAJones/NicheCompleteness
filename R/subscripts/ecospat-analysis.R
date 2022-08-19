## -----------------------------------------------------------------------------
##
## Script name: ecospat-analysis.R
##
## Purpose of script: run tests for niche unfilling & centroid distance
##
## Author: Dr Lewis Jones
##
## Last updated: 2022-08-17
##
# Load packages ----------------------------------------------------------------
library(raster)
library(ecospat)
source("./R/options.R")
# Remove all non-ecospat settings
rm(list=ls()[-which(ls() == "ecospat_res")]) 
# Generate directories ---------------------------------------------------------
dir.create("./results/ecospat/", showWarnings = FALSE)
dir.create("./results/ecospat/plots/", showWarnings = FALSE)
dir.create("./results/ecospat/plots/sant/", showWarnings = FALSE)
dir.create("./results/ecospat/plots/camp/", showWarnings = FALSE)
dir.create("./results/ecospat/plots/maas/", showWarnings = FALSE)
# Analyses ---------------------------------------------------------------------
# Define intervals
intervals <- c("sant", "camp", "maas")
# Run for loop across intervals
for (int in intervals) {
  # Get climate file paths
  files <- list.files(paste0("./data/climate/", int, "/"),
                      pattern = ".grd", full.names = TRUE)
  # Stack rasters
  stk <- stack(files)
  
  # Get species file paths 
  species_files <- readRDS(
    paste0("./results/virtual-species/sampled/", int, ".RDS"))
  
  # Generate empty dataframe
  ecospat_df <- data.frame()
  
  # Run for loop over species files
  for (i in species_files) {
    # Load data
    df <- readRDS(i)
    # Minimum of five points needed for ecospat function
    if (nrow(df$sampled_distribution) < 5) {
      next
      }
    # Extract climate data for whole study area
    df$env <- na.omit(data.frame(getValues(stk)))
    # Which cells are within potential distribution?
    cells_potential <- Which(df$pa.raster == 1, cells = TRUE)
    # Which cells are within the occupied distribution?
    cells_dist <- cellFromXY(df$pa.raster, 
                             xy = df$distribution[, c("x", "y")])
    # Which cells are within the sampled distribution?
    cells_samp <- cellFromXY(df$pa.raster, 
                             xy = df$sampled_distribution[, c("x", "y")])
    
    # Extract climate data to points for potential distribution
    df$potential_env <- raster::extract(x = stk, y = cells_potential)
    # Extract climate data to points for full distribution
    df$full_env <- raster::extract(x = stk, y = cells_dist)
    # Extract climate data to points for sampled distribution
    df$sampled_env <- raster::extract(x = stk, y = cells_samp)
    
    # Compute PCA for whole study area
    pca.env <- ade4::dudi.pca(df$env,
                        center = TRUE,
                        scale = TRUE, 
                        scannf = FALSE, 
                        nf = 2)
    
    # Plot PCA
    #ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)
    
    # PCA scores for the whole study area
    scores.globclim <- pca.env$li
    
    # Add potential distribution
    scores.potential <- suprow(pca.env,df$potential_env)$li
    
    # Add occupied distribution
    scores.full <- suprow(pca.env,df$full_env)$li
    
    # Add sampled distribution
    scores.sampled <- suprow(pca.env,df$sampled_env)$li
    
    # Grid potential distribution niche
    grid.clim.potential <- ecospat.grid.clim.dyn(
      glob = scores.globclim, #entire globe available for background pixels
      glob1 = scores.globclim, #entire globe available for background pixels
      sp = scores.potential, #environmental values for species occurrences
      R = ecospat_res, #resolution of grid
      th.sp = 0, #do not exclude low species density values
      th.env = 0) #do not exclude low species density values
    
    # Grid occupied distribution niche
    grid.clim.full <- ecospat.grid.clim.dyn(
      glob = scores.globclim, #entire globe available for background pixels
      glob1 = scores.potential, #potential area available for background pixels
      sp = scores.full, #environmental values for species occurrences
      R = ecospat_res, #resolution of grid
      th.sp = 0, #do not exclude low species density values
      th.env = 0) #do not exclude low species density values
    
    # Grid sampled distribution niche
    grid.clim.samp <- ecospat.grid.clim.dyn(
      glob = scores.globclim, #entire globe available for background pixels
      glob1 = scores.full, #background pixels (available to sampled species)
      sp = scores.sampled, #sampled environmental values
      R = ecospat_res, #resolution of grid
      th.sp = 0, #do not exclude low species density values
      th.env = 0) #do not exclude low species density values
    
    # Quantify niche dynamics
    # Potential and occupied distribution
    niche.dyn.potential.full <- ecospat.niche.dyn.index(
      z1 = grid.clim.potential, 
      z2= grid.clim.full, 
      intersection = NA)
    # Potential and sampled distribution
    niche.dyn.potential.sampled <- ecospat.niche.dyn.index(
      z1 = grid.clim.potential, 
      z2 = grid.clim.samp,
      intersection = NA)
    # Occupied and sampled distribution
    niche.dyn.full.sampled <- ecospat.niche.dyn.index(
      z1 = grid.clim.full, 
      z2 = grid.clim.samp, 
      intersection = NA)
    
    # Extract niche dynamics metric
    df <- t(data.frame(niche.dyn.potential.full$dynamic.index.w,
                       niche.dyn.potential.sampled$dynamic.index.w,
                       niche.dyn.full.sampled$dynamic.index.w))
    df <- data.frame(df)
   
    # Calculate and add centroid distance
    x1 <- cbind(median(scores.potential[, 1]), median(scores.potential[, 2]))
    x2 <- cbind(median(scores.full[, 1]), median(scores.full[, 2]))
    x3 <- cbind(median(scores.sampled[, 1]), median(scores.sampled[, 2]))
    centroid.potential.full <- as.numeric(sqrt(rowSums((x1 - x2)^2)))
    centroid.potential.sampled <- as.numeric(sqrt(rowSums((x1 - x3)^2)))
    centroid.full.sampled <- as.numeric(sqrt(rowSums((x2 - x3)^2)))
    
    # Format data
    df$centroid <- c(centroid.potential.full,
                     centroid.potential.sampled,
                     centroid.full.sampled)
    df$comparison <- c("potential_full", "potential_sampled", "full_sampled")
    
    # Add species name
    species <- tools::file_path_sans_ext(basename(i))
    species <- as.numeric(substr(species, 9, nchar(species)))
    df <- cbind.data.frame(df, species)
    
    # Remove row names
    rownames(df) <- NULL
    
    # Bind data
    ecospat_df <- rbind.data.frame(ecospat_df, df)
    
    # Plot
    png(paste0("./results/ecospat/plots/", int, "/species-", species, ".png"),
        width = 150,  height = 150, units = "mm", res = 300)
    
    ecospat.plot.niche.dyn(grid.clim.full, grid.clim.samp,
                           quant = 0, interest = 2, 
                           title = tools::file_path_sans_ext(basename(i)), 
                           name.axis1 = "PCA-1", 
                           name.axis2 = "PCA-2")
    title(xlab = "PCA-1", ylab = "PCA-2")
    #ecospat.shift.centroids(scores.full, scores.sampled,
    #                        scores.globclim, scores.globclim)
    dev.off()
    #Sys.sleep(1)
  }
  ecospat_df <- ecospat_df[order(ecospat_df$species),]
  saveRDS(object = ecospat_df, file = paste0("./results/ecospat/", int, ".RDS"))
}

# FINISH -----------------------------------------------------------------------
beepr::beep(sound = 2)
