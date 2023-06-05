# Header ----------------------------------------------------------------
# Project: NicheCompleteness
# File name: ecospat-analysis.R
# Last updated: 2023-03-16
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/NicheCompleteness
# Load packages ---------------------------------------------------------
library(raster)
library(terra)
library(biomod2)
library(ecospat)
library(ade4)
source("./R/options.R")
# Generate directories --------------------------------------------------
dir.create("./results/ecospat/", showWarnings = FALSE)
dir.create("./results/ecospat/plots/", showWarnings = FALSE)
dir.create("./results/ecospat/plots/sant/", showWarnings = FALSE)
dir.create("./results/ecospat/plots/camp/", showWarnings = FALSE)
dir.create("./results/ecospat/plots/maas/", showWarnings = FALSE)
# Analyses --------------------------------------------------------------
# Run for loop across intervals
for (int in params$stage) {
  # Get climate file paths
  files <- list.files(paste0("./data/climate/", int, "/"),
                      pattern = ".tiff", full.names = TRUE)
  # Stack rasters
  stk <- rast(files)
  
  # Get species file paths 
  species_files <- readRDS(
    paste0("./results/virtual-species/sampled/", int, ".RDS"))
  
  # Generate empty dataframe
  ecospat_df <- data.frame()
  
  # Run for loop over species files
  for (i in species_files) {
    # Load data
    df <- readRDS(i)
    
    # Minimum of five points needed for ecospat functionality
    if (nrow(df$sampled_distribution_xy) < 5) {
      next
    }
    
    # Extract climate data for whole study area
    df$env <- terra::values(stk, na.rm = TRUE)
    
    # Which cells are within potential distribution?
    pot_xy <- df$potential_xy
    pot_xy <- pot_xy[which(pot_xy$layer == 1), c("x", "y")]
    cells_potential <- cellFromXY(object = stk, xy = pot_xy)
    
    # Which cells are within the occupied distribution?
    dist_xy <- df$distribution_xy
    dist_xy <- dist_xy[which(dist_xy$layer == 1), c("x", "y")]
    cells_occ <- cellFromXY(object = stk, xy = dist_xy)
    
    # Which cells are within the sampled distribution?
    cells_samp <- cellFromXY(object = stk, xy = df$sampled_distribution_xy)
    
    # Extract climate data to points for potential distribution
    df$potential_env <- raster::extract(x = stk, y = cells_potential)
    
    # Extract climate data to points for full distribution
    df$occuppied_env <- raster::extract(x = stk, y = cells_occ)
    
    # Extract climate data to points for sampled distribution
    df$sampled_env <- raster::extract(x = stk, y = cells_samp)
    
    # Compute PCA for whole study area
    pca_env <- ade4::dudi.pca(df$env,
                        center = TRUE,
                        scale = TRUE, 
                        scannf = FALSE, 
                        nf = 2)
    
    # Plot PCA
    #ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)
    
    # PCA scores for the whole study area
    scores_globclim <- pca_env$li

    # Add potential distribution
    scores_potential <- suprow(pca_env, df$potential_env)$li
    
    # Add occupied distribution
    scores_occ <- suprow(pca_env, df$occuppied_env)$li
    
    # Add sampled distribution
    scores_sampled <- suprow(pca_env, df$sampled_env)$li
    
    # Grid potential distribution niche
    grid_clim_potential <- ecospat.grid.clim.dyn(
      glob = scores_globclim, #entire globe available for background pixels
      glob1 = scores_globclim, #entire globe available for background pixels
      sp = scores_potential, #environmental values for species occurrences
      R = params$ecospat_res, #resolution of grid
      th.sp = 0, #do not exclude low species density values
      th.env = 0) #do not exclude low species density values
    
    # Grid occupied distribution niche
    grid_clim_occ <- ecospat.grid.clim.dyn(
      glob = scores_globclim, #entire globe available for background pixels
      glob1 = scores_potential, #potential distribution available for background pixels
      sp = scores_occ, #environmental values for species occurrences
      R = params$ecospat_res, #resolution of grid
      th.sp = 0, #do not exclude low species density values
      th.env = 0) #do not exclude low species density values
    
    # Grid sampled distribution niche
    grid_clim_samp <- ecospat.grid.clim.dyn(
      glob = scores_globclim, #entire globe available for background pixels
      glob1 = scores_occ, #occupied distribution available for background pixels
      sp = scores_sampled, #sampled environmental values
      R = params$ecospat_res, #resolution of grid
      th.sp = 0, #do not exclude low species density values
      th.env = 0) #do not exclude low species density values
    
    # Quantify niche dynamics
    # Potential and occupied distribution
    niche_dyn_potential_occ <- ecospat.niche.dyn.index(
      z1 = grid_clim_potential, 
      z2 = grid_clim_occ, 
      intersection = NA)
    # Potential and sampled distribution
    niche_dyn_potential_sampled <- ecospat.niche.dyn.index(
      z1 = grid_clim_potential, 
      z2 = grid_clim_samp,
      intersection = NA)
    # Occupied and sampled distribution
    niche_dyn_occ_sampled <- ecospat.niche.dyn.index(
      z1 = grid_clim_occ, 
      z2 = grid_clim_samp, 
      intersection = NA)
    
    # Extract niche dynamics metric
    df <- t(data.frame(niche_dyn_potential_occ$dynamic.index.w,
                       niche_dyn_potential_sampled$dynamic.index.w,
                       niche_dyn_occ_sampled$dynamic.index.w))
    df <- data.frame(df)
   
    # Calculate and add centroid distance
    x1 <- cbind(median(scores_potential[, 1]), median(scores_potential[, 2]))
    x2 <- cbind(median(scores_occ[, 1]), median(scores_occ[, 2]))
    x3 <- cbind(median(scores_sampled[, 1]), median(scores_sampled[, 2]))
    centroid_potential_occ <- as.numeric(sqrt(rowSums((x1 - x2)^2)))
    centroid_potential_sampled <- as.numeric(sqrt(rowSums((x1 - x3)^2)))
    centroid_occ_sampled <- as.numeric(sqrt(rowSums((x2 - x3)^2)))
    
    # Format data
    df$centroid <- c(centroid_potential_occ,
                     centroid_potential_sampled,
                     centroid_occ_sampled)
    df$comparison <- c("potential_occupied", "potential_sampled", "occupied_sampled")
    
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
    
    ecospat.plot.niche.dyn(grid_clim_occ, grid_clim_samp,
                           quant = 0, interest = 2, 
                           title = tools::file_path_sans_ext(basename(i)), 
                           name.axis1 = "PCA-1", 
                           name.axis2 = "PCA-2")
    title(xlab = "PCA-1", ylab = "PCA-2")
    #ecospat.shift.centroids(scores.full, scores.sampled,
    #                        scores.globclim, scores.globclim)
    dev.off()
    #Sys.sleep(1)
    ecospat_df <- ecospat_df[order(ecospat_df$species),]
  }
  saveRDS(object = ecospat_df, file = paste0("./results/ecospat/", int, ".RDS"))
}

# Finish ----------------------------------------------------------------
beepr::beep(sound = 4)
