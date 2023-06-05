# Header ----------------------------------------------------------------
# Project: NicheCompleteness
# File name: predicts-analysis.R
# Last updated: 2023-03-20
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/NicheCompleteness
# Load packages ---------------------------------------------------------
library(terra)
library(predicts)
library(rJava)
source("./R/options.R")
source("./R/functions/binary-overlap.R")
# Analyses --------------------------------------------------------------
# Create directory
dir.create("./results/predicts/", showWarnings = FALSE)
# Run for loop across intervals
for (int in params$stage) {
  # Get species that have been sampled and have more than 5 occurrences
  species <- unique(readRDS(paste0("./results/ecospat/", int, ".RDS"))$species)
  # Get species files
  species_files <- paste0("./results/virtual-species/",
                          int,
                          "/species-",
                          species, ".RDS")
  # Get interval climate data
  files <- list.files(paste0("./data/climate/", int),
                      pattern = ".tiff", full.names = TRUE)
  
  # Stack rasters
  stk <- rast(files)
  
  # Generate ENMs and run overlap tests
  overlap <- lapply(species_files, function(i){
    # Load data
    df <- readRDS(i)
    
    # Extract species name
    species <- tools::file_path_sans_ext(basename(i))
    
    # Presence points
    pr_points <- df$distribution_xy[which(df$distribution_xy$layer == 1), c("x", "y")]
    pr_points_vars <- extract(x = stk, y = pr_points)
    
    # Sample points
    smp_points <- df$sampled_distribution_xy[, c("x", "y")]
    smp_points_vars <- extract(x = stk, y = smp_points)
    
    #----- Generate BIOCLIM models -----
    # Generate model for occ distribution
    species_occ_bc <- envelope(x = pr_points_vars[, -1])
    # Get prediction from model
    species_occ_bc_suitability <- predict(species_occ_bc, stk)

    # Convert to binary predictions (any value more than 0 is suitable in
    # a BIOCLIM model)
    LPT <- min(extract(species_occ_bc_suitability, pr_points))
    species_occ_bc_suitability[species_occ_bc_suitability >= LPT] <- 1
    species_occ_bc_suitability[species_occ_bc_suitability < 1] <- 0
    
    # Generate model for sampled distribution
    species_sampled_bc <- envelope(x = smp_points_vars[, -1])
    
    # Get prediction from model
    species_sampled_bc_suitability <- predict(species_sampled_bc, stk)
    
    # Convert to binary predictions (any value more than 0 is suitable in
    # a BIOCLIM model)
    LPT <- min(extract(species_sampled_bc_suitability, smp_points))
    species_sampled_bc_suitability[species_sampled_bc_suitability >= LPT] <- 1
    species_sampled_bc_suitability[species_sampled_bc_suitability < 1] <- 0
    
    #----- Generate MAXENT models -----
    # Generate model for occ distribution
    species_occ_mx <- MaxEnt(x = stk, p = pr_points)
    # Get prediction from model
    species_occ_mx_suitability <- predict(species_occ_mx, stk)
    
    # Get LPT threshold
    LPT <- min(extract(species_occ_mx_suitability, pr_points))
    
    # Apply threshold
    species_occ_mx_suitability[species_occ_mx_suitability >= LPT] <- 1
    species_occ_mx_suitability[species_occ_mx_suitability < 1] <- 0
    
    # Generate model for sampled distribution
    species_sampled_mx <- MaxEnt(x = stk, p = pr_points)
    # Get prediction from model
    species_sampled_mx_suitability <- predict(species_sampled_mx, stk)
    
    # Get LPT threshold
    LPT <- min(extract(species_sampled_mx_suitability, smp_points))
    # Apply threshold
    species_sampled_mx_suitability[species_sampled_mx_suitability >= LPT] <- 1
    species_sampled_mx_suitability[species_sampled_mx_suitability < 1] <- 0
    
    #----- Calculate overlap between each model -----
    # Calculate binary overlap
    # create potential ras
    potential_ras <- rast(res = params$res, vals = NA)
    bg <- cellFromXY(object = potential_ras,
                     xy = df$potential_xy[which(df$potential_xy$layer == 0), c("x", "y")])
    pot <- cellFromXY(object = potential_ras,
                      xy = df$potential_xy[which(df$potential_xy$layer == 1), c("x", "y")])
    potential_ras[bg] <- 0
    potential_ras[pot] <- 1
    
    # create distribution ras
    distribution_ras <- rast(res = params$res, vals = NA)
    bg <- cellFromXY(object = distribution_ras,
                     xy = df$distribution_xy[which(df$distribution_xy$layer == 0), c("x", "y")])
    dist <- cellFromXY(object = distribution_ras,
                       xy = df$distribution_xy[which(df$distribution_xy$layer == 1), c("x", "y")])
    distribution_ras[bg] <- 0
    distribution_ras[dist] <- 1
    
    # BIOCLIM
    BIOCLIM <- rbind.data.frame(
    # potential vs occupied distribution
    binary_overlap(x = potential_ras, y = species_occ_bc_suitability),
    # potential vs sampled distribution
    binary_overlap(x = potential_ras, y = species_sampled_bc_suitability),
    # known occupied distribution vs modeled occupied distribution
    binary_overlap(x = distribution_ras, y = species_occ_bc_suitability),
    # known occupied distribution vs sampled distribution
    binary_overlap(x = distribution_ras, y = species_sampled_bc_suitability)
    )
    
    # MAXENT
    MAXENT <- rbind.data.frame(
      # potential vs occupied distribution
      binary_overlap(x = potential_ras, y = species_occ_mx_suitability),
      # potential vs sampled distribution
      binary_overlap(x = potential_ras, y = species_sampled_mx_suitability),
      # known occupied distribution vs modeled occupied distribution
      binary_overlap(x = distribution_ras, y = species_occ_mx_suitability),
      # known occupied distribution vs sampled distribution
      binary_overlap(x = distribution_ras, y = species_sampled_mx_suitability)
    )
    
    # Bind data
    bin_over <- rbind.data.frame(BIOCLIM, MAXENT)
    
    # Add data columns
    bin_over$model <- rep(x = c("BIOCLIM", "MAXENT"), times = 1, each = 4)
    bin_over$known <- rep(x = c("potential", "occupied"), times = 2, each = 2)
    bin_over$predicted <- rep(x = c("occupied", "sampled"), times = 4, each = 1)

    bin_over$species <- species
    bin_over$n <- sum(df$distribution_xy$layer == 1, na.rm = TRUE)
    bin_over$n_samp <- nrow(df$sampled_distribution)
    cat(message(paste0(species, " complete.")))
    # Return data
    return(bin_over)
  })
  # Save data
  saveRDS(overlap, paste0("./results/predicts/", int, ".RDS"))
}
