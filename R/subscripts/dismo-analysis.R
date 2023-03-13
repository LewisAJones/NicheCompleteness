## -----------------------------------------------------------------------------
##
## Script name: dismo-analysis.R
##
## Purpose of script: generate ecological niche models and perform tests
##
## Author: Dr Lewis Jones
##
## Last updated: 2022-08-18
##
# Load packages ----------------------------------------------------------------
library(raster)
library(dismo)
library(ENMTools)
library(rJava)
source("./R/options.R")
source("./R/functions/binary-overlap.R")
# Analyses ---------------------------------------------------------------------
# Create directory
dir.create("./results/dismo/", showWarnings = FALSE)
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
  
  # Raster stack required for dismo
  stk <- raster::stack(files)
  
  # Sample globe for bg points (reduces influence of varying background)
  bg_points <- as.data.frame(stk, xy = TRUE, df = TRUE, na.rm = TRUE)[, c("x", "y")]
  
  # Generate ENMs and run overlap tests
  overlap <- lapply(species_files, function(i){
    
    # Load data
    df <- readRDS(i)
    
    # Extract species name
    species <- tools::file_path_sans_ext(basename(i))
    
    # Format to enmtools.species object to maintain consistent format
    species_occ <- enmtools.species(
      species.name = species,
      presence.points = df$distribution_xy[which(df$distribution_xy$layer == 1), c("x", "y")],
      background.points = bg_points)
    # Check species
    species_occ <- check.species(species_occ)
    
    # Format to enmtools.species object to maintain consistent format
    species_sampled <- enmtools.species(
      species.name = species,
      presence.points = df$sampled_distribution_xy[, c("x", "y")],
      background.points = bg_points)
    # Check species
    species_sampled <- check.species(species_sampled)
    
    #----- Generate BIOCLIM models -----
    # Generate model for occ distribution
    species_occ_bc <- dismo::bioclim(x = stk, 
                                     p = species_occ$presence.points)
    # Get prediction from model
    species_occ_bc_suitability <- dismo::predict(object = species_occ_bc,
                                                 x = stk)

    # Convert to binary predictions (any value more than 0 is suitable in
    # a BIOCLIM model)
    LPT <- min(raster::extract(x = species_occ_bc_suitability,
                               y = species_occ$presence.points))
    species_occ_bc_suitability[species_occ_bc_suitability >= LPT] <- 1
    species_occ_bc_suitability[species_occ_bc_suitability < 1] <- 0
    
    # Generate model for sampled distribution
    species_sampled_bc <- dismo::bioclim(x = stk, 
                                         p = species_sampled$presence.points)
    # Get prediction from model
    species_sampled_bc_suitability <- dismo::predict(object = species_sampled_bc,
                                                     x = stk)
    
    # Convert to binary predictions (any value more than 0 is suitable in
    # a BIOCLIM model)
    LPT <- min(raster::extract(x = species_sampled_bc_suitability,
                               y = species_sampled$presence.points))
    species_sampled_bc_suitability[species_sampled_bc_suitability >= LPT] <- 1
    species_sampled_bc_suitability[species_sampled_bc_suitability < 1] <- 0
    
    #----- Generate MAXENT models -----
    # Generate model for occ distribution
    species_occ_mx <- dismo::maxent(x = stk,
                                     p = species_occ$presence.points,
                                     a = species_occ$background.points)
    # Get prediction from model
    species_occ_mx_suitability <- dismo::predict(object = species_occ_mx,
                                                  x = stk)
    # Get LPT threshold
    LPT <- min(raster::extract(x = species_occ_mx_suitability,
                               y = species_occ$presence.points))
    # Apply threshold
    species_occ_mx_suitability[species_occ_mx_suitability >= LPT] <- 1
    species_occ_mx_suitability[species_occ_mx_suitability < 1] <- 0
    
    # Generate model for sampled distribution
    species_sampled_mx <- dismo::maxent(x = stk,
                                        p = species_sampled$presence.points,
                                        a = species_occ$background.points)
    # Get prediction from model
    species_sampled_mx_suitability <- dismo::predict(object = species_sampled_mx,
                                                     x = stk)
    
    # Get LPT threshold
    LPT <- min(raster::extract(x = species_sampled_mx_suitability,
                               y = species_sampled$presence.points))
    # Apply threshold
    species_sampled_mx_suitability[species_sampled_mx_suitability >= LPT] <- 1
    species_sampled_mx_suitability[species_sampled_mx_suitability < 1] <- 0
    
    #----- Calculate overlap between each model -----
    # Calculate binary overlap
    # create potential ras
    potential_ras <- raster::raster(res = params$res, vals = NA)
    bg <- cellFromXY(object = potential_ras,
                     xy = df$potential_xy[which(df$potential_xy$layer == 0), c("x", "y")])
    pot <- cellFromXY(object = potential_ras,
                      xy = df$potential_xy[which(df$potential_xy$layer == 1), c("x", "y")])
    potential_ras[bg] <- 0
    potential_ras[pot] <- 1
    
    # create distribution ras
    distribution_ras <- raster::raster(res = params$res, vals = NA)
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
  saveRDS(overlap, paste0("./results/dismo/", int, ".RDS"))
}
