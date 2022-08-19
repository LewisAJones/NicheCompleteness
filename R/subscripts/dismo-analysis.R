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
library(pbmcapply)
#use CESGA
#setwd("/mnt/netapp2/Store_uni/home/uvi/ba/ljo/NicheCompleteness/")
source("./R/functions/binary-overlap.R")
# Analyses ---------------------------------------------------------------------
# Create directory
dir.create("./results/dismo/", showWarnings = FALSE)
# Define intervals
intervals <- c("camp", "maas")  
# Run for loop across intervals
for (int in intervals) {
  # Get species that have been sampled and have more than 5 occurrences
  species <- unique(readRDS(paste0("./results/ecospat/", int, ".RDS"))$species)
  # Get species files
  species_files <- paste0("./results/virtual-species/",
                          int,
                          "/species-",
                          species, ".RDS")
  # Get interval climate data
  files <- list.files(paste0("./data/climate/", int),
                      pattern = ".grd", full.names = TRUE)
  
  # Stack rasters
  stk <- stack(files)
  
  # Sample globe for bg points (reduces influence of varying background)
  bg.points <- data.frame(rasterToPoints(stk, spatial = FALSE)[, c("x", "y")])
  
  # Generate ENMs and run overlap tests
  overlap <- lapply(species_files, function(i){
    
    # Load data
    df <- readRDS(i)
    
    # Extract species name
    species <- tools::file_path_sans_ext(basename(i))
    
    # Format to enmtools.species object to maintain consistent format
    species_full <- enmtools.species(
      species.name = species,
      presence.points = df$distribution,
      background.points = bg.points)
    # Check species
    species_full <- check.species(species_full)
    
    # Format to enmtools.species object to maintain consistent format
    species_sampled <- enmtools.species(
      species.name = species,
      presence.points = df$sampled_distribution,
      background.points = bg.points)
    # Check species
    species_sampled <- check.species(species_sampled)
    #----- Generate BIOCLIM models -----
    # Generate model for full distribution
    species_full.bc <- dismo::bioclim(x = stk, 
                                      p = species_full$presence.points)
    # Get prediction from model
    species_full.bc.suitability <- dismo::predict(
      object = species_full.bc, 
      x = stk)

    # Convert to binary predictions (any value more than 0 is suitable in
    # climate envelope model)
    LPT <- min(
      raster::extract(x = species_full.bc.suitability,
              y = species_full$presence.points))
    species_full.bc.suitability[species_full.bc.suitability >= LPT] <- 1
    species_full.bc.suitability[species_full.bc.suitability < 1] <- 0
    
    # Generate model for sampled distribution
    species_sampled.bc <- dismo::bioclim(x = stk, 
                                         p = species_sampled$presence.points)
    # Get prediction from model
    species_sampled.bc.suitability <- dismo::predict(
      object = species_sampled.bc,
      x = stk)
    # Convert to binary predictions (any value more than 0 is suitable in
    # climate envelope model)
    LPT <- min(
      raster::extract(x = species_sampled.bc.suitability,
              y = species_sampled$presence.points))
    species_sampled.bc.suitability[species_sampled.bc.suitability >= LPT] <- 1
    species_sampled.bc.suitability[species_sampled.bc.suitability < 1] <- 0
    #----- Generate MAXENT models -----
    # Generate model for full distribution
    species_full.mx <- dismo::maxent(x = stk,
                                     p = species_full$presence.points,
                                     a = species_full$background.points)
    # Get prediction from model
    species_full.mx.suitability <- dismo::predict(object = species_full.mx,
                                                  x = stk)
    # Get LPT threshold
    LPT <- min(raster::extract(x = species_full.mx.suitability,
                         y = species_full$presence.points))
    # Apply threshold
    species_full.mx.suitability[species_full.mx.suitability >= LPT] <- 1
    species_full.mx.suitability[species_full.mx.suitability < 1] <- 0
    
    # Generate model for sampled distribution
    species_sampled.mx <- dismo::maxent(x = stk,
                                        p = species_sampled$presence.points,
                                        a = species_full$background.points)
    # Get prediction from model
    species_sampled.mx.suitability <- dismo::predict(
      object = species_sampled.mx,
      x = stk)
    
    # Get LPT threshold
    LPT <- min(raster::extract(x = species_sampled.mx.suitability,
                               y = species_sampled$presence.points))
    # Apply threshold
    species_sampled.mx.suitability[species_sampled.mx.suitability >= LPT] <- 1
    species_sampled.mx.suitability[species_sampled.mx.suitability < 1] <- 0
    
    #----- Calculate overlap between each model -----
    # Calculate binary overlap
    bin_over <- rbind(
      binary_overlap(x = df$pa.raster,
                     y = species_full.bc.suitability),
      binary_overlap(x = df$pa.raster,
                     y = species_sampled.bc.suitability),
      binary_overlap(x = df$ras_distribution,
                     y = species_full.bc.suitability),
      binary_overlap(x = df$ras_distribution,
                     y = species_sampled.bc.suitability),
      binary_overlap(x = df$pa.raster,
                     y = species_full.mx.suitability),
      binary_overlap(x = df$pa.raster,
                     y = species_sampled.mx.suitability),
      binary_overlap(x = df$ras_distribution,
                     y = species_full.mx.suitability),
      binary_overlap(x = df$ras_distribution,
                     y = species_sampled.mx.suitability)
      )
    # Bind data and format
    bin_over$model <- rep(x = c("BIOCLIM", "MAXENT"), times = 1, each = 4)
    bin_over$comparison <- rep(x = c("potential_full",
                                     "potential_sampled",
                                     "occupied_full",
                                     "occupied_sample"), times = 2, each = 1)
    bin_over$species <- species
    bin_over$n <- nrow(df$distribution)
    bin_over$n_samp <- nrow(df$sampled_distribution)
    cat("species", species)
    # Return data
    return(bin_over)
  })
  # Save data
  saveRDS(overlap, paste0("./results/dismo/", int, ".RDS"))
}
