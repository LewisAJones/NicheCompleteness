## -----------------------------------------------------------------------------
##
## Script name: virtual-species-gen.R
##
## Purpose of script: generate virtual species
##
## Author: Dr Lewis Jones
##
## Last updated: 2022-08-31
##
# Load packages ----------------------------------------------------------------
library(raster)
library(pbmcapply)
# Load options
source("./R/options.R")
# Load functions
source("./R/functions/binary-ras.R")
# Generate directory
dir.create("./results/virtual-species/", showWarnings = FALSE)
# Simulation -------------------------------------------------------------------
#intervals for analyses
intervals <- c("sant", "camp", "maas")
#run for loop across intervals
for (int in intervals) {
  
# Load data ---------------------------------------------------------------
  # Create directory
  dir.create(paste0("./results/virtual-species/", int), showWarnings = FALSE)
  
  # Get file paths
  files <- list.files(paste0("./data/climate/", int, "/"), 
                      pattern = ".grd", full.names = TRUE)
  
  # Stack rasters
  stk <- raster::stack(files)
  
  # Background raster
  background <- stk$max_precip
  background[!is.na(background)] <- 0

# Generate seeds ----------------------------------------------------------
  seeds <- raster::as.data.frame(x = stk, xy = TRUE, na.rm = TRUE)
  cells <- cellFromXY(object = stk, seeds[, c("x", "y")])
  seeds <- cbind.data.frame(cells, seeds)
  
  # Types of species
  type <- expand.grid(niche = names(niche_opt), dispersal = names(disp_opt))
  
  # Generate all species types
  seeds <- lapply(1:nrow(type), function(x) {
    seeds$niche <- type$niche[x]
    seeds$dispersal <- type$dispersal[x]
    seeds
  })
  # Bind data
  seeds <- do.call(rbind.data.frame, seeds)
  # Order by seed cell
  seeds <- seeds[order(seeds$cells), ]
  # Remove row names
  row.names(seeds) <- NULL
  # Add niche/dispersal values
  seeds$temp <- NA
  seeds$precip <- NA
  seeds$disp <- NA
  seeds$temp[which(seeds$niche == "broad_niche")]  <- niche_opt$broad_niche["temp"]
  seeds$precip[which(seeds$niche == "broad_niche")] <- niche_opt$broad_niche["precip"]
  seeds$temp[which(seeds$niche == "narrow_niche")]  <- niche_opt$narrow_niche["temp"]
  seeds$precip[which(seeds$niche == "narrow_niche")] <- niche_opt$narrow_niche["precip"]
  seeds$disp[which(seeds$dispersal == "good_disp")] <- disp_opt$good_disp
  seeds$disp[which(seeds$dispersal == "poor_disp")] <- disp_opt$poor_disp
  
  # Add species ID
  seeds$species_id <- 1:nrow(seeds)
  
  # Save seeds
  saveRDS(seeds, paste0("./results/virtual-species/seeds_", int, ".RDS"))
  
# Generate distributions --------------------------------------------------
  # Individual species files
  invisible(pbmclapply(1:n_species, function(x) {
    tmp <- seeds[x, ]
    # Generate origin raster
    seed_ras <- background
    seed_ras[seeds$cells[x]] <- 1
    # Compute tolerances
    tolerances <- list(max_temp = list(lower = tmp$max_temp - 
                                       tmp$temp,
                                     upper = tmp$max_temp + 
                                       tmp$temp),
                     min_temp = list(lower = tmp$min_temp - 
                                       tmp$temp,
                                     upper = tmp$min_temp + 
                                       tmp$temp),
                     max_precip = list(lower = tmp$max_precip - 
                                         (tmp$max_precip * tmp$precip),
                                       upper = tmp$max_precip + 
                                         (tmp$max_precip * tmp$precip)),
                     min_precip = list(lower = tmp$min_precip - 
                                         (tmp$min_precip * tmp$precip),
                                       upper = tmp$min_precip + 
                                         (tmp$min_precip * tmp$precip)))

    # Generate potential distribution
    potential_ras <- binary_ras(x = stk$max_temp,
                                lower = tolerances$max_temp$lower,
                                upper = tolerances$max_temp$upper) +
                      binary_ras(x = stk$min_temp,
                                 lower = tolerances$min_temp$lower,
                                 upper = tolerances$min_temp$upper) +
                      binary_ras(x = stk$min_precip,
                                 lower = tolerances$min_precip$lower,
                                 upper = tolerances$min_precip$upper) +
                      binary_ras(x = stk$max_precip,
                                 lower = tolerances$max_precip$lower,
                                 upper = tolerances$max_precip$upper)
    
    # If all variables agree, suitable for presence
    potential_ras[potential_ras != 4] <- 0
    # Else set as absence
    potential_ras[potential_ras == 4] <- 1
      
    # Generate dispersal steps
    dispersal_steps <- sample(x = 0:tmp$disp, size = burn_in, replace = TRUE,
                              prob = dexp(x = 0:tmp$disp, rate = 1))
    
    # Set up distribution raster
    dist_ras <- seed_ras
    
    # Filter non-dispersal steps
    dispersal_steps <- dispersal_steps[which(dispersal_steps > 0)]
    
    # Simulate dispersal
    for (i in 1:length(dispersal_steps)) {
      
      # Generate matrix of weights for searching
      w <- matrix(
        rep(x = 1, 
            length.out = ((dispersal_steps[i]*2)+1)^2),
        nr = (dispersal_steps[i]*2)+1,
        nc = (dispersal_steps[i]*2)+1)
      
      # Set central value in matrix as 0 
      w[ceiling(length(w)/2)] <- 0
      
      # Set up focal window
      f <- focal(x = dist_ras, w = w, pad = TRUE, padValue = 0, na.rm = TRUE)
      
      # Update values not in focal window to NA
      f[f < 1] <- NA
      
      # Mask by focal window to retain suitable cells within search area
      f <- mask(x = potential_ras, mask = f, updatevalue = 0)
      
      # Remove non-suitable cells
      f[f < 1] <- NA
      
      # Remove already occupied cells
      f <- mask(x = f, mask = dist_ras, maskvalue = 1)
      
      # If no cells to colonise, next timestep
      if (cellStats(f, stat = "sum") == 0) {next}
      
      # Sample a cell for colonising
      f <- sampleRandom(x = f, size = 1, asRaster = TRUE)
      
      # Replace NAs for addition
      f[is.na(f)] <- 0
      
      # Add focal window and origin together
      dist_ras <- dist_ras + f
      
      # Transform all values >= 1 update to 1 (specifies presence)
      dist_ras[dist_ras >= 1] <- 1
    }
    # Distribution xy
    dist_xy <- as.data.frame(rasterToPoints(dist_ras))
    # Retain only xy data for presences
    dist_xy <- dist_xy[which(dist_xy$layer == 1), c("x", "y")]
    # Generate list
    tmp <- list(species_id = tmp$species_id,
                seed_cell = tmp$cells,
                seed_ras = seed_ras,
                seed_xy = cbind.data.frame(tmp$x,
                                           tmp$y),
                seed_vals = cbind.data.frame(tmp$max_temp,
                                             tmp$min_temp,
                                             tmp$max_precip,
                                             tmp$min_precip),
                niche_type = tmp$niche,
                niche_tolerances = tolerances,
                potential_ras = potential_ras,
                dispersal_type = tmp$dispersal,
                dispersal_steps = dispersal_steps,
                distribution_ras = dist_ras,
                distribution_xy =dist_xy
                )
    saveRDS(tmp, paste0("./results/virtual-species/", int, "/species-", x, ".RDS"))
  }, mc.cores = detectCores()-1, mc.preschedule = FALSE, mc.cleanup = TRUE))
}