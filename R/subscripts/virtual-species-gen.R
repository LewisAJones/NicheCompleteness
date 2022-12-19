## ----------------------------------------------------------------------##
##
## Script name: virtual-species-gen.R
##
## Purpose of script: Generate virtual species
##
## Author: Dr Lewis Jones
##
## Last update: 2022-12-17
##
## ----------------------------------------------------------------------##
# Load packages -----------------------------------------------------------
library(raster)
library(geosphere)
library(pbmcapply)
# Update working directory if using CESGA
#setwd("/mnt/netapp2/Store_uni/home/uvi/ba/ljo/NicheCompleteness/")
# Load options
source("./R/options.R")
# Load functions
source("./R/functions/binary-ras.R")
# Generate directory
dir.create("./results/virtual-species/", showWarnings = FALSE)
# Simulation ---------------------------------------------------------------
#intervals for analyses
intervals <- c("sant", "camp", "maas")
#run for loop across intervals
for (int in intervals) {
# Load data ----------------------------------------------------------------
  
  # Create directory
  dir.create(paste0("./results/virtual-species/", int), showWarnings = FALSE)
  
  # Get file paths
  files <- list.files(paste0("./data/climate/", int, "/"), 
                      pattern = ".grd", full.names = TRUE)
  
  # Stack rasters
  stk <- raster::stack(files)
  
  # Create background raster
  background <- stk$max_precip
  names(background) <- "land"
  background[!is.na(background)] <- 0
  
  # Load distance matrix
  dist_m <- readRDS(paste0("./data/distances/", int, "/dist.RDS"))

# Generate seeds ----------------------------------------------------------
  # Convert to dataframe
  seeds <- raster::as.data.frame(x = stk, xy = TRUE, na.rm = TRUE)
  # Extract cell indexes
  cells <- cellFromXY(object = stk, seeds[, c("x", "y")])
  # Get cell area for each cell
  area <- area(x = stk)[cells]
  # Bind data
  seeds <- cbind.data.frame(cells, area, seeds)
  
  # Sample the number of species desired weighted by cell area
  seeds <- seeds[sample(x = nrow(seeds),
                        size = params$n_species,
                        replace = FALSE,
                        prob = seeds$area), ]
  
  # Types of species
  type <- expand.grid(niche = names(params$niche_opt),
                      dispersal = names(params$disp_opt))
  
  # Randomly assign species type
  ran_type <- sample(x = 1:nrow(type),
                     size = params$n_species,
                     replace = TRUE)
  seeds$niche <- type$niche[ran_type]
  seeds$dispersal <- type$dispersal[ran_type]
  # Order by seed cell
  seeds <- seeds[order(seeds$cells), ]
  # Reset row names
  row.names(seeds) <- 1:nrow(seeds)
  # Add niche/dispersal values
  seeds$temp <- NA
  seeds$precip <- NA
  seeds$disp <- NA
  seeds$temp[which(seeds$niche == "broad_niche")]  <- params$niche_opt$broad_niche$temp
  seeds$precip[which(seeds$niche == "broad_niche")] <- params$niche_opt$broad_niche$precip
  seeds$temp[which(seeds$niche == "narrow_niche")]  <- params$niche_opt$narrow_niche$temp
  seeds$precip[which(seeds$niche == "narrow_niche")] <- params$niche_opt$narrow_niche$precip
  seeds$disp[which(seeds$dispersal == "good_disp")] <- params$disp_opt$good_disp
  seeds$disp[which(seeds$dispersal == "poor_disp")] <- params$disp_opt$poor_disp
  
  # Add species ID
  species_id <- 1:nrow(seeds)
  seeds <- cbind.data.frame(species_id, seeds)
  
  # Save seeds
  saveRDS(seeds, paste0("./results/virtual-species/seeds_", int, ".RDS"))
  
# Generate distributions --------------------------------------------------
  # Individual species files
  invisible(pbmclapply(1:params$n_species, function(x) {
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
    
    # If all variables (n = 4) agree, suitable for presence
    potential_ras <- binary_ras(x = potential_ras, lower = 4, upper = 4)
  
    # Generate dispersal steps (unit: m)
    dispersal_steps <- sample(x = 0:tmp$disp,
                              size = params$burn_in,
                              replace = TRUE,
                              prob = dweibull(x = 0:tmp$disp,
                                              shape = 1,
                                              scale = 1E5))
    
    # Filter non-dispersal steps (less than the minimum distance between cells)
    min_val <- min(dist_m, na.rm = TRUE)
    disp <- dispersal_steps[which(dispersal_steps > min_val)]
    
    # Set up distribution raster
    dist_ras <- seed_ras
    
    # Simulate dispersal
    for (i in seq_along(disp)) {
      
      # Which cells are occupied?
      occ_cells <- as.character(Which(x = dist_ras, 1))
      
      # Sample one population for dispersal
      occ_cells <- sample(occ_cells, size = 1)
      
      # Get cell indexes which are less than or equal X distance from seed
      reachable <- names(which(dist_m[occ_cells, ] <= disp[i]))
      
      # Retain cells which are suitable for occupation (value of 1)
      reachable <- reachable[which(potential_ras[as.numeric(reachable)] == 1)]
        
      # If no cells are available within dispersal distance, move to next
      if (length(reachable) == 0) { next }
        
      # Sample cell for colonising and convert to numeric
      cell_update <- as.numeric(sample(x = reachable, size = 1))
      
      # Set cell value to occupied
      dist_ras[cell_update] <- 1
      
      # Plot time-steps?
      if (params$plot_output == TRUE){
          #par(mfrow=c(2,1))
          #plot(potential_ras, main = "Habitat suitability")
          plot(dist_ras, main = paste0("Dispersal simulation: timestep ", i))
          Sys.sleep(0.1)
      }
    }
    # Distribution xy
    dist_xy <- as.data.frame(rasterToPoints(dist_ras))
    # Retain only xy data for presences
    dist_xy <- dist_xy[which(dist_xy$land == 1), c("x", "y")]
    # Generate list
    tmp <- list(species_id = tmp$species_id,
                seed_cell = tmp$cells,
                seed_ras = seed_ras,
                seed_xy = cbind.data.frame(tmp[, c("x", "y")]),
                seed_vals = cbind.data.frame(tmp[, c("max_temp",
                                                     "min_temp",
                                                     "max_precip",
                                                     "min_precip")]),
                niche_type = tmp[, c("niche")],
                niche_tolerances = tolerances,
                potential_ras = potential_ras,
                dispersal_type = tmp[, c("dispersal")],
                dispersal_steps = dispersal_steps,
                distribution_ras = dist_ras,
                distribution_xy = dist_xy
                )
    saveRDS(tmp, paste0("./results/virtual-species/", int, "/species-", x, ".RDS"))
  }, mc.cores = detectCores()-1, mc.preschedule = FALSE, mc.cleanup = TRUE))
}

# Finish -----------------------------------------------------------------------
beepr::beep(sound = 4)