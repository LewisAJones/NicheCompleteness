## -----------------------------------------------------------------------------
##
## Script name: virtual-species.R
##
## Purpose of script: generate virtual species
##
## Author: Dr Lewis Jones
##
## Last updated: 2022-08-18
##
# Load packages ----------------------------------------------------------------
library(raster)
library(virtualspecies)
library(pbmcapply)
# Update wd if using CESGA
setwd("/mnt/netapp2/Store_uni/home/uvi/ba/ljo/NicheCompleteness/")
# Load options
source("./R/options.R")
# Generate directory
dir.create("./results/virtual-species/", showWarnings = FALSE)
# Analyses----------------------------------------------------------------------
#intervals for analyses
intervals <- c("sant", "camp", "maas")
#run for loop across intervals
for(int in intervals){
  # Create directory
  dir.create(paste0("./results/virtual-species/", int), showWarnings = FALSE)
  
  # Get file paths
  files <- list.files(paste0("./data/climate/", int, "/"), 
                      pattern = ".grd", full.names = TRUE)
  
  # Stack rasters
  stk <- raster::stack(files)
  
  invisible(pbmclapply(1:n_species, function(n){
    
    # Generate random species
    exist <- 0
    # If species cannot exist due to lack of available environment, rerun
    while(exist == 0){
      random.sp <- virtualspecies::generateRandomSp(raster.stack = stk,
                                    approach = "response",
                                    relations = "gaussian",
                                    realistic.sp = TRUE,
                                    species.type = "multiplicative",
                                    rescale.each.response = TRUE,
                                    convert.to.PA = TRUE,
                                    PA.method = "threshold",
                                    beta = 0.5,
                                    plot = plot_output
      )
      vals <- na.omit(raster::getValues(random.sp$pa.raster))
      if(length(vals) > 0){exist <- 1}
    }
    
    # Dispersal capacity (randomly sample from good vs poor dispersal)
    random.sp$dispersal_cap <- sample(x = disp_cap, size = 1)
    
    # Exploration stage (sample from potential options)
    # Generate d for good dispersal capacity
    if(random.sp$dispersal_cap == "good_disp"){
      d <- sample(x = seq(0, good_disp, 1), size = burn_in, replace = TRUE,
                  prob = exp(-1.2 * seq(0, good_disp, 1)))
    }
    # Generate d for poor dispersal capacity
    if(random.sp$dispersal_cap == "poor_disp"){
      d <- sample(x = seq(0, poor_disp, 1), size = burn_in, replace = TRUE,
                  prob = exp(-1.6 * seq(0, poor_disp, 1)))
    }
    
    # Add dispersal search for species 
    random.sp$dispersal <- d
    
    # Sample origin occurrence from random species
    random.sp$origin <- virtualspecies::sampleOccurrences(
      x = random.sp, n = 1, plot = plot_output, 
      replacement = TRUE)$sample.points[c("x", "y")]
    
    # Generate raster of species origin
    r_origin <- random.sp$pa.raster
    r_origin[!is.na(r_origin)] <- 0
    r_origin[raster::cellFromXY(object = r_origin, xy = random.sp$origin)] <- 1
    
    # Dispersal simulation stage
    for(i in 1:burn_in){
      
      # Next iteration if there is no exploratory search
      if(d[i] == 0){next} 
      
      # Generate matrix of weights for searching
      w <- matrix(
        rep(x = 1, 
            length.out = ((d[i]*2)+1)^2), nr = (d[i]*2)+1, nc = (d[i]*2)+1)
      
      # Set central value in matrix as 0 
      w[ceiling(length(w)/2)] <- 0
      
      # Set up focal window
      f <- focal(x = r_origin, w = w, pad = TRUE, padValue = 0, na.rm = TRUE)
      
      # Update values not in focal window to NA
      f[f < 1] <- NA
      
      # Mask by focal window to retain suitable cells within search area
      f <- mask(x = random.sp$pa.raster, mask = f, updatevalue = 0)
      
      # Remove non-suitable cells
      f[f < 1] <- NA
      
      # Remove already occupied cells
      f <- mask(x = f, mask = r_origin, maskvalue = 1)
      
      # If no cells to colonise, next timestep
      if (cellStats(f, stat = "sum") == 0) {next}
      
      # Sample a cell for colonising
      f <- sampleRandom(x = f, size = 1, asRaster = TRUE)
      
      # Replace NAs for addition
      f[is.na(f)] <- 0
      
      # Add focal window and origin together
      r_origin <- r_origin + f
      
      # Transform all values >= 1 update to 1 (specifies presence)
      r_origin[r_origin >= 1] <- 1
      
      if(plot_output == TRUE){
        par(mfrow=c(2,1))
        plot(random.sp$pa.raster, main = "Habitat suitability")
        plot(r_origin, main = paste0("Dispersal simulation: timestep ", i))
        Sys.sleep(0.1)
      }
    }
    # Add raster to species object
    random.sp$ras_distribution <- r_origin
    
    # Convert raster to spatial points
    xy <- as.data.frame(rasterToPoints(r_origin))
    
    # Retain only xy data for presences
    xy <- xy[which(xy$layer == 1),c("x", "y")]
    
    # Add to species object
    random.sp$distribution <- xy
    
    #save data
    saveRDS(random.sp, paste0("./results/virtual-species/", int, "/species-", n, ".RDS"))
  }, mc.cores = detectCores()-1, mc.preschedule = FALSE, mc.cleanup = TRUE)) #for parallel processing
}
