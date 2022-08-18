## ---------------------------
##
## Script name: virtual-species.R
##
## Purpose of script: generate virtual species
##
## Author: Dr Lewis Jones
##
## Date Created: 2022-03-04
##
## Copyright (c) Lewis Jones, 2022
## Email: LewisA.Jones@outlook.com
##
#---------Load packages-----------
library(raster)
library(virtualspecies)
library(pbmcapply)
#plot outputs
pl = TRUE
#use CESGA
setwd("/mnt/netapp2/Store_uni/home/uvi/ba/ljo/NicheCompleteness/")
source("./R/options.R")
dir.create("./results/virtual-species/", showWarnings = FALSE)
#-----------Analyses--------------
#intervals for analyses
intervals <- c("sant", "camp", "maas")
#run for loop across intervals
for(int in intervals){
  dir.create(paste0("./results/virtual-species/", int), showWarnings = FALSE)
  
  #get file paths
  files <- list.files(paste0("./data/climate/", int, "/"), pattern = ".grd", full.names = TRUE)
  
  #stack rasters
  stk <- raster::stack(files)
  
  invisible(pbmclapply(1:n_species, function(n){
    
    #generate random species
    exist <- 0
    #if species cannot exist due to lack of suitability within environment, rerun prior function
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
                                                    plot = pl
      )
      vals <- raster::getValues(random.sp$pa.raster)
      vals <- na.omit(vals)
      if(length(vals) > 0){exist <- 1}
    }
    
    
    #dispersal capacity (randomly sample from good vs poor dispersal)
    random.sp$dispersal_cap <- sample(x = disp_cap, size = 1, replace = TRUE)
    
    #exploration stage (sample from potential options)
    #generate d for good dispersal capacity
    if(random.sp$dispersal_cap == "good_disp"){
      d <- sample(x = seq(0, good_disp, 1), size = burn_in, replace = TRUE, prob = exp(-1.5 * seq(0, good_disp, 1)))
    }
    #generate d for poor dispersal capacity
    if(random.sp$dispersal_cap == "poor_disp"){
      d <- sample(x = seq(0, poor_disp, 1), size = burn_in, replace = TRUE, prob = exp(-2 * seq(0, poor_disp, 1)))
    }
    
    #add dispersal search for species 
    random.sp$dispersal <- d
    
    #sample origin occurrence from random species
    random.sp$origin <- virtualspecies::sampleOccurrences(x = random.sp, n = 1, plot = pl, replacement = TRUE)$sample.points[c("x", "y")]
    
    #generate raster of species origin
    r_origin <- random.sp$pa.raster
    r_origin[!is.na(r_origin)] <- 0
    r_origin[raster::cellFromXY(object = r_origin, xy = random.sp$origin)] <- 1
    
    #dispersal simulation stage
    for(i in 1:burn_in){
      
      #next iteration if there is no exploratory search
      if(d[i] == 0){next} 
      
      #generate matrix of weights for searching
      w <- matrix(rep(x = 1, length.out = ((d[i]*2)+1)^2), nr = (d[i]*2)+1, nc = (d[i]*2)+1)
      
      #set central value in matrix as 0 
      w[ceiling(length(w)/2)] <- 0
      
      cells <- raster::Which(x = r_origin == 1, cells = TRUE)
      
      for(c in cells){
        #set up focal window
        f <- r_origin
        f[!is.na(f)] <- 0
        f[c] <- 1
        f <- focal(x = f, w = w, pad = TRUE, padValue = 0, na.rm = TRUE)
        
        #mask by focal raster to retain suitable cells within p/a raster
        f <- mask(x = random.sp$pa.raster, mask = f, maskvalue = 0, updatevalue = 0)
        
        #remove already occupied cells
        f <- mask(x = f, mask = r_origin, maskvalue = 1)
        
        #if no cells to colonise, next timestep
        if(cellStats(f, stat = "sum") == 0){next}
        
        #cells not available become NA
        f[f != 1] <- NA
        
        #which cells are in focal zone
        f <- sampleRandom(x = f, size = 1, na.rm = TRUE, asRaster = TRUE)
        
        f[is.na(f)] <- 0
        
        #add rasters
        r_origin <- r_origin + f
        
      }
      
      if(pl == TRUE){
        #par(mfrow=c(2,1))
        #plot(random.sp$pa.raster, main = "Habitat suitability")
        plot(r_origin, main = paste0("Dispersal simulation: timestep ", i))
        Sys.sleep(0.1)
      }
    }
    #add raster to species object
    random.sp$ras_distribution <- r_origin
    
    #convert raster to spatial points
    xy <- as.data.frame(rasterToPoints(r_origin))
    
    #retain only xy data for presences
    xy <- xy[which(xy$layer == 1),c("x", "y")]
    
    #add to species object
    random.sp$distribution <- xy
    
    #save data
    saveRDS(random.sp, paste0("./results/virtual-species/", int, "/species-", n, ".RDS"))
  }, mc.cores = detectCores()-1, mc.preschedule = FALSE, mc.cleanup = TRUE)) #for parallel processing
}
