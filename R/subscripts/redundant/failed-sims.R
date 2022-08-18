list <- paste0("species-", 1:10000)

sant <- list.files("./results/virtual-species/sant/")
sant <- tools::file_path_sans_ext(sant)
camp <- list.files("./results/virtual-species/camp/")
camp <- tools::file_path_sans_ext(camp)
maas <- list.files("./results/virtual-species/maas/")
maas <- tools::file_path_sans_ext(maas)

sant <- setdiff(x = list, y = sant)
sant <- as.numeric(gsub("species-", "", sant))
camp <- setdiff(x = list, y = camp)
camp <- as.numeric(gsub("species-", "", camp))
maas <- setdiff(x = list, y = maas)
maas <- as.numeric(gsub("species-", "", maas))
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
pl = FALSE
#use CESGA
#setwd("/mnt/netapp2/Store_uni/home/uvi/ba/ljo/NicheCompleteness/")
source("./R/options.R")
#-----------Analyses--------------
#intervals for analyses
int <- c("camp")

#get file paths
files <- list.files(paste0("./data/climate/", int, "/"), pattern = ".grd", full.names = TRUE)
  
  #stack rasters
  stk <- stack(files)
  
  invisible(pbmclapply(camp, function(n){
    
    #generate random species
    exist <- 0
    #if species cannot exist due to lack of suitability within environment, rerun prior function
    while(exist == 0){
      random.sp <- generateRandomSp(raster.stack = stk,
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
      vals <- getValues(random.sp$pa.raster)
      vals <- na.omit(vals)
      if(length(vals) > 0){exist <- 1}
    }
    
    #sample origin occurrence from random species
    random.sp$origin <- sampleOccurrences(x = random.sp, n = 1, plot = pl, replacement = TRUE)$sample.points[c("x", "y")]
    
    #generate raster of species origin
    r_origin <- random.sp$pa.raster
    r_origin[!is.na(r_origin)] <- 0
    r_origin[cellFromXY(object = r_origin, xy = random.sp$origin)] <- 1
    
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
    
    #dispersal simulation stage
    for(i in 1:burn_in){
      
      #next iteration if there is no exploratory search
      if(d[i] == 0){next} 
      
      #generate matrix of weights for searching
      
      w <- matrix(rep(x = 1, length.out = ((d[i]*2)+1)^2), nr = (d[i]*2)+1, nc = (d[i]*2)+1)
      
      #set central value in matrix as 0 
      w[ceiling(length(w)/2)] <- 0
      
      #set up focal window
      f <- focal(x = r_origin, w = w, pad = TRUE, padValue = 0, na.rm = TRUE)
      
      #values not in focal window become NA
      f[f < 1] <- NA
      
      #mask by focal window to retain suitable cells within search area
      f <- mask(x = random.sp$pa.raster, mask = f, updatevalue = 0)
      
      #remove non-suitable cells
      f[f < 1] <- NA
      
      #remove already occupied cells
      f <- mask(x = f, mask = r_origin, maskvalue = 1)
      
      #if no cells to colonise, next timestep
      if(cellStats(f, stat = "sum") == 0){next}
      
      #sample a cell for colonising
      f <- sampleRandom(x = f, size = 1, asRaster = TRUE)
      
      #replace NAs for additon
      f[is.na(f)] <- 0
      
      #add focal window and origin together
      r_origin <- r_origin + f
      
      #all values >= 1 update to 1 (specifies presence)
      r_origin[r_origin >= 1] <- 1
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
