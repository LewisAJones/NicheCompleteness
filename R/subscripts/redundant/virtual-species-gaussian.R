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
source("./R/options.R")
#plot outputs
plt = FALSE
#-----------Analyses--------------
#intervals for analyses
intervals <- c("sant", "camp", "maas")
#run for loop across intervals
for(int in intervals){
  
  #get file paths
  files <- list.files(paste0("./data/climate/", int, "/"), pattern = ".grd", full.names = TRUE)

  #stack rasters
  stk <- stack(files)
  
  #define original CRS
  crs(stk) <- prj

  virtual_species <- pbmclapply(1:n_species, function(n){
    
    #sample niche options
    niche_type <- sample(x = niche_opt, size = 1, replace = TRUE)
    
    #assign values (SD) for defining response function
    if(niche_type == "broad"){niche <- broad}
    if(niche_type == "narrow"){niche <- narrow}
    
    #randomly sample cell from rasters to define 'optimal suitability value'
    opt_vals <- sampleRandom(x = stk, size = 1, xy = TRUE, df = TRUE)
    #get origin
    origin <- data.frame(matrix(opt_vals[,c("x", "y")], ncol = 2))
    colnames(origin) <- c("x", "y")
    
    #define response function following a gausian distribution  
    params <- formatFunctions(max_precip = c(fun = 'dnorm',
                                               mean = opt_vals[colnames(opt_vals) == "max_precip"],
                                               sd = as.numeric(niche[names(niche) == "precip"])),
                                max_temp = c(fun = 'dnorm',
                                             mean = opt_vals[colnames(opt_vals) == "max_temp"],
                                             sd = as.numeric(niche[names(niche) == "temp"])),
                                min_precip = c(fun = 'dnorm',
                                               mean = opt_vals[colnames(opt_vals) == "min_precip"],
                                               sd = as.numeric(niche[names(niche) == "precip"])),
                                min_temp = c(fun = 'dnorm',
                                             mean = opt_vals[colnames(opt_vals) == "min_temp"],
                                             sd = as.numeric(niche[names(niche) == "temp"])))
      
    #generate species from defined function
    random.sp <- generateSpFromFun(raster.stack = stk,
                                     parameters = params,
                                     species.type = "multiplicative",
                                     rescale.each.response = TRUE,
                                     plot = plt)
    
    #assign niche type
    random.sp$niche_type <- niche_type
    
    #convert to presence/absence  
    random.sp$PA.conversion <- convertToPA(random.sp,
                                             PA.method = "threshold",
                                             beta = 0.5,
                                             plot = plt)
    
    random.sp$pa.raster <- random.sp$PA.conversion$pa.raster
      
    #sample origin occurrence from random species
    random.sp$origin <- origin
    
    #generate raster of species origin
    r_origin <- random.sp$pa.raster
    r_origin[!is.na(r_origin)] <- 0
    r_origin[cellFromXY(object = r_origin, xy = random.sp$origin)] <- 1
    
    #dispersal capacity (randomly sample from good vs poor dispersal)
    random.sp$dispersal_cap <- sample(x = disp_cap, size = 1, replace = TRUE)
    
    #exploration stage (sample from potential options)
    #generate d for good dispersal capacity
    if(random.sp$dispersal_cap == "good_disp"){
      d <- sample(x = seq(0, good_disp, 1), size = burn_in, replace = TRUE, prob = dexp(x = seq(0, good_disp, 1), rate = 1))
    }
    #generate d for poor dispersal capacity
    if(random.sp$dispersal_cap == "poor_disp"){
      d <- sample(x = seq(0, poor_disp, 1), size = burn_in, replace = TRUE, prob = dexp(x = seq(0, poor_disp, 1), rate = 1.5))
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
  }, mc.cores = detectCores()-1, mc.preschedule = FALSE, mc.cleanup = TRUE) #for parallel processing
}
