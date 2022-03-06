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
source("./R/options.R")
#-----------Analyses--------------
#get file paths
files <- list.files("./data/raw-data/wc2/", full.names = TRUE)

#stack rasters
stk <- stack(files)

#resample data to desired resolution
stk <- resample(x = stk, y = raster(res = res))

#rename layers
names(stk) <- c("bio_13", "bio_14", "bio_5", "bio_6")

#generate random species
random.sp <- generateRandomSp(raster.stack = stk,
                              approach = "response",
                              relations = "gaussian",
                              realistic.sp = TRUE,
                              species.type = "multiplicative",
                              rescale.each.response = TRUE,
                              convert.to.PA = TRUE,
                              PA.method = "threshold",
                              beta = 0.5
                              )

#sample origin occurrence from random species
random.sp$origin <- sampleOccurrences(x = random.sp, n = 1)$sample.points[c("x", "y")]
#generate raster of species origin
r_origin <- random.sp$pa.raster
r_origin[!is.na(r_origin)] <- 0
r_origin[cellFromXY(object = r_origin, xy = random.sp$origin)] <- 1

#dispersal capacity (randomly sample from good vs poor dispersal)
random.sp$dispersal_cap <- sample(x = disp_cap, size = 1)

#exploration stage (sample from potential options)
#generate d for good dispersal capacity
if(random.sp$dispersal_cap == "good_disp"){
  d <- sample(x = seq(0, good_disp, 1), size = burn_in, replace = TRUE, prob = dgeom(x = seq(0, good_disp, 1), prob = 0.8))
}
#generate d for poor dispersal capacity
if(random.sp$dispersal_cap == "poor_disp"){
  d <- sample(x = seq(0, poor_disp, 1), size = burn_in, replace = TRUE, prob = dgeom(x = seq(0, poor_disp, 1), prob = 0.8))
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
  f <- focal(x = r_origin, w = w, pad = TRUE, padValue = 0)
  #values not in focal window become NA
  f[f < 1] <- NA
  #mask by focal window to retain suitable cells within search area
  f <- mask(x = random.sp$pa.raster, mask = f, updatevalue = 0)
  #add focal window and origin together
  r_origin <- r_origin + f
  #all values >= 1 update to 1 (specifies presence)
  r_origin[r_origin >= 1] <- 1
  #plot
  plot(r_origin, main = paste0("Time step ", i))
  Sys.sleep(0.1)
}

r_origin <- as.data.frame(rasterToPoints(r_origin))
r_origin <- r_origin[which(r_origin$layer == 1),]
