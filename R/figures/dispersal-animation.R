## ---------------------------
##
## Script name: dispersal-animation.R
##
## Purpose of script: generate vdispersal animation
##
## Author: Dr Lewis Jones
##
## Date Created: 2022-04-01
##
## Copyright (c) Lewis Jones, 2022
## Email: LewisA.Jones@outlook.com
##
#---------Load packages-----------
library(raster)
library(animation)
library(rgdal)
source("./R/options.R")
#-----------Animation--------------
#get climate layer as background example
r <- raster("./data/climate/sant/max_precip.grd")
#convert all values to 1
r[!is.na(r)] <- 1
#generate origin raster
r_origin <- r
#set all values in landscape to 0
r_origin[!is.na(r_origin)] <- 0
#generate random sample in origin raster
xy <- sampleRandom(x = r, size = 1, xy = TRUE)[,c("x", "y")]
r_origin[cellFromXY(object = r_origin, xy = xy)] <- 1

#dispersal types
good <- sample(x = seq(0, good_disp, 1), size = burn_in, replace = TRUE, prob = exp(-2 * seq(0, good_disp, 1)))
poor <- sample(x = seq(0, poor_disp, 1), size = burn_in, replace = TRUE, prob = exp(-2.5 * seq(0, poor_disp, 1)))

#-----------GOOD DISPERSAL CAPACITY--------------
good_origin <- r_origin
good_stk <- stack(good_origin)
#dispersal simulation stage
for(i in 1:length(good)){
  
  #next iteration if there is no exploratory search
  if(good[i] == 0){
    good_stk <- stack(good_stk, good_origin)
    next} 
  
  #generate matrix of weights for searching
  
  w <- matrix(rep(x = 1, length.out = ((good[i]*2)+1)^2), nr = (good[i]*2)+1, nc = (good[i]*2)+1)
  
  #set central value in matrix as 0 
  w[ceiling(length(w)/2)] <- 0
  
  #set up focal window
  f <- focal(x = good_origin, w = w, pad = TRUE, padValue = 0, na.rm = TRUE)
  
  #values not in focal window become NA
  f[f < 1] <- NA
  
  #mask by focal window to retain suitable cells within search area
  f <- mask(x = r, mask = f, updatevalue = 0)
  
  #add focal window and origin together
  good_origin <- good_origin + f
  
  #all values >= 1 update to 1 (specifies presence)
  good_origin[good_origin >= 1] <- 1
  #plot
  good_stk <- stack(good_stk, good_origin)
}

#-----------POOR DISPERSAL CAPACITY--------------
poor_origin <- r_origin
poor_stk <- stack(poor_origin)
#dispersal simulation stage
for(i in 1:length(poor)){
  
  #next iteration if there is no exploratory search
  if(poor[i] == 0){
    poor_stk <- stack(poor_stk, poor_origin)
    next} 
  
  #generate matrix of weights for searching
  
  w <- matrix(rep(x = 1, length.out = ((poor[i]*2)+1)^2), nr = (poor[i]*2)+1, nc = (poor[i]*2)+1)
  
  #set central value in matrix as 0 
  w[ceiling(length(w)/2)] <- 0
  
  #set up focal window
  f <- focal(x = poor_origin, w = w, pad = TRUE, padValue = 0, na.rm = TRUE)
  
  #values not in focal window become NA
  f[f < 1] <- NA
  
  #mask by focal window to retain suitable cells within search area
  f <- mask(x = r, mask = f, updatevalue = 0)
  
  #add focal window and origin together
  poor_origin <- poor_origin + f
  
  #all values >= 1 update to 1 (specifies presence)
  poor_origin[poor_origin >= 1] <- 1
  #plot
  poor_stk <- stack(poor_stk, poor_origin)
}
#-----------SAVE GIF--------------
#generate background for plot
r <- raster(res = 1, ext = extent(ex), vals = 1)
r <- projectRaster(from = r, crs = prj, res = res)
#generate border for plotting
shp <- rasterToPolygons(x = r, dissolve = TRUE, na.rm = TRUE)
shp <- smoothr::smooth(x = shp, method = "ksmooth", smoothness = 20)
#load library
library(gifski) 
#create tmp folder
dir.create("./figures/gif/")
#generate paths
png_path <- file.path("./figures/gif/", "frame%03d.png")
#generate image
png(png_path, width = 1500, height = 500, units = "px")
#plotting format
par(mfrow=c(1,2),ask = FALSE)
#for loop of plotting
for (i in 1:nlayers(good_stk)) {
  plot(r, col = "lightblue", legend = FALSE, axes = FALSE, box = FALSE)
  plot(x = good_stk[[i]],
          legend = FALSE, axes = FALSE, box = FALSE, add = TRUE)
  plot(shp, add = TRUE, lwd = 2)
  title(paste0("GOOD DISPERSAL CAPACITY \n TIME STEP ", i), line = -1)
  plot(r, col = "lightblue", legend = FALSE, axes = FALSE, box = FALSE)
  plot(x = poor_stk[[i]],
       legend = FALSE, axes = FALSE, box = FALSE, add = TRUE)
  plot(shp, add = TRUE, lwd = 2)
  title(paste0("POOR DISPERSAL CAPACITY \n TIME STEP ", i), line = -1)
  }
#end plotting
dev.off()
#get files
png_files <- sprintf(png_path, 1:nlayers(good_stk))
#generate gif
gifski(png_files, gif_file = "./figures/dispersal_cap.gif", 
       width = 1500,
       height = 500,
       delay = 0.1, progress = FALSE)
#remove folder
unlink("./figures/gif/", recursive = TRUE)
