## ---------------------------
##
## Script name: prepare-climate-data.R
##
## Purpose of script: Prepare climate data for analyses
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
library(ncdf4)
library(stringr)
source("./R/options.R")
#-----------Data prep-------------
#get months in lower case for data loading
months <- c(tolower(month.abb))
#define raster for resampling
r <- raster(res = 1, ext = extent(ex))
#---------Maastrichtian-----------
#load mask 
msk <- raster("./data/raw-data/climate/Maastrichtian/teyeo/teyeo.qrparm.mask.nc", varname = "lsm")
#get file names
files <- list.files("./data/raw-data/climate/Maastrichtian/teyeo/", full.names = TRUE)
#extract only monthly variables
files <- files[sapply(months, function(x){str_which(files, x)})]
#load precipitation data
precip <- stack(files, varname = "precip_mm_srf")
#mask data
precip <- mask(x = precip, mask = msk, maskvalue = 0)
#assign names
names(precip) <- months
#convert from kg/m2/s to mm/day
precip <- precip * 86400
#load temperature data
temp <- stack(files, varname = "temp_mm_srf")
#mask data
temp <- mask(x = temp, mask = msk, maskvalue = 0)
#assign names
names(temp) <- months
#convert kelvin to celsius
temp <- temp - 273.15
#calculate max and minimums
max_precip <- calc(x = precip, fun = max)
min_precip <- calc(x = precip, fun = min)
max_temp <- calc(x = temp, fun = max)
min_temp <- calc(x = temp, fun = min)
#stack data
stk <- stack(max_precip, min_precip, max_temp, min_temp)
#add names
names(stk) <- c("max_precip", "min_precip", "max_temp", "min_temp")
#rotate data
stk <- raster::rotate(stk)
#resample data
stk <- resample(x = stk, y = r)
#define original GCRS
crs(stk) <- gcrs
#project data
stk <- projectRaster(from = stk, crs = prj, res = res)
plot(stk)
#save data
writeRaster(x = stk, filename = paste0("./data/climate/maas/", names(stk), ".grd"), bylayer = TRUE, overwrite = TRUE)
#---------Campanian-----------
#load mask 
msk <- raster("./data/raw-data/climate/Campanian/teyeq/teyeq.qrparm.mask.nc", varname = "lsm")
#get file names
files <- list.files("./data/raw-data/climate/Campanian/teyeq/", full.names = TRUE)
#extract only monthly variables
files <- files[sapply(months, function(x){str_which(files, x)})]
#load precipitation data
precip <- stack(files, varname = "precip_mm_srf")
#mask data
precip <- mask(x = precip, mask = msk, maskvalue = 0)
#assign names
names(precip) <- months
#convert from kg/m2/s to mm/day
precip <- precip * 86400
#load temperature data
temp <- stack(files, varname = "temp_mm_srf")
#mask data
temp <- mask(x = temp, mask = msk, maskvalue = 0)
#assign names
names(temp) <- months
#convert kelvin to celsius
temp <- temp - 273.15
#calculate max and minimums
max_precip <- calc(x = precip, fun = max)
min_precip <- calc(x = precip, fun = min)
max_temp <- calc(x = temp, fun = max)
min_temp <- calc(x = temp, fun = min)
#stack data
stk <- stack(max_precip, min_precip, max_temp, min_temp)
#add names
names(stk) <- c("max_precip", "min_precip", "max_temp", "min_temp")
#rotate data
stk <- raster::rotate(stk)
#resample data
stk <- resample(x = stk, y = r)
#define original GCRS
crs(stk) <- gcrs
#project data
stk <- projectRaster(from = stk, crs = prj, res = res)
plot(stk)
#save data
writeRaster(x = stk, filename = paste0("./data/climate/camp/", names(stk), ".grd"), bylayer = TRUE, overwrite = TRUE)
#---------Santonian-----------
#load mask 
msk <- raster("./data/raw-data/climate/Santonian/teyer/teyer.qrparm.mask.nc", varname = "lsm")
#get file names
files <- list.files("./data/raw-data/climate/Santonian/teyer/", full.names = TRUE)
#extract only monthly variables
files <- files[sapply(months, function(x){str_which(files, x)})]
#load precipitation data
precip <- stack(files, varname = "precip_mm_srf")
#mask data
precip <- mask(x = precip, mask = msk, maskvalue = 0)
#assign names
names(precip) <- months
#convert from kg/m2/s to mm/day
precip <- precip * 86400
#load temperature data
temp <- stack(files, varname = "temp_mm_srf")
#mask data
temp <- mask(x = temp, mask = msk, maskvalue = 0)
#assign names
names(temp) <- months
#convert kelvin to celsius
temp <- temp - 273.15
#calculate max and minimums
max_precip <- calc(x = precip, fun = max)
min_precip <- calc(x = precip, fun = min)
max_temp <- calc(x = temp, fun = max)
min_temp <- calc(x = temp, fun = min)
#stack data
stk <- stack(max_precip, min_precip, max_temp, min_temp)
#add names
names(stk) <- c("max_precip", "min_precip", "max_temp", "min_temp")
#rotate data
stk <- raster::rotate(stk)
#resample data
stk <- resample(x = stk, y = r)
#define original GCRS
crs(stk) <- gcrs
#project data
stk <- projectRaster(from = stk, crs = prj, res = res)
plot(stk)
#save data
writeRaster(x = stk, filename = paste0("./data/climate/sant/", names(stk), ".grd"), bylayer = TRUE, overwrite = TRUE)
#--------------------

