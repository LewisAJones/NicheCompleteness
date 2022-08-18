## -----------------------------------------------------------------------------
##
## Script name: prepare-climate-data.R
##
## Purpose of script: Prepare climate data for analyses
##
## Author: Dr Lewis Jones
##
## Last update: 2022-08-17
##
# Load packages ----------------------------------------------------------------
library(raster)
library(ncdf4)
library(stringr)
source("./R/options.R")
# Data preparation -------------------------------------------------------------
# Grab months and convert to lowercase
months <- c(tolower(month.abb))
# Define raster for resampling
r <- raster(res = 1, ext = extent(ex))
# Maastrichtian ----------------------------------------------------------------
# Load mask 
msk <- raster(
  "./data/raw-data/climate/Maastrichtian/teyeo/teyeo.qrparm.mask.nc", 
  varname = "lsm")
# Get file names
files <- list.files("./data/raw-data/climate/Maastrichtian/teyeo/", 
                    full.names = TRUE)
# Extract only monthly variables
files <- files[sapply(months, function(x){str_which(files, x)})]
# Load precipitation data
precip <- stack(files, varname = "precip_mm_srf")
# Mask data
precip <- mask(x = precip, mask = msk, maskvalue = 0)
# Assign names
names(precip) <- months
# Convert from kg/m2/s to mm/day
precip <- precip * 86400
# Load temperature data
temp <- stack(files, varname = "temp_mm_srf")
# Mask data
temp <- mask(x = temp, mask = msk, maskvalue = 0)
# Assign names
names(temp) <- months
# Convert kelvin to celsius
temp <- temp - 273.15
# Calculate max and min
max_precip <- calc(x = precip, fun = max)
min_precip <- calc(x = precip, fun = min)
max_temp <- calc(x = temp, fun = max)
min_temp <- calc(x = temp, fun = min)
# Stack data
stk <- stack(max_precip, min_precip, max_temp, min_temp)
# Add names
names(stk) <- c("max_precip", "min_precip", "max_temp", "min_temp")
# Rotate data
stk <- raster::rotate(stk)
# Resample data (the extent and resolution must be updated to avoid rgdal issue)
stk <- resample(x = stk, y = r)
# Define original GCRS
crs(stk) <- gcrs
# Plot data
plot(stk)
# Save data
writeRaster(
  x = stk,
  filename = paste0("./data/climate/maas/", 
  names(stk), ".grd"), bylayer = TRUE, overwrite = TRUE)
# Campanian---------------------------------------------------------------------
# Load mask 
msk <- raster("./data/raw-data/climate/Campanian/teyeq/teyeq.qrparm.mask.nc", 
              varname = "lsm")
# Get file names
files <- list.files("./data/raw-data/climate/Campanian/teyeq/",
                    full.names = TRUE)
# Extract only monthly variables
files <- files[sapply(months, function(x){str_which(files, x)})]
# Load precipitation data
precip <- stack(files, varname = "precip_mm_srf")
# Mask data
precip <- mask(x = precip, mask = msk, maskvalue = 0)
# Assign names
names(precip) <- months
# Convert from kg/m2/s to mm/day
precip <- precip * 86400
# Load temperature data
temp <- stack(files, varname = "temp_mm_srf")
# Mask data
temp <- mask(x = temp, mask = msk, maskvalue = 0)
# Assign names
names(temp) <- months
# Convert kelvin to celsius
temp <- temp - 273.15
# Calculate max and min
max_precip <- calc(x = precip, fun = max)
min_precip <- calc(x = precip, fun = min)
max_temp <- calc(x = temp, fun = max)
min_temp <- calc(x = temp, fun = min)
# Stack data
stk <- stack(max_precip, min_precip, max_temp, min_temp)
# Add names
names(stk) <- c("max_precip", "min_precip", "max_temp", "min_temp")
# Rotate data
stk <- raster::rotate(stk)
# Resample data (the extent and resolution must be updated to avoid rgdal issue)
stk <- resample(x = stk, y = r)
# Define original GCRS
crs(stk) <- gcrs
# Plot data
plot(stk)
# Save data
writeRaster(
  x = stk,
  filename = paste0("./data/climate/camp/", 
                    names(stk), ".grd"), bylayer = TRUE, overwrite = TRUE)
# Santonian --------------------------------------------------------------------
# Load mask 
msk <- raster("./data/raw-data/climate/Santonian/teyer/teyer.qrparm.mask.nc",
              varname = "lsm")
# Get file names
files <- list.files("./data/raw-data/climate/Santonian/teyer/",
                    full.names = TRUE)
# Extract only monthly variables
files <- files[sapply(months, function(x){str_which(files, x)})]
# Load precipitation data
precip <- stack(files, varname = "precip_mm_srf")
# Mask data
precip <- mask(x = precip, mask = msk, maskvalue = 0)
# Assign names
names(precip) <- months
# Convert from kg/m2/s to mm/day
precip <- precip * 86400
# Load temperature data
temp <- stack(files, varname = "temp_mm_srf")
# Mask data
temp <- mask(x = temp, mask = msk, maskvalue = 0)
# Assign names
names(temp) <- months
# Convert kelvin to celsius
temp <- temp - 273.15
# Calculate max and min
max_precip <- calc(x = precip, fun = max)
min_precip <- calc(x = precip, fun = min)
max_temp <- calc(x = temp, fun = max)
min_temp <- calc(x = temp, fun = min)
# Stack data
stk <- stack(max_precip, min_precip, max_temp, min_temp)
# Add names
names(stk) <- c("max_precip", "min_precip", "max_temp", "min_temp")
# Rotate data
stk <- raster::rotate(stk)
# Resample data (the extent and resolution must be updated to avoid rgdal issue)
stk <- resample(x = stk, y = r)
# Define original GCRS
crs(stk) <- gcrs
# Plot data
plot(stk)
# Save data
writeRaster(
  x = stk,
  filename = paste0("./data/climate/sant/", 
                    names(stk), ".grd"), bylayer = TRUE, overwrite = TRUE)
#-------------------------------------------------------------------------------

