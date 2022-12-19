## ----------------------------------------------------------------------##
##
## Script name: prepare-climate-data.R
##
## Purpose of script: Prepare climate data
##
## Author: Dr Lewis Jones
##
## Last update: 2022-12-17
##
## ----------------------------------------------------------------------##
# Load packages -----------------------------------------------------------
library(raster)
library(ncdf4)
library(stringr)
library(geosphere)
source("./R/options.R")
# Create directory for data
dir.create("./data/climate/", showWarnings = FALSE)
# Data preparation --------------------------------------------------------
# Grab months and convert to lowercase
months <- c(tolower(month.abb))
# Generate raster template
r <- raster(res = params$res, ext = extent(params$ex), crs = params$crs)
# Run for loop
for (i in params$stage) {
  # Build local dir
  dir <- paste0("./data/raw-data/climate/", i, "/")
  # Load mask
  msk <- raster(paste0(dir, "/mask.nc"), varname = "lsm")
  # Climate file names
  files <- list.files(dir, full.names = TRUE)
  # Retain monthly climate data
  files <- files[sapply(months, function(x){str_which(files, x)})]
  # Load precipitation data
  precip <- stack(files, varname = "precip_mm_srf")
  # Assign names
  names(precip) <- months
  # Mask data
  precip <- mask(x = precip, mask = msk, maskvalue = 0)
  # Convert from kg/m2/s to mm per month
  precip <- (precip * 86400) * 30
  # Load temperature data
  temp <- stack(files, varname = "temp_mm_srf")
  # Assign names
  names(temp) <- months
  # Mask data
  temp <- mask(x = temp, mask = msk, maskvalue = 0)
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
  # Resample data
  stk <- resample(x = stk, y = r)
  # Plot data
  # plot(stk)
  # Create output directory
  out_dir <- paste0("./data/climate/", i, "/")
  dir.create(out_dir)
  # Save data
  writeRaster(
    x = stk,
    filename = paste0(out_dir, 
                      names(stk), ".grd"), bylayer = TRUE, overwrite = TRUE)
}
# Finish ------------------------------------------------------------------
beepr::beep(sound = 4)
