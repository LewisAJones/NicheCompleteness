## ----------------------------------------------------------------------##
##
## Script name: prepare-climate-data-calculate-dist.R
##
## Purpose of script: Prepare climate data and calculate distances for 
## simulations
##
## Author: Dr Lewis Jones
##
## Last update: 2022-12-16
##
## ----------------------------------------------------------------------##
# Load packages -----------------------------------------------------------
library(raster)
library(ncdf4)
library(stringr)
library(geosphere)
source("./R/options.R")
# Data preparation --------------------------------------------------------
# Grab months and convert to lowercase
months <- c(tolower(month.abb))
# Define raster for resampling
r <- raster(res = params$rsm, ext = extent(params$ex))
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
  # Project data
  stk <- projectRaster(from = stk, res = params$res, crs = params$crs)
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

# Calculate distance matrix -----------------------------------------------
  # Convert a layer to spdf
  spdf <- rasterToPoints(stk$max_precip, spatial = TRUE)
  spdf@data$cells <- cellFromXY(object = stk$max_precip, spdf)
  spdf_t <- spTransform(x = spdf,
                        CRSobj = CRS("+proj=longlat +datum=WGS84"))
  spdf <- data.frame(cells = spdf@data$cells,
                     x = spdf@coords[, 1],
                     y = spdf@coords[, 2],
                     lng = spdf_t@coords[, 1],
                     lat = spdf_t@coords[, 2])
  # Calculate distances
  dist_m <- geosphere::distm(x = spdf[, c("lng", "lat")], fun = distHaversine)
  rownames(dist_m) <- spdf$cells
  colnames(dist_m) <- spdf$cells
  # Save distances
  out_dir <- paste0("./data/distances/", i, "/")
  dir.create(out_dir)
  saveRDS(object = dist_m, file = paste0(out_dir, "dist.RDS"), compress = "xz")
}
# Finish ------------------------------------------------------------------
beepr::beep(sound = 4)
