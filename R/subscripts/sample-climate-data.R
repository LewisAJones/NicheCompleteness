## -----------------------------------------------------------------------------
##
## Script name: sample-climate-data.R
##
## Purpose of script: sample available climate data
##
## Author: Dr Lewis Jones
##
## Lasted updated: 2022-08-23
##
# Load packages ----------------------------------------------------------------
library(raster)
# Santonian --------------------------------------------------------------------
# Stack climate rasters
sant <- stack(list.files(paste0("./data/climate/sant/"), 
                         pattern = ".grd", full.names = TRUE))
# Load sampling window
sant_samp <- raster("./results/sampling-window/sampling_raster_sant.grd")
# Set non-sampled values to NA for masking
sant_samp[sant_samp == 0] <- NA
# Extract sampled values
sant_samp <- mask(x = sant, mask = sant_samp)
# Remove sampled values (for plotting)
sant <- mask(x = sant, mask = sant_samp, inverse = TRUE)
# Convert to dataframes
sant <- as.data.frame(x = sant, xy = TRUE, na.rm = TRUE)
sant_samp <- as.data.frame(x = sant_samp, xy = TRUE, na.rm = TRUE)
# Add sampled status
sant$sampled <- "Available"
sant_samp$sampled <- "Sampled"
# Bind data
sant <- rbind.data.frame(sant, sant_samp)
# Add interval
sant$interval <- "Santonian"
# Campanian --------------------------------------------------------------------
# Stack climate rasters
camp <- stack(list.files(paste0("./data/climate/camp/"), 
                         pattern = ".grd", full.names = TRUE))
# Load sampling window
camp_samp <- raster("./results/sampling-window/sampling_raster_camp.grd")
# Set non-sampled values to NA for masking
camp_samp[camp_samp == 0] <- NA
# Extract sampled values
camp_samp <- mask(x = camp, mask = camp_samp)
# Remove sampled values (for plotting)
camp <- mask(x = camp, mask = camp_samp, inverse = TRUE)
# Convert to dataframes
camp <- as.data.frame(x = camp, xy = TRUE, na.rm = TRUE)
camp_samp <- as.data.frame(x = camp_samp, xy = TRUE, na.rm = TRUE)
# Add sampled status
camp$sampled <- "Available"
camp_samp$sampled <- "Sampled"
# Bind data
camp <- rbind.data.frame(camp, camp_samp)
# Add interval
camp$interval <- "Campanian"
# Maastrichtian ----------------------------------------------------------------
# Stack climate rasters
maas <- stack(list.files(paste0("./data/climate/maas/"), 
                         pattern = ".grd", full.names = TRUE))
# Load sampling window
maas_samp <- raster("./results/sampling-window/sampling_raster_maas.grd")
# Set non-sampled values to NA for masking
maas_samp[maas_samp == 0] <- NA
# Extract sampled values
maas_samp <- mask(x = maas, mask = maas_samp)
# Remove sampled values (for plotting)
maas <- mask(x = maas, mask = maas_samp, inverse = TRUE)
# Convert to dataframes
maas <- as.data.frame(x = maas, xy = TRUE, na.rm = TRUE)
maas_samp <- as.data.frame(x = maas_samp, xy = TRUE, na.rm = TRUE)
# Add sampled status
maas$sampled <- "Available"
maas_samp$sampled <- "Sampled"
# Bind data
maas <- rbind.data.frame(maas, maas_samp)
# Add interval
maas$interval <- "Maastrichtian"

#bind data
df <- rbind.data.frame(sant, camp, maas)
df$interval = factor(df$interval, 
                     levels=c("Santonian", "Campanian", "Maastrichtian"))
# Save -------------------------------------------------------------------------
dir.create("./results/climate", showWarnings = FALSE)
saveRDS(df, "./results/climate/sampled-climate.RDS")
