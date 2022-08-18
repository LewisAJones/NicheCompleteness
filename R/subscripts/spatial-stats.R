## ----------------------------------------------------------------------------
##
## Script name: spatial-stats.R
##
## Purpose of script: calculate spatial sampling coverage and MST length
##
## Author: Dr Lewis Jones
##
## Last updated: 2022-08-18
##
# Load packages ----------------------------------------------------------------
library(vegan)
# ------------------------------------------------------------------------------
# Spatial sampling coverage calculation
sant <- raster("./results/sampling-window/sampling_raster_sant.grd")
sant_samp <- sant
sant[sant > 0] <- 0
sant_samp[sant_samp == 0] <- NA
sant_samp <- sum(
  freq(sant_samp, useNA = 'no')[,c("count")]) / 
  sum(freq(sant, useNA = 'no')[,c("count")])

camp <- raster("./results/sampling-window/sampling_raster_camp.grd")
camp_samp <- camp
camp[camp > 0] <- 0
camp_samp[camp_samp == 0] <- NA
camp_samp <- sum(
  freq(camp_samp, useNA = 'no')[,c("count")]) / 
  sum(freq(camp, useNA = 'no')[,c("count")])

maas <- raster("./results/sampling-window/sampling_raster_maas.grd")
maas_samp <- maas
maas[maas > 0] <- 0
maas_samp[maas_samp == 0] <- NA
maas_samp <- sum(
  freq(maas_samp, useNA = 'no')[,c("count")]) / 
  sum(freq(maas, useNA = 'no')[,c("count")])

#bind data
SC <- rbind.data.frame(sant_samp, camp_samp, maas_samp)
#conver to percentages
SC <- SC * 100
#round data
SC <- round(SC, digits = 2)

#spatial clustering
r <- raster("./results/sampling-window/sampling_raster_sant.grd")
r[r == 0] <- NA
r <- rasterToPoints(r)[,c("x", "y")]
gcdists <- pointDistance(r, lonlat = FALSE)
mst_sp <- vegan::spantree(gcdists)
MST <- sum(mst_sp$dist)/1000
sant <- round(MST, digits = 0)

r <- raster("./results/sampling-window/sampling_raster_camp.grd")
r[r == 0] <- NA
r <- rasterToPoints(r)[,c("x", "y")]
gcdists <- pointDistance(r, lonlat = FALSE)
mst_sp <- vegan::spantree(gcdists)
MST <- sum(mst_sp$dist)/1000
camp <- round(MST, digits = 0)

r <- raster("./results/sampling-window/sampling_raster_maas.grd")
r[r == 0] <- NA
r <- rasterToPoints(r)[,c("x", "y")]
gcdists <- pointDistance(r, lonlat = FALSE)
mst_sp <- vegan::spantree(gcdists)
MST <- sum(mst_sp$dist)/1000
maas <- round(MST, digits = 0)