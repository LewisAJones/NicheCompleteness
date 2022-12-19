## ----------------------------------------------------------------------------
##
## Script name: spatial-stats.R
##
## Purpose of script: calculate spatial sampling coverage and MST length
##
## Author: Dr Lewis Jones
##
## Last updated: 2022-08-23
##
# Load packages ----------------------------------------------------------------
library(raster)
library(vegan)
# Spatial sampling coverage calculation ----------------------------------------
# Load rasters
r <- stack(list.files("./results/sampling-window/", 
                      pattern = ".grd", full.names = TRUE))
# Assign names
names(r) <- c("camp", "maas", "sant")
# New object for binary sampling window
r_samp <- r
# Modify rasters for processing
r[r > 0] <- 0
r_samp[r_samp == 0] <- NA
# Calculate coverage
sant_samp <- sum(freq(r_samp$sant, useNA = 'no')[,c("count")]) / 
  sum(freq(r$sant, useNA = 'no')[,c("count")])

camp_samp <- sum(freq(r_samp$camp, useNA = 'no')[,c("count")]) / 
  sum(freq(r$camp, useNA = 'no')[,c("count")])

maas_samp <- sum(freq(r_samp$maas, useNA = 'no')[,c("count")]) / 
  sum(freq(r$maas, useNA = 'no')[,c("count")])

# Bind data
coverage <- data.frame(coverage = c(sant_samp, camp_samp, maas_samp))
# Convert to percentages
coverage <- coverage * 100
# Round data
coverage <- round(coverage, digits = 2)

# Spatial sampling extent ------------------------------------------------------
# Load rasters
r <- stack(list.files("./results/sampling-window/", 
                      pattern = ".grd", full.names = TRUE))
# Assign names
names(r) <- c("camp", "maas", "sant")
# Set non-sampled cells to NA
r[r == 0] <- NA
# Convert to points
# Empty dataframe
extent <- data.frame()
intervals <- c("sant", "camp", "maas")
for (i in intervals) {
  pts <- rasterToPoints(r[[i]])[,c("x", "y")]
  # Calculate point distances
  gcdists <- pointDistance(pts, lonlat = TRUE)
  gcdists <- as.dist(gcdists)
  # Calculate minimum spanning tree
  mst_sp <- vegan::spantree(gcdists)
  # Convert to km
  MST <- sum(mst_sp$dist) / 1000
  # Round off data
  tmp <- round(MST, digits = 0)
  # Bind data
  extent <- rbind.data.frame(extent, tmp)
}
# Update column name
colnames(extent) <- c("extent")
# Bind data --------------------------------------------------------------------
intervals <- c("Santonian", "Campanian", "Maastrichtian")
# Count number of species simulated
simulated <- c(length(list.files("./results/virtual-species/sant/")),
               length(list.files("./results/virtual-species/camp/")),
               length(list.files("./results/virtual-species/maas/")))
# Count number of species sampled
sampled <- c(length(readRDS("./results/virtual-species/sampled/sant.RDS")),
             length(readRDS("./results/virtual-species/sampled/camp.RDS")),
             length(readRDS("./results/virtual-species/sampled/maas.RDS")))
# Count number of species passing the threshold (n >= 5)
threshold <- c(nrow(readRDS("./results/ecospat/sant.RDS"))/3,
               nrow(readRDS("./results/ecospat/camp.RDS"))/3,
               nrow(readRDS("./results/ecospat/maas.RDS"))/3)
df <- cbind.data.frame(intervals, simulated, sampled, threshold, coverage, extent)
# Save data  -------------------------------------------------------------------
saveRDS(df, "./results/sampling-window/spatial-stats.RDS")
