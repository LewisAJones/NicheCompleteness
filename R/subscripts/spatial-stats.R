# Header ----------------------------------------------------------------
# Project: NicheCompleteness
# File name: spatial-stats.R
# Last updated: 2023-03-29
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/NicheCompleteness
# Load libraries --------------------------------------------------------
library(vegan)
library(raster)
# Spatial sampling extent -----------------------------------------------
extent <- data.frame()
intervals <- c("sant", "camp", "maas")
for (i in intervals) {
  pts <- readRDS(paste0("./results/sampling-window/xy_coords_", i, ".RDS"))
  pts <- pts[, c("p_lng", "p_lat")]
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
df <- cbind.data.frame(intervals, simulated, sampled, threshold, extent)
# Save data  -------------------------------------------------------------------
saveRDS(df, "./results/sampling-window/spatial-stats.RDS")
