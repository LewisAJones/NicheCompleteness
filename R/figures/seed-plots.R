## ---------------------------
##
## Script name: seed-plots.R
##
## Purpose of script: seeds plots
##
## Author: Dr Lewis Jones
##
## Date Created: 2022-09-06
##
## Email: LewisA.Jones@outlook.com
##
#---------Load packages-----------
library(raster)
#---------------------------------
# Species files
sant_seeds <- readRDS("./results/virtual-species/seeds_sant.RDS")
camp_seeds <- readRDS("./results/virtual-species/seeds_camp.RDS")
maas_seeds <- readRDS("./results/virtual-species/seeds_maas.RDS")
#---------------------------------
jpeg("./figures/seed_plots.jpg", 
     width = 180,  height = 250, units = "mm", res = 600)
par(mfrow = c(3, 1), mar = c(2, 7, 2, 0.5))
#---------------------------------
# Santonian
r <- raster("./data/climate/sant/max_precip.grd")
r[r > 0] <- 0
cells <- unique(sant_seeds$cells)
r[cells] <- 1
plot(r, axes = FALSE, box = FALSE, legend = FALSE, 
     main = "Santonian", cex.main = 1.5)
#---------------------------------
#Campanian
r <- raster("./data/climate/camp/max_precip.grd")
r[r > 0] <- 0
cells <- unique(camp_seeds$cells)
r[cells] <- 1
plot(r, axes = FALSE, box = FALSE, legend = FALSE, 
     main = "Campanian", cex.main = 1.5)
#---------------------------------
#Maastrichtian
r <- raster("./data/climate/maas/max_precip.grd")
r[r > 0] <- 0
cells <- unique(maas_seeds$cells)
r[cells] <- 1
plot(r, axes = FALSE, box = FALSE, legend = FALSE,
     main = "Maastrichtian", cex.main = 1.5)
#---------------------------------
#Campanian
camp_potential <- raster("./results/virtual-species/richness/camp_potential.grd")
camp_dist <- raster("./results/virtual-species/richness/camp_div.grd")
camp_sampled <- raster("./results/virtual-species/richness/camp_div_samp.grd")
#---------------------------------
#Maastrichtian
maas_potential <- raster("./results/virtual-species/richness/maas_potential.grd")
maas_dist <- raster("./results/virtual-species/richness/maas_div.grd")
maas_sampled <- raster("./results/virtual-species/richness/maas_div_samp.grd")
#---------------------------------
dev.off()
#---------------------------------





