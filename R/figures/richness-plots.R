## ---------------------------
##
## Script name: richness-plots.R
##
## Purpose of script: richness plots
##
## Author: Dr Lewis Jones
##
## Date Created: 2022-03-23
##
## Copyright (c) Lewis Jones, 2022
## Email: LewisA.Jones@outlook.com
##
#---------Load packages-----------
library(raster)
source("./R/options.R")
source("./R/functions/div-plot.R")
#---------------------------------
#generate background for plot
r <- raster(res = 1, ext = extent(ex), vals = 1)
r <- projectRaster(from = r, crs = prj, res = res)
#generate border for plotting
shp <- rasterToPolygons(x = r, dissolve = TRUE, na.rm = TRUE)
shp <- smoothr::smooth(x = shp, method = "ksmooth", smoothness = 20)
#---------------------------------
#Santonian
sant_potential <- raster("./results/virtual-species/misc/sant_potential.grd")
sant_dist <- raster("./results/virtual-species/misc/sant_div.grd")
sant_sampled <- raster("./results/virtual-species/misc/sant_div_samp.grd")
#---------------------------------
#Campanian
camp_potential <- raster("./results/virtual-species/misc/camp_potential.grd")
camp_dist <- raster("./results/virtual-species/misc/camp_div.grd")
camp_sampled <- raster("./results/virtual-species/misc/camp_div_samp.grd")
#---------------------------------
#Maastrichtian
maas_potential <- raster("./results/virtual-species/misc/maas_potential.grd")
maas_dist <- raster("./results/virtual-species/misc/maas_div.grd")
maas_sampled <- raster("./results/virtual-species/misc/maas_div_samp.grd")
#---------------------------------
jpeg("./figures/div_plots.jpg", width = 300,  height = 180, units = "mm", res = 600)
par(mfrow = c(3, 3), mar = c(0.25, 0.25, 0.25, 6))
plot(shp, col = "#f7fbff")
div_plot(x = sant_potential, title = "Santonian: Potential distribution")
plot(shp, col = "#f7fbff")
div_plot(x = sant_dist, title = "Santonian: Realised distribution")
plot(shp, col = "#f7fbff")
div_plot(x = sant_sampled, title = "Santonian: Sampled distribution")
plot(shp, col = "#f7fbff")
div_plot(x = camp_potential, title = "Campanian: Potential distribution")
plot(shp, col = "#f7fbff")
div_plot(x = camp_dist, title = "Campanian: Realised distribution")
plot(shp, col = "#f7fbff")
div_plot(x = camp_sampled, title = "Campanian: Sampled distribution")
plot(shp, col = "#f7fbff")
div_plot(x = maas_potential, title = "Maastrichtian: Potential distribution")
plot(shp, col = "#f7fbff")
div_plot(x = maas_dist, title = "Maastrichtian: Realised distribution")
plot(shp, col = "#f7fbff")
div_plot(x = maas_sampled, title = "Maastrichtian: Sampled distribution")
dev.off()
#---------------------------------





