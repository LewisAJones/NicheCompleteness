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
library(MetBrewer)
library(raster)
source("./R/options.R")
source("./R/functions/div-plot.R")
r <- raster(res = 1)
#---------------------------------
#Santonian
sant <- raster("./results/virtual-species/misc/sant_div.grd")
sant_hs <- raster("./data/raw-data/climate/Santonian/teyer/teyer.qrparm.orog.nc")
sant_hs[sant_hs == 0] <- NA
#rotate data
sant_hs <- raster::rotate(sant_hs)
#resample data
sant_hs <- resample(x = sant_hs, y = r)
#define original GCRS
crs(sant_hs) <- gcrs
#project data
sant_hs <- projectRaster(from = sant_hs, crs = prj, res = res)
#---------------------------------
#Campanian
camp <- raster("./results/virtual-species/misc/camp_div.grd")
camp_hs <- raster("./data/raw-data/climate/Campanian/teyeq/teyeq.qrparm.orog.nc")
camp_hs[camp_hs == 0] <- NA
#rotate data
camp_hs <- raster::rotate(camp_hs)
#resample data
camp_hs <- resample(x = camp_hs, y = r)
#define original GCRS
crs(camp_hs) <- gcrs
#project data
camp_hs <- projectRaster(from = camp_hs, crs = prj, res = res)
#---------------------------------
#Maastrichtian
maas <- raster("./results/virtual-species/misc/maas_div.grd")
maas_hs <- raster("./data/raw-data/climate/Maastrichtian/teyeo/teyeo.qrparm.orog.nc")
maas_hs[maas_hs == 0] <- NA
#rotate data
maas_hs <- raster::rotate(maas_hs)
#resample data
maas_hs <- resample(x = maas_hs, y = r)
#define original GCRS
crs(maas_hs) <- gcrs
#project data
maas_hs <- projectRaster(from = maas_hs, crs = prj, res = res)
#---------------------------------

png("./figures/div_plots.png", width = 160,  height = 220, units = "mm", res = 600)
par(mfrow=c(3,1), mar = c(1, 1, 1, 1))
div_plot(x = sant, hs = sant_hs, title = "Santonian")
div_plot(x = camp, hs = camp_hs, title = "Campanian")
div_plot(x = maas, hs = maas_hs, title = "Maastrichtian")
dev.off()













