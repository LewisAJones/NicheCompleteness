## ---------------------------
##
## Script name: climate-samp-plot.R
##
## Purpose of script: plot climate sampled
##
## Author: Dr Lewis Jones
##
## Date Created: 2023-04-26
##
## Copyright (c) Lewis Jones, 2023
## Email: LewisA.Jones@outlook.com
##
#---------Load packages-----------
library(MetBrewer)
library(raster)
library(gridExtra)
#cols for plotting
cols <- met.brewer(name = "Hiroshige", n = 5, type = "discrete")
#---------------------------------
#stack rasters
sant <- stack(list.files(paste0("./data/climate/sant/"), pattern = ".grd", full.names = TRUE))
  sant_samp <- raster("./results/sampling-window/sampling_raster_sant.grd")
  sant_samp <- mask(x = sant, mask = sant_samp)
  sant <- as.data.frame(x = sant, xy = TRUE, na.rm = TRUE)
  sant_samp <- as.data.frame(x = sant_samp, xy = TRUE, na.rm = TRUE)
camp <- stack(list.files(paste0("./data/climate/camp/"), pattern = ".grd", full.names = TRUE))
  camp_samp <- raster("./results/sampling-window/sampling_raster_camp.grd")
  camp_samp <- mask(x = camp, mask = camp_samp)
  camp <- as.data.frame(x = camp, xy = TRUE, na.rm = TRUE)
  camp_samp <- as.data.frame(x = camp_samp, xy = TRUE, na.rm = TRUE)
maas <- stack(list.files(paste0("./data/climate/maas/"), pattern = ".grd", full.names = TRUE))
  maas_samp <- raster("./results/sampling-window/sampling_raster_maas.grd")
  maas_samp <- mask(x = maas, mask = maas_samp)
  maas <- as.data.frame(x = maas, xy = TRUE, na.rm = TRUE)
  maas_samp <- as.data.frame(x = maas_samp, xy = TRUE, na.rm = TRUE)
#---------Plot-----------
jpeg("./figures/climate_sampling_plots.jpg", width = 150,  height = 200, units = "mm", res = 300)
par(mfrow=c(3,2), mar=c(4.5,4.5,1.5,1.5))
#---------Santonian-----------
#Temperature
plot(sant$min_temp, sant$max_temp, 
     main = "Santonian",
     xlab = "",
     ylab = expression(bold("Maximum temperature (ºC)")),
     pch = 20,
     cex = 0.75,
     cex.lab = 1.25,
     col = cols[2])
points(sant_samp$min_temp, sant_samp$max_temp, pch = 20, col = scales::alpha(cols[5], 1), cex = 0.75)
points(mean(sant$min_temp),mean(sant$max_temp), pch = 23, col = "black", bg = cols[2], cex = 1)
points(mean(sant_samp$min_temp),mean(sant_samp$max_temp), pch = 23, col = "black", bg = cols[5], cex = 1)
legend("bottomright", legend=c("Available", "Sampled"),
       col=c(cols[2], cols[5]), pch = 20, cex = 0.8)
#Precipitation
plot(sant$min_precip, sant$max_precip, 
     main = "Santonian",
     xlab = "",
     ylab = "",
     pch = 20,
     cex = 0.75,
     cex.lab = 1.25,
     col = cols[2])
points(sant_samp$min_precip, sant_samp$max_precip, pch = 20, col = scales::alpha(cols[5], 1), cex = 0.75)
points(mean(sant$min_precip),mean(sant$max_precip), pch = 23, col = "black", bg = cols[2], cex = 1)
points(mean(sant_samp$min_precip),mean(sant_samp$max_precip), pch = 23, col = "black", bg = cols[5], cex = 1)
legend("bottomright", legend=c("Available", "Sampled"),
       col=c(cols[2], cols[5]), pch = 20, cex = 0.8)
#---------Campanian-----------
#Temperature
plot(camp$min_temp, camp$max_temp, 
     main = "Campanian",
     xlab = "",
     ylab = expression(bold("Maximum temperature (ºC)")),
     pch = 20,
     cex = 0.75,
     cex.lab = 1.25,
     col = cols[2])
points(camp_samp$min_temp, camp_samp$max_temp, pch = 20, col = scales::alpha(cols[5], 1), cex = 0.75)
points(mean(camp$min_temp),mean(camp$max_temp), pch = 23, col = "black", bg = cols[2], cex = 1)
points(mean(camp_samp$min_temp),mean(camp_samp$max_temp), pch = 23, col = "black", bg = cols[5], cex = 1)
legend("bottomright", legend=c("Available", "Sampled"),
       col=c(cols[2], cols[5]), pch = 20, cex = 0.8)
#Precipitation
plot(camp$min_precip, camp$max_precip, 
     main = "Campanian",
     xlab = "",
     ylab = "",
     pch = 20,
     cex = 0.75,
     cex.lab = 1.25,
     col = cols[2])
points(camp_samp$min_precip, camp_samp$max_precip, pch = 20, col = scales::alpha(cols[5], 1), cex = 0.75)
points(mean(camp$min_precip),mean(camp$max_precip), pch = 23, col = "black", bg = cols[2], cex = 1)
points(mean(camp_samp$min_precip),mean(camp_samp$max_precip), pch = 23, col = "black", bg = cols[5], cex = 1)
legend("bottomright", legend=c("Available", "Sampled"),
       col=c(cols[2], cols[5]), pch = 20, cex = 0.8)
#---------Maastrichtian-----------
#Temperature
plot(maas$min_temp, maas$max_temp, 
     main = "Maastrichtian",
     xlab = expression(bold("Minimum temperature (ºC)")),
     ylab = expression(bold("Maximum temperature (ºC)")),
     pch = 20,
     cex = 0.75,
     cex.lab = 1.25,
     col = cols[2])
points(maas_samp$min_temp, maas_samp$max_temp, pch = 20, col = scales::alpha(cols[5], 1), cex = 0.75)
points(mean(maas$min_temp),mean(maas$max_temp), pch = 23, col = "black", bg = cols[2], cex = 1)
points(mean(maas_samp$min_temp),mean(maas_samp$max_temp), pch = 23, col = "black", bg = cols[5], cex = 1)
legend("bottomright", legend=c("Available", "Sampled"),
       col=c(cols[2], cols[5]), pch = 20, cex = 0.8)
#Precipitation
plot(maas$min_precip, maas$max_precip, 
     main = "Maastrichtian",
     xlab = expression(bold("Minimum precipitation (mm/day)")),
     ylab = "",
     pch = 20,
     cex = 0.75,
     cex.lab = 1.25,
     col = cols[2])
points(maas_samp$min_precip, maas_samp$max_precip, pch = 20, col = scales::alpha(cols[5], 1), cex = 0.75)
points(mean(maas$min_precip),mean(maas$max_precip), pch = 23, col = "black", bg = cols[2], cex = 1)
points(mean(maas_samp$min_precip),mean(maas_samp$max_precip), pch = 23, col = "black", bg = cols[5], cex = 1)
legend("bottomright", legend=c("Available", "Sampled"),
       col=c(cols[2], cols[5]), pch = 20, cex = 0.8)

dev.off()
