## ---------------------------
##
## Script name: sampling-window.R
##
## Purpose of script: sampling window plot
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
#---------------------------------
#sampling windows
sant_samp <- raster("./results/sampling-window/sampling_raster_sant.grd")
sant_samp[sant_samp > 0] <- 1
camp_samp <- raster("./results/sampling-window/sampling_raster_camp.grd")
camp_samp[camp_samp > 0] <- 1
maas_samp <- raster("./results/sampling-window/sampling_raster_maas.grd")
maas_samp[maas_samp > 0] <- 1
#---------------------------------
#generate background for plot
r <- raster(res = 1, ext = extent(ex), vals = 1)
r <- projectRaster(from = r, crs = prj, res = res)
#generate border for plotting
shp <- rasterToPolygons(x = r, dissolve = TRUE, na.rm = TRUE)
shp <- smoothr::smooth(x = shp, method = "ksmooth", smoothness = 20)
#---------------------------------
#plot  
jpeg("./figures/sampling_plots.jpg", width = 180,  height = 270, units = "mm", res = 300)
par(mfrow=c(3,1), mar = c(1, 1, 1, 1))
plot(shp, col = "#f7fbff", lwd = 2)
plot(sant_samp,
     add = TRUE,
     main = "Santonian",
     col = c("grey90", "forestgreen"),
     box = FALSE,
     axes = FALSE,
     legend = FALSE,
     legend.width = 0,
     legend.shrink = 0,
     legend.mar = 0,
     interpolate = FALSE)
legend("bottomright",
       legend = c("Sampled", "Not sampled"),
       fill = c("forestgreen", "grey90"),
       border = "black",
       bty = "n",
       cex = 1.5)
plot(shp, col = NA, lwd = 2, add = TRUE)
title("Santonian", line = -2, adj = 0, cex.main = 2)

plot(shp, col = "#f7fbff")
plot(camp_samp,
     add = TRUE,
     main = "Campanian",
     col = c("grey90", "forestgreen"),
     box = FALSE,
     axes = FALSE,
     legend = FALSE,
     legend.width = 0,
     legend.shrink = 0,
     legend.mar = 0,
     interpolate = FALSE)
legend("bottomright",
       legend = c("Sampled", "Not sampled"),
       fill = c("forestgreen", "grey90"),
       border = "black",
       bty = "n",
       cex = 1.5)
plot(shp, col = NA, lwd = 2, add = TRUE)
title("Campanian", line = -2, adj = 0, cex.main = 2)

plot(shp, col = "#f7fbff")
plot(maas_samp,
     add = TRUE,
     main = "Maastrichtian",
     col = c("grey90", "forestgreen"),
     box = FALSE,
     axes = FALSE,
     legend = FALSE,
     legend.width = 0,
     legend.shrink = 0,
     legend.mar = 0,
     interpolate = FALSE)
legend("bottomright",
       legend = c("Sampled", "Not sampled"),
       fill = c("forestgreen", "grey90"),
       border = "black",
       bty = "n",
       cex = 1.5)
plot(shp, col = NA, lwd = 2, add = TRUE)
title("Maastrichtian", line = -2, adj = 0, cex.main = 2)
dev.off() 

