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
#---------------------------------
#sampling windows
sant_samp <- raster("./results/sampling-window/sampling_raster_sant.grd")
  sant_samp[is.na(sant_samp)] <- 0
  sant_samp[sant_samp > 0] <- 1
  sant_mask <- raster("./data/climate/sant/max_precip.grd")
  sant_mask[sant_mask > 0] <- 0
  sant_samp <- sant_mask + sant_samp
camp_samp <- raster("./results/sampling-window/sampling_raster_camp.grd")
  camp_samp[is.na(camp_samp)] <- 0
  camp_samp[camp_samp > 0] <- 1
  camp_mask <- raster("./data/climate/camp/max_precip.grd")
  camp_mask[camp_mask > 0] <- 0
  camp_samp <- camp_mask + camp_samp
maas_samp <- raster("./results/sampling-window/sampling_raster_maas.grd")
  maas_samp[is.na(maas_samp)] <- 0
  maas_samp[maas_samp > 0] <- 1
  maas_mask <- raster("./data/climate/maas/max_precip.grd")
  maas_mask[maas_mask > 0] <- 0
  maas_samp <- maas_mask + maas_samp
  
png("./figures/sampling_plots.png", width = 150,  height = 220, units = "mm", res = 600)
par(mfrow=c(3,1), mar = c(1.5, 1.5, 1.5, 1.5))
plot(sant_samp,
     main = "Santonian",
     col = c("grey90", "forestgreen"),
     box = FALSE,
     axes = FALSE,
     legend = FALSE,
     legend.width = 0,
     legend.shrink = 0,
     legend.mar = 0,
     interpolate = TRUE)
legend("right",
       legend = c("Sampled", "Not sampled"),
       fill = c("forestgreen", "grey90"),
       border = "black",
       bty = "n")
plot(camp_samp,
     main = "Campanian",
     col = c("grey90", "forestgreen"),
     box = FALSE,
     axes = FALSE,
     legend = FALSE,
     legend.width = 0,
     legend.shrink = 0,
     legend.mar = 0,
     interpolate = TRUE)
legend("right",
       legend = c("Sampled", "Not sampled"),
       fill = c("forestgreen", "grey90"),
       border = "black",
       bty = "n")
plot(maas_samp,
     main = "Maastrichtian",
     col = c("grey90", "forestgreen"),
     box = FALSE,
     axes = FALSE,
     legend = FALSE,
     legend.width = 0,
     legend.shrink = 0,
     legend.mar = 0,
     interpolate = TRUE)
legend("right",
       legend = c("Sampled", "Not sampled"),
       fill = c("forestgreen", "grey90"),
       border = "black",
       bty = "n")
dev.off() 

