## ---------------------------
##
## Script name: climate-samp-plot.R
##
## Purpose of script: plot sampled climate
##
## Author: Dr Lewis Jones
##
## Date Created: 2023-04-26
##
## Email: LewisA.Jones@outlook.com
##
#---------Load packages-----------
library(MetBrewer)
library(raster)
library(ggplot2)
library(gridExtra)
#cols for plotting
cols <- met.brewer(name = "Hiroshige", n = 5, type = "discrete")
#---------------------------------
#stack rasters
sant <- stack(list.files(paste0("./data/climate/sant/"), pattern = ".grd", full.names = TRUE))
sant_samp <- raster("./results/sampling-window/sampling_raster_sant.grd")
sant_samp[sant_samp == 0] <- NA
#extract values which are sampled
sant_samp <- mask(x = sant, mask = sant_samp)
#remove sampled values
sant <- mask(x = sant, mask = sant_samp, inverse = TRUE)
#convert to dataframes
sant <- as.data.frame(x = sant, xy = TRUE, na.rm = TRUE)
sant_samp <- as.data.frame(x = sant_samp, xy = TRUE, na.rm = TRUE)
#add colours and sampled status
sant$col <- cols[2]
sant$sampled <- "Available"
sant_samp$col <- cols[5]
sant_samp$sampled <- "Sampled"
#bind data
sant <- rbind.data.frame(sant, sant_samp)
#add interval
sant$interval <- "Santonian"

#stack rasters
camp <- stack(list.files(paste0("./data/climate/camp/"), pattern = ".grd", full.names = TRUE))
camp_samp <- raster("./results/sampling-window/sampling_raster_camp.grd")
camp_samp[camp_samp == 0] <- NA
#extract values which are sampled
camp_samp <- mask(x = camp, mask = camp_samp)
#remove sampled values
camp <- mask(x = camp, mask = camp_samp, inverse = TRUE)
#convert to dataframes and bind
camp <- as.data.frame(x = camp, xy = TRUE, na.rm = TRUE)
camp_samp <- as.data.frame(x = camp_samp, xy = TRUE, na.rm = TRUE)
#add colours and sampled status
camp$col <- cols[2]
camp$sampled <- "Available"
camp_samp$col <- cols[5]
camp_samp$sampled <- "Sampled"
#bind data
camp <- rbind.data.frame(camp, camp_samp)
#add interval
camp$interval <- "Campanian"

#stack rasters
maas <- stack(list.files(paste0("./data/climate/maas/"), pattern = ".grd", full.names = TRUE))
maas_samp <- raster("./results/sampling-window/sampling_raster_maas.grd")
maas_samp[maas_samp == 0] <- NA
#extract values which are sampled
maas_samp <- mask(x = maas, mask = maas_samp)
#remove sampled values
maas <- mask(x = maas, mask = maas_samp, inverse = TRUE)
#convert to dataframes and bind
maas <- as.data.frame(x = maas, xy = TRUE, na.rm = TRUE)
maas_samp <- as.data.frame(x = maas_samp, xy = TRUE, na.rm = TRUE)
#add colours and sampled status
maas$col <- cols[2]
maas$sampled <- "Available"
maas_samp$col <- cols[5]
maas_samp$sampled <- "Sampled"
#bind data
maas <- rbind.data.frame(maas, maas_samp)
#add interval name
maas$interval <- "Maastrichtian"

#bind data
df <- rbind.data.frame(sant, camp, maas)
df$interval = factor(df$interval, levels=c("Santonian", "Campanian", "Maastrichtian"))
#---------------------------------
#plot temperature
temp <- ggplot(data = df, aes(x = min_temp, y = max_temp, colour = sampled, shape = sampled)) + 
    geom_point(size = 0.8) +
    xlab("Minimum temperature (ºC)") +
    ylab("Maximum temperature (ºC)") +
    scale_color_manual(values=c(cols[2], cols[5])) +
    scale_shape_manual(values=c(16, 15)) +
    facet_wrap(~interval) +
    theme(plot.background = element_rect(colour = NA, fill = "white"),
          panel.background = element_blank(),
          plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
          panel.grid.minor.y = element_line(colour = "grey90"),
          panel.grid.minor.x = element_line(colour = "grey90"),
          panel.grid.major.y = element_line(colour = "grey90"),
          panel.grid.major.x = element_line(colour = "grey90"),
          panel.border = element_rect(colour = "black", fill = NA),
          legend.position = c(0.06, 0.1),
          legend.background = element_rect(fill = NA, colour = NA),
          legend.text = element_text(size = 10),
          legend.spacing.x = unit(0.05, 'cm'),
          legend.title = element_blank(),
          legend.key=element_blank(),
          axis.text = element_text(size = 12, angle = 0, hjust = 0.5),
          axis.title = element_text(size = 14, face = "bold", vjust = 0),
          strip.text = element_text(size = 12, face = "bold"),
          aspect.ratio = 1)
temp <- temp + guides(colour = guide_legend(override.aes = list(size=2.5)))

#plot precipitation  
prec <- ggplot(data = df, aes(x = min_precip, y = max_precip, colour = sampled, shape = sampled)) + 
  geom_point(size = 0.8) +
  xlab("Minimum precipitation (mm/day)") +
  ylab("Maximum precipitation (mm/day)") +
  scale_color_manual(values=c(cols[2], cols[5])) +
  scale_shape_manual(values=c(16, 15)) +
  facet_wrap(~interval) +
  theme(plot.background = element_rect(colour = NA, fill = "white"),
    panel.background = element_blank(),
    plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
    panel.grid.minor.y = element_line(colour = "grey90"),
    panel.grid.minor.x = element_line(colour = "grey90"),
    panel.grid.major.y = element_line(colour = "grey90"),
    panel.grid.major.x = element_line(colour = "grey90"),
    panel.border = element_rect(colour = "black", fill = NA),
    legend.position = c(0.94, 0.9),
    legend.background = element_rect(fill = NA, colour = NA),
    legend.text = element_text(size = 10),
    legend.spacing.x = unit(0.05, 'cm'),
    legend.title = element_blank(),
    legend.key=element_blank(),
    axis.text = element_text(size = 12, angle = 0, hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold", vjust = 0),
    strip.text = element_text(size = 12, face = "bold"),
    aspect.ratio = 1)
prec <- prec + guides(colour = guide_legend(override.aes = list(size=2.5)))
#---------------------------------
#arrange and save
#p <- ggpubr::ggarrange(temp, prec, nrow = 2, labels = "AUTO", align = "v",
#                         font.label = list(size = 20))
p <- ggpubr::ggarrange(temp, prec, nrow = 2, align = "v")
ggsave("./figures/climate_sampling_plots.jpg", height = 120, width = 150, units = "mm", dpi = 600, scale = 1.75)
