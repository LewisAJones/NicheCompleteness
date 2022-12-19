## -----------------------------------------------------------------------------
##
## Script name: fig-3.R
##
## Purpose of script: plot sampled climate
##
## Author: Dr Lewis Jones
##
## Lasted updated: 2022-08-23
##
# Load packages ----------------------------------------------------------------
library(MetBrewer)
library(ggplot2)
# Set-up -----------------------------------------------------------------------
# Colours for plotting
cols <- met.brewer(name = "Hiroshige", n = 5, type = "discrete")
# Read data
df <- readRDS("./results/climate/sampled-climate.RDS")
# Plot -------------------------------------------------------------------------
#plot temperature
temp <- ggplot(data = df, aes(x = min_temp, y = max_temp, colour = sampled, shape = sampled)) + 
    geom_point(size = 0.8) +
    xlab("Minimum temperature (ºC)") +
    ylab("Maximum temperature (ºC)") +
    scale_color_manual(values=c(cols[2], cols[5])) +
    scale_shape_manual(values=c(17, 19)) +
    facet_wrap(~interval) +
    theme(plot.background = element_rect(colour = NA, fill = "white"),
          panel.background = element_blank(),
          plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
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
  scale_shape_manual(values=c(17, 19)) +
  facet_wrap(~interval) +
  theme(plot.background = element_rect(colour = NA, fill = "white"),
    panel.background = element_blank(),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
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
p <- ggpubr::ggarrange(temp, prec, nrow = 2, labels = "AUTO", align = "v",
                         font.label = list(size = 20))
#p <- ggpubr::ggarrange(temp, prec, nrow = 2, align = "v")
ggsave("./figures/fig-3.jpg", height = 120, width = 150, units = "mm", dpi = 600, scale = 1.75)
