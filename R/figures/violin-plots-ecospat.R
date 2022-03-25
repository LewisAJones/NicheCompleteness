## ---------------------------
##
## Script name: violin-plots-ecospat.R
##
## Purpose of script: plot ecospat results
##
## Author: Dr Lewis Jones
##
## Date Created: 2022-03-25
##
## Copyright (c) Lewis Jones, 2022
## Email: LewisA.Jones@outlook.com
##
#---------Load packages-----------
library(MetBrewer)
library(ggplot2)
#---------------------------------
#load data
# create a dataset
data <- data.frame(
  interval=c( rep("Santonian",500), rep("Campanian",500), rep("Maastrichtian",500)),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1))
)

#convert data to factors for plotting
data$interval <- factor(data$interval, levels = c("Santonian","Campanian", "Maastrichtian"), ordered = TRUE)
#---------SCHOENER'S D-------------
p1 <- data %>%
  ggplot( aes(x=interval, y=value, fill=interval)) +
  geom_violin() +
  scale_fill_met_d(name = "Hiroshige", alpha=1) +
  labs(x = "Interval", y = "Schoener's D") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.grid.minor.y = element_line(colour = "grey90"),
        panel.grid.minor.x = element_line(colour = "grey90"),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 14, angle = 0, hjust = 0),
        axis.text = element_text(size = 12, angle = 0, hjust = 0.5),
        axis.title = element_text(size = 14, face = "bold", vjust = 0),
        strip.text = element_text(size = 12, face = "bold"),
        aspect.ratio = 1)

#---------NICHE UNFILLING-------------
p2 <- data %>%
  ggplot( aes(x=interval, y=value, fill=interval)) +
  geom_violin() +
  scale_fill_met_d(name = "Hiroshige", alpha=1) +
  labs(x = "Interval", y = "Niche unfilling") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.grid.minor.y = element_line(colour = "grey90"),
        panel.grid.minor.x = element_line(colour = "grey90"),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 14, angle = 0, hjust = 0),
        axis.text = element_text(size = 12, angle = 0, hjust = 0.5),
        axis.title = element_text(size = 14, face = "bold", vjust = 0),
        strip.text = element_text(size = 12, face = "bold"),
        aspect.ratio = 1)
p2
#---------CENTROID-------------
p3 <- data %>%
  ggplot( aes(x=interval, y=value, fill=interval)) +
  geom_violin() +
  scale_fill_met_d(name = "Hiroshige", alpha=1) +
  labs(x = "Interval", y = "Centroid distance") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.grid.minor.y = element_line(colour = "grey90"),
        panel.grid.minor.x = element_line(colour = "grey90"),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 14, angle = 0, hjust = 0),
        axis.text = element_text(size = 12, angle = 0, hjust = 0.5),
        axis.title = element_text(size = 14, face = "bold", vjust = 0),
        strip.text = element_text(size = 12, face = "bold"),
        aspect.ratio = 1)
p3
#---------------------------------
#arrange and save
p <- ggpubr::ggarrange(p1, p2, p3, nrow = 3, labels = "AUTO")
ggsave("./figures/ecospat_violin.png", height = 200, width = 70, units = "mm", dpi = 600, scale = 1.6)

