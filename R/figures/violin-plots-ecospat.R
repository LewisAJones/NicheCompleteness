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
library(ggplot2)
library(ggpubr)
library(dplyr)
#---------------------------------
#load data
sant <- readRDS("./results/ecospat/sant.RDS")
sant$interval <- c("Santonian")
camp <- readRDS("./results/ecospat/camp.RDS")
camp$interval <- c("Campanian")
maas <- readRDS("./results/ecospat/maas.RDS")
maas$interval <- c("Maastrichtian")

data <- rbind.data.frame(sant, camp, maas)

#convert data to factors for plotting
data$interval <- factor(data$interval, 
                        levels = c("Santonian","Campanian", "Maastrichtian"), 
                        ordered = TRUE)

#---------NICHE UNFILLING-------------
#count number of observations
summ <- data %>%
  group_by(interval) %>%
  summarize(n = n(), overlap = max(unfilling)*1.05)

p1 <- data %>%
  ggplot( aes(x=interval, y=unfilling, fill=interval)) +
  geom_violin(scale = "width") +
  geom_boxplot(width = 0.15) +
  #geom_text(data = summ, aes(x = interval, y = overlap, label = n/3)) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(breaks = seq(0, 1, 0.25), labels = seq(0, 1, 0.25)) +
  labs(x = "Interval", y = "Niche unfilling") +
  theme(plot.background = element_rect(colour = NA, fill = "white"),
        panel.background = element_blank(),
        plot.margin = margin(0.25,0.25,0.25,0.25, "cm"),
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
p1
#---------CENTROID-------------
#count number of observations
summ <- data %>%
  group_by(interval) %>%
  summarize(n = n(), overlap = max(centroid)*1.05)

p2 <- data %>%
  ggplot( aes(x=interval, y=centroid, fill=interval)) +
  geom_violin(scale = "width") +
  geom_boxplot(width = 0.15) +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Interval", y = "Centroid distance") +
  theme(plot.background = element_rect(colour = NA, fill = "white"),
        panel.background = element_rect(colour = NA, fill = "white"),
        plot.margin = margin(0.25,0.25,0.25,0.25, "cm"),
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
#---------------------------------
#arrange and save
p <- ggpubr::ggarrange(p1, p2, ncol = 2, labels = "AUTO", align = "v",
                       font.label = list(size = 20))
ggsave("./figures/fig-4.png", 
       height = 65, width = 140, units = "mm", dpi = 600, scale = 1.6, bg = "white")

