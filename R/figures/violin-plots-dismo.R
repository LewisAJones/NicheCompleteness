# -----------------------------------------------------------------------
# Project: NicheCompleteness
# File name: violin-plots-dismo.R
# Last updated: 2023-02-27
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/NicheCompleteness
# -----------------------------------------------------------------------
# Load packages ---------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
# -----------------------------------------------------------------------
# Load data and format
sant <- readRDS("./results/dismo/sant.RDS")
sant <- bind_rows(sant)
sant$interval <- c("Santonian")

camp <- readRDS("./results/dismo/camp.RDS")
camp <- bind_rows(camp)
camp$interval <- c("Campanian")

maas <- readRDS("./results/dismo/maas.RDS")
maas <- bind_rows(maas)
maas$interval <- c("Maastrichtian")

# Bind data
data <- rbind.data.frame(sant, camp, maas)

# long format dataframe
data <- data %>% pivot_longer(cols = TPR:FNR)

# Add factor levels for plotting
data$interval <- factor(data$interval,
                        levels = c("Santonian","Campanian", "Maastrichtian"),
                        ordered = TRUE)
data$name <- factor(data$name,
                    levels = c("TPR","FNR", "FPR", "TNR"),
                    ordered = TRUE)


# Potential and sampled -------------------------------------------------
# Subset the dataset
tmp <- data[which(data$known == "potential" & data$predicted == "sampled"), ]

p1 <- ggplot(data = tmp, aes(x = name, y = value, fill = name)) +
  geom_violin(scale = "width") +
  geom_boxplot(width = 0.15) +
  #geom_text(data = summ, aes(x = interval, y = overlap, label = n/3)) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(breaks = seq(0, 1, 0.25), labels = seq(0, 1, 0.25)) +
  labs(x = "Interval", y = "Scores") +
  facet_grid(model~interval) +
  theme(plot.background = element_rect(colour = NA, fill = "white"),
        panel.background = element_blank(),
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
        axis.title = element_text(size = 16, face = "bold", vjust = 0),
        strip.text = element_text(size = 12, face = "bold"),
        aspect.ratio = 1)
p1

# Occupied and sampled --------------------------------------------------
# Subset the dataset
tmp <- data[which(data$known == "occupied" & data$predicted == "sampled"), ]

p2 <- ggplot(data = tmp, aes(x = name, y = value, fill = name)) +
  geom_violin(scale = "width") +
  geom_boxplot(width = 0.15) +
  #geom_text(data = summ, aes(x = interval, y = overlap, label = n/3)) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(breaks = seq(0, 1, 0.25), labels = seq(0, 1, 0.25)) +
  labs(x = "Interval", y = "Scores") +
  facet_grid(model~interval) +
  theme(plot.background = element_rect(colour = NA, fill = "white"),
        panel.background = element_blank(),
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
        axis.title = element_text(size = 16, face = "bold", vjust = 0),
        strip.text = element_text(size = 12, face = "bold"),
        aspect.ratio = 1)
p2

ggsave(filename = "./figures/fig-5.png", plot = p1,
       height = 140, width = 200, units = "mm", dpi = 600, scale = 1.4)
ggsave(filename = "./figures/fig-6.png", plot = p2, 
       height = 140, width = 200, units = "mm", dpi = 600, scale = 1.4)
