## ---------------------------
##
## Script name: violin-plots-dismo.R
##
## Purpose of script: plot dismo results
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
library(dplyr)
library(ggplot2)
#---------------------------------
#load data
sant <- readRDS("./results/dismo/sant.RDS")
sant <- bind_rows(sant)
sant$interval <- c("Santonian")

camp <- readRDS("./results/dismo/camp.RDS")
camp <- bind_rows(camp)
camp$interval <- c("Campanian")

maas <- readRDS("./results/dismo/maas.RDS")
maas <- bind_rows(maas)
maas$interval <- c("Maastrichtian")

data <- rbind.data.frame(sant, camp, maas)

#convert data to factors for plotting
data$interval <- factor(data$interval, levels = c("Santonian","Campanian", "Maastrichtian"), ordered = TRUE)

data_bioclim <- subset(data, model == "BIOCLIM")
data_maxent <- subset(data, model == "MAXENT")
#---------BINARY METRICS-------------
#### BIOCLIM

p1 <- data_bioclim %>%
  ggplot(aes(x=interval, y=deficit_realised_samp, fill=interval)) +
  geom_violin(scale = "width") +
  geom_boxplot(width = 0.15) +
  scale_fill_met_d(name = "Hiroshige", alpha=1) +
  scale_y_continuous(limits = c(0, 1.05)) +
  labs(x = "Interval", y = "Prportion of false negatives (BIOCLIM)") +
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
p1

p2 <- data_maxent %>%
  ggplot(aes(x=interval, y=deficit_realised_samp, fill=interval)) +
  geom_violin(scale = "width") +
  geom_boxplot(width = 0.15) +
  scale_fill_met_d(name = "Hiroshige", alpha=1) +
  scale_y_continuous(limits = c(0, 1.05)) +
  labs(x = "Interval", y = "Proportion of false negatives (MAXENT)") +
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

#### BALANCE

p3 <- data_bioclim %>%
  ggplot(aes(x=interval, y= balance_realised_samp, fill=interval)) +
  geom_violin(scale = "width") +
  geom_boxplot(width = 0.15) +
  scale_fill_met_d(name = "Hiroshige", alpha=1) +
  scale_y_continuous(limits = c(0, 1.05)) +
  labs(x = "Interval", y = "Proportion of true positives (BIOCLIM)") +
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

p4 <- data_maxent %>%
  ggplot(aes(x=interval, y= balance_realised_samp, fill=interval)) +
  geom_violin(scale = "width") +
  geom_boxplot(width = 0.15) +
  scale_fill_met_d(name = "Hiroshige", alpha=1) +
  scale_y_continuous(limits = c(0, 1.05)) +
  labs(x = "Interval", y = "Proportion of true positives (MAXENT)") +
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
p4

#### SURPLUS

p5 <- data_bioclim %>%
  ggplot(aes(x=interval, y= surplus_realised_samp, fill=interval)) +
  geom_violin(scale = "width") +
  geom_boxplot(width = 0.15) +
  scale_fill_met_d(name = "Hiroshige", alpha=1) +
  scale_y_continuous(limits = c(0, 1.05)) +
  labs(x = "Interval", y = "Proportion of false positives (BIOCLIM)") +
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
p5

p6 <- data_maxent %>%
  ggplot(aes(x=interval, y= surplus_realised_samp, fill=interval)) +
  geom_violin(scale = "width") +
  geom_boxplot(width = 0.15) +
  scale_fill_met_d(name = "Hiroshige", alpha=1) +
  scale_y_continuous(limits = c(0, 1.05)) +
  labs(x = "Interval", y = "Proportion of false positives (MAXENT)") +
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
p6

p <- ggpubr::ggarrange(p1, p3, p5, p2, p4, p6, ncol = 3, nrow = 2, align = "v")
ggsave("./figures/binary_metric_violin.jpg",  height = 150, width = 250, units = "mm", dpi = 600, scale = 1.6)

#### SURPLUS
#count number of observations
summ <- data_bioclim %>%
  group_by(interval) %>%
  summarize(n = n(), surplus_full_samp = max(surplus_full_samp)*1.3)

p5 <- data_bioclim %>%
  ggplot(aes(x=interval, y= surplus_full_samp, fill=interval)) +
  geom_violin(scale = "width") +
  geom_boxplot(width = 0.15) +
  geom_text(data = summ, aes(x = interval, y = surplus_full_samp, label = n), fontface = "bold") +
  scale_fill_met_d(name = "Hiroshige", alpha=1) +
  scale_y_continuous(limits = c(0, 1.05)) +
  labs(x = "Interval", y = "Surplus (BIOCLIM)") +
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
p5

#count number of observations
summ <- data_maxent %>%
  group_by(interval) %>%
  summarize(n = n(), surplus_full_samp = max(surplus_full_samp)*1.025)

p6 <- data_maxent %>%
  ggplot(aes(x=interval, y = surplus_full_samp, fill=interval)) +
  geom_violin(scale = "width") +
  geom_boxplot(width = 0.15) +
  geom_text(data = summ, aes(x = interval, y = surplus_full_samp, label = n), fontface = "bold") +
  scale_fill_met_d(name = "Hiroshige", alpha=1) +
  scale_y_continuous(limits = c(0, 1.05)) +
  labs(x = "Interval", y = "Surplus (MaxEnt)") +
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
p6

p <- ggpubr::ggarrange(p1, p3, p5, p2, p4, p6, ncol = 3, nrow = 2, labels = "AUTO", align = "v",
                       font.label = list(size = 20))
ggsave("./figures/binary_metric_violin.jpg",  height = 150, width = 250, units = "mm", dpi = 600, scale = 1.6)
