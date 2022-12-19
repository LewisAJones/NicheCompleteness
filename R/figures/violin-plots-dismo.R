## -----------------------------------------------------------------------------
##
## Script name: fig-4.R
##
## Purpose of script: violin plots of dismo results
##
## Author: Dr Lewis Jones
##
## Last updated: 2022-08-24
##
# Load packages ----------------------------------------------------------------
library(MetBrewer)
library(dplyr)
library(ggplot2)
library(tidyr)
# ------------------------------------------------------------------------------
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

# Add factor levels for plotting
data$interval <- factor(data$interval,
                        levels = c("Santonian","Campanian", "Maastrichtian"),
                        ordered = TRUE)

# Gather data
data <- gather(data = data,
               key = "metric",
               value = "value",
               true_positive, false_positive, true_negative, false_negative)

data$metric[which(data$metric == "true_positive")] <- "True Positive"
data$metric[which(data$metric == "false_positive")] <- "False Positive"
data$metric[which(data$metric == "true_negative")] <- "True Negative"
data$metric[which(data$metric == "false_negative")] <- "False Negative"

# Add factor levels for plotting
data$metric <- factor(data$metric,
                        levels = c("True Positive","False Positive", "True Negative", "False Negative"),
                        ordered = TRUE)

# plot data  -------------------------------------------------------------------

p1 <- data %>%
  filter(comparison == "occupied_sample") %>%
  ggplot(aes(x = model, y = value, fill = model)) +
  geom_violin(scale = "width") +
  geom_boxplot(width = 0.15) +
  scale_fill_met_d(name = "Hiroshige", alpha=1) +
  scale_y_continuous(limits = c(0, 1.05)) +
  facet_grid(interval ~ metric) +
  labs(x = "Model", y = "Proportion of cells") +
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







p <- ggpubr::ggarrange(p1, p3, p5, p2, p4, p6, ncol = 3, nrow = 2, labels = "AUTO", align = "v",
                       font.label = list(size = 20))
ggsave("./figures/binary_metric_violin.jpg",  height = 150, width = 250, units = "mm", dpi = 600, scale = 1.6)
