# Header ----------------------------------------------------------------
# Project: NicheCompleteness
# File name: richness-plots.R
# Last updated: 2023-03-15
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/NicheCompleteness
# Load packages ---------------------------------------------------------
library(terra)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(sf)
source("./R/options.R")
#------------------------------------------------------------------------
# Generate long format df for easy plotting -----------------------------
r <- rast(list.files("./results/virtual-species/richness/", full.names = TRUE))
filenames <- list.files("./results/virtual-species/richness/")
filenames <- tools::file_path_sans_ext(filenames)
names(r) <- filenames
r <- project(x = r, "ESRI:54030")
r <- terra::as.data.frame(r, xy = TRUE)
# long format dataframe
r <- r %>% pivot_longer(cols = camp_distribution:sant_sampled)

# Update names
r$type <- NA
r$type[grepl(pattern = "_distribution", x = r$name)] <- "Distribution"
r$type[grepl(pattern = "_potential", x = r$name)] <- "Potential"
r$type[grepl(pattern = "_samp", x = r$name)] <- "Sampled"
r$name[grepl(pattern = "camp_", x = r$name)] <- "Campanian"
r$name[grepl(pattern = "maas_", x = r$name)] <- "Maastrichtian"
r$name[grepl(pattern = "sant_", x = r$name)] <- "Santonian"
colnames(r)[which(colnames(r) == "value")] <- "Richness"

r$name <- factor(r$name, levels = c("Santonian", "Campanian", "Maastrichtian"))
r$type <- factor(r$type, levels = c("Potential", "Distribution", "Sampled"))

ggplot(data = r, aes(x = x, y = y, fill = Richness)) +
  geom_tile() +
  labs(x = "Longitude (\u00B0)", y = "Latitude (\u00B0)") +
  scale_fill_viridis_c(na.value = NA) +
  facet_grid(type~name) +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom"
  ) +
  coord_equal()
  
# Save plot
ggsave("./figures/fig-4.jpg",
       height = 120, width = 200, units = "mm", dpi = 600, scale = 1.75)
