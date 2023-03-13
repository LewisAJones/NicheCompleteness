## ---------------------------
##
## Script name: richness-plots.R
##
## Purpose of script: richness plots
##
## Author: Dr Lewis Jones
##
## Date Created: 2023-01-23
##
## Email: LewisA.Jones@outlook.com
##
#---------Load packages--------------------------------------------------
library(raster)
library(ggplot2)
library(stringr)
library(sf)
source("./R/options.R")
source("./R/functions/div-plot.R")
#------------------------------------------------------------------------
# Generate long format df for easy plotting -----------------------------
# Set up intervals and type of raster
intervals <- c("sant", "camp", "maas")
type <- c("potential", "distribution", "sampled")
# Create empty dataframe for populating
df <- data.frame()
for (i in intervals) {
  for (j in type) {
    file <- paste0("./results/virtual-species/richness/", i, "_", j, ".grd")
    r <- raster(file)
    crs(x = r) <- crs("+proj=longlat +datum=WGS84 +no_defs")
    r <- as.data.frame(r, xy = TRUE)
    r$interval <- i
    r$type <- j
    # Rescale values to 0--1
    r$layer <- r$layer / max(r$layer, na.rm = TRUE) 
    df <- rbind.data.frame(df, r)
  }
}

# Update names for plotting
df[which(df$interval == "sant"), c("interval")] <- c("Santonian")
df[which(df$interval == "camp"), c("interval")] <- c("Campanian")
df[which(df$interval == "maas"), c("interval")] <- c("Maastrichtian")
df$interval <- str_to_title(df$interval)
df$type <- str_to_title(df$type)

# Set factor levels for plotting
df$interval <- factor(df$interval, levels = c("Santonian", "Campanian", "Maastrichtian"))
df$type <- factor(df$type, levels = c("Potential", "Distribution", "Sampled"))

# Plot data
p <- ggplot(data = df, aes(x = x, y = y, fill = layer)) +
      geom_tile() +
      coord_map(projection = "mollweide") +
      #scale_fill_viridis_c(na.value = NA) + 
      scale_fill_distiller(palette = "Greens", direction = -1, na.value = "grey90",
                           guide = guide_colourbar(
                             ticks.colour = "black",
                             frame.colour = "black")) + 
      facet_grid(interval ~ type) +
      theme(
        plot.background = element_rect(colour = NA, fill = "white"),
        panel.background = element_blank(),
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        panel.grid.minor.y = element_line(colour = "grey90", linewidth = 1),
        panel.grid.minor.x = element_line(colour = "grey90", linewidth = 1),
        panel.grid.major.y = element_line(colour = "grey90", linewidth = 1),
        panel.grid.major.x = element_line(colour = "grey90", linewidth = 1),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = c("bottom"),
        legend.background = element_rect(fill = NA, colour = NA),
        legend.text = element_text(size = 10),
        legend.spacing.x = unit(0.05, 'cm'),
        legend.title = element_blank(),
        legend.key.width = unit(2.5, "cm"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = 12, face = "bold"))
# Save plot
ggsave("./figures/fig-4.jpg",
       height = 120, width = 200, units = "mm", dpi = 600, scale = 1.75)
