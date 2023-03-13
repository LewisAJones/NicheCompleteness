# -----------------------------------------------------------------------
# Project: NicheCompleteness
# File name: fig-1.R
# Last updated: 2023-02-23
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/NicheCompleteness
# Load libraries --------------------------------------------------------
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(raster)
library(terra)
library(geodata)
library(sf)
# Generate niche schematic -----------------------------------------------------
# Set seed
set.seed(5)
# Data
df <- data.frame(x = rnorm(200), y = rnorm(200))
# Generate plot
p1 <- ggplot() +
  stat_density_2d(data = df, aes(x = x, y = y, fill = ..level..), 
                  colour = "black", geom = "polygon", bins = 6) +
  geom_text(data = NULL, 
            aes(x = -0.1, y = -0.1, label = "N[S]"), size = 4, parse = TRUE) +
  geom_text(data = NULL, 
            aes(x = -0.3, y = 0.65, label = "N[O]"), size = 4, parse = TRUE) +
  geom_text(data = NULL, 
            aes(x = -0.75, y = 0.6, label = "N[R]"), size = 4, colour = "white", parse = TRUE) +
  geom_text(data = NULL, 
            aes(x = -1.2, y = 0.3, label = "N[P]"), size = 4, colour = "white", parse = TRUE) +
  geom_text(data = NULL, 
            aes(x = -1.6, y = 0.6, label = "N[F]"), size = 4, colour = "white", parse = TRUE) +
  xlab("Axis 1") + 
  ylab("Axis 2") +
  scale_fill_distiller(palette = "Blues") +
  #scale_fill_met_c(name = "Hiroshige", direction = -1) +
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 12, margin = margin(t = 10)),
        axis.title.y = element_text(size = 12, margin = margin(r = 10)),
        axis.line.y.left = element_line(size = 0.75, arrow = arrow(length = unit(3, "mm"))),
        axis.line.x.bottom = element_line(size = 0.75, arrow = arrow(length = unit(3, "mm")))) +
  coord_fixed()
p1
# Grab colours from plot for secondary plot
b <- ggplot_build(p1)
cols <- unique(b$data[[1]]["fill"])
# Generate geographic schematic ------------------------------------------------
# Get worldclim data
wc <- worldclim_country(country = "ESP", res = 10, var="tavg", path=tempdir())
# Get mean of data
wc <- terra::app(x = wc, "mean")
# Get world map data
lc <- world(path = tempdir(), resolution = 2, level = 0)
# Extract Spain and Portugal
lc <- lc[which(lc$NAME_0 %in% c("Spain", "Portugal"))]
# Define extent for the Peninsula
e <- raster::extent(-10, 3.5, 36, 43.5)
# Crop and mask raster layer by extent and then shape
peninsular <- raster::mask(x = wc, mask = lc)
peninsular <- raster::crop(x = peninsular, y = e)
lc <- raster::crop(x = lc, y = e)
# Set up rasters for different distributions
# Potential distribution
potential <- peninsular
  potential[potential < 15] <- 0
  potential[potential > 0] <- 1
# Actual distribution 
occupied <- peninsular
  occupied[occupied < 17] <- 0
  occupied[occupied > 0] <- 1
# Sampled distribution
sampled <- occupied
  sampled[sampled == 0] <- NA
  samp <- terra::spatSample(x = sampled, 
                               cells = TRUE, 
                               xy = TRUE,
                               size = 10, 
                               method = "random",
                               na.rm = TRUE)
sampled <- occupied
sampled[sampled == 1] <- 0
sampled[samp$cell] <- 1
r <- potential + occupied + sampled
r[r == 0] <- NA

# Create SPDF for plotting
r <- as.data.frame(r, xy = TRUE)
colnames(r) <- c("x", "y", "value")
lc <- as(lc, "Spatial")
# Islands
blc <- lc
blc@polygons <- blc@polygons[-2]
blc@polygons[[1]]@Polygons <- blc@polygons[[1]]@Polygons[-9]
blc@polygons[[1]]@plotOrder <- c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L)
# Remove Islands
lc@polygons[[1]]@Polygons <- lc@polygons[[1]]@Polygons[9]
lc@polygons[[1]]@plotOrder <- 1L

# Generate plot
p2 <- ggplot() +  
  geom_polygon(data = lc, aes(x = long, y = lat, group = group), 
               fill = "grey90", colour = "black", size = 0.25) +
  geom_tile(data = r, aes(x = x, y = y, fill = as.factor(value),
                          colour = as.factor(value))) + 
  geom_polygon(data = blc, aes(x = long, y = lat, group = group), 
               fill = "white", colour = "white", size = 0.25) +
  geom_polygon(data = lc, aes(x = long, y = lat, group = group), 
               fill = NA, colour = "black", size = 0.25) +
  geom_point(data = samp, aes(x = x, y = y), 
             stroke = 0.25, colour = "black", fill = cols[5,],
             shape = 22, size = 1.25) +
  scale_fill_manual(values = c(cols[1,], cols[3,], cols[5,]),
                    labels = c("Potential distribution", 
                               "Apparent distribution", 
                               "Sampled distribution")) +
  scale_colour_manual(values = c(cols[1,], cols[3,], cols[5,]),
                    labels = c("Potential distribution", 
                               "Apparent distribution", 
                               "Sampled distribution")) +
  coord_quickmap() +
  theme_map() +
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
        legend.position = c(0.67, 0.05),
        legend.title = element_blank(),
        legend.key = element_rect(colour = "black"),
        legend.key.size = unit(3, "mm"),
        panel.background = element_rect(colour = "white"))
p2
# Build panel plot -------------------------------------------------------------
# Build
p <- ggarrange(p1, p2, labels = "AUTO", ncol = 1, heights = c(1.1, 1))
# Save
ggsave("./figures/fig-1.png", units = "mm",
       height = 165, width = 100, dpi = 300, bg = "white")


