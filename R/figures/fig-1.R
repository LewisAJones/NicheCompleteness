## -----------------------------------------------------------------------------
##
## Script name: fig-1.R
##
## Purpose of script: produce niche schematic
##
## Author: Dr Lewis Jones
##
## Last update: 2022-08-19
##
# Load packages ----------------------------------------------------------------
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(raster)
library(rnaturalearth)
library(rnaturalearthhires)
library(MetBrewer)
# Generate niche schematic -----------------------------------------------------
# Set seed
set.seed(5)
# Data
df <- data.frame(x = rnorm(200), y = rnorm(200))
# Generate plot
p1 <- ggplot() +
  stat_density_2d(data = df, aes(x = x, y = y, fill = ..level..), 
                  colour = "black", geom = "polygon", bins = 5) +
  geom_text(data = NULL, 
            aes(x = -0.1, y = -0.1, label = "N[S]"), size = 4, parse = TRUE) +
  geom_text(data = NULL, 
            aes(x = -0.4, y = 0.7, label = "N[O]"), size = 4, parse = TRUE) +
  geom_text(data = NULL, 
            aes(x = -0.9, y = 0.6, label = "N[R]"), size = 4, colour = "white", parse = TRUE) +
  geom_text(data = NULL, 
            aes(x = -1.4, y = 0.6, label = "N[P]"), size = 4, colour = "white", parse = TRUE) +
  xlab("PCA 1") + 
  ylab("PCA 2") +
  scale_fill_met_c(name = "Hiroshige", direction = -1) +
  theme(legend.position = "none",
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
# Get Spain worldclim data
wc <- raster("./data/raw-data/wc2-5/tmin6.bil")
# Get shapefile of Spain
SPDF <- rnaturalearth::ne_countries(scale = 10, country = "spain")
# Define extent for Spain
e <- raster::extent(-10, 4, 35.5, 44)
# Crop and mask raster layer by extent and then shape
ESP <- raster::crop(x = wc, y = e)
ESP <- raster::mask(x = ESP, mask = SPDF)
SPDF <- raster::crop(x = SPDF, y = e)
# Set up rasters for different niche levels
# Potential niche
potential <- ESP
  potential[potential < 145] <- 0
  potential[potential > 0] <- 1
# Occupied niche 
occupied <- ESP
  occupied[occupied < 165] <- 0
  occupied[occupied > 0] <- 1
# Sampled niche
sampled <- occupied
  sampled[sampled == 0] <- NA
  sampled <- raster::sampleRandom(x = sampled, size = 100, asRaster = TRUE)
sampled[is.na(sampled)] <- 0
r <- potential + occupied + sampled
r[r == 0] <- NA

# Create SPDF for plotting
r <- as(r, "SpatialPixelsDataFrame")
r <- as.data.frame(r)
colnames(r) <- c("value", "x", "y")

# Generate plot
p2 <- ggplot() +  
  geom_polygon(data = SPDF, aes(x = long, y = lat, group = group), 
               fill = "grey90", colour = "black", size = 0.25) +
  geom_tile(data = r, aes(x = x, y = y, fill = as.factor(value), colour = as.factor(value))) + 
  geom_polygon(data = SPDF, aes(x = long, y = lat, group = group), 
               fill = NA, colour = "black", size = 0.25) +
  scale_fill_manual(values = c(cols[1,], cols[3,], cols[4,]),
                    labels = c("Potential distribution", "Actual distribution", "Sampled distribution")) +
  scale_colour_manual(values = c(cols[1,], cols[3,], cols[4,]),
                    labels = c("Potential distribution", "Actual distribution", "Sampled distribution")) +
  coord_equal() +
  theme_map() +
  theme(legend.position = c(0.65, 0.05),
        legend.title = element_blank(),
        legend.key.size = unit(3, "mm"),
        panel.background = element_rect(colour = "white"))

# Build panel plot -------------------------------------------------------------
# Build
p <- ggarrange(p1, p2, labels = "AUTO")
# Save
ggsave("./figures/fig-1.jpg", units = "mm",
       height = 80, width = 200, dpi = 600)


