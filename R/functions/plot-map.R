library(raster)
library(ggplot2)
library(sf)
# Set up bounding box
ras <- raster::raster(res = 5, val = 1)
ras <- rasterToPolygons(x = ras, dissolve = TRUE)
# Robinson projection
bb <- sf::st_as_sf(x = ras)
bb <- st_transform(x = bb, crs = "ESRI:54030")
# Function
plot_map <- function(x, pts, main, bb){
  ggplot(x) + 
    geom_sf(data = bb, fill = "lightblue", colour = NA) +
    geom_sf(size = 0.1, colour = "lightgrey") +
    geom_sf(data = pts,
               size = 0.5, shape = 21, colour = "black", fill = "#2171b5",
               alpha = 0.5) +
    labs(title = main) +
    theme_void() +
    theme(
      plot.margin = margin(2, 2, 2, 2, "mm"),
      axis.text = element_blank(),
      plot.title = element_text(hjust = 0.5)) +
    coord_sf(crs = sf::st_crs("ESRI:54030"))
}