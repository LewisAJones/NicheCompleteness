div_plot <- function(x, title, normalise = FALSE){
  if (normalise == TRUE) {
    x <- x/raster::cellStats(x = x, stat = "max")
    plot(x, 
       breaks = seq(0, 1, 0.1),
       useRaster = TRUE,
       interpolate = FALSE,
       col = viridisLite::turbo(n = 11),
       alpha = 1,
       add = TRUE,
       axes = FALSE,
       box = FALSE,
       legend.args = list(text = "Species richness", side =2, 
                          font = 2, line = 0, cex = 0.8))} else {
  plot(x, 
       useRaster = TRUE,
       interpolate = FALSE,
       col = viridisLite::turbo(n = 11),
       alpha = 1,
       add = TRUE,
       axes = FALSE,
       box = FALSE,
       legend.args = list(text = "Species richness", side =2, 
                         font = 2, line = 0, cex = 0.8))
  title(main = title, line = -2)
}
}