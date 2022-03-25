div_plot <- function(x, hs, title){
  plot(hs,
       main = title,
       adj = 0,
       col = grey(1:100/100),
       legend = FALSE,
       box = FALSE,
       axes = FALSE,
       useRaster = TRUE,
       interpolate = TRUE)
  plot(x, 
       add = TRUE, 
       breaks = seq(0, 1000, 100),
       useRaster = TRUE,
       interpolate = TRUE,
       col = viridisLite::viridis(n = 10),
       alpha = 0.8,
       legend.args = list(text = 'Species richness', side =2, 
                          font = 2, line = -5, cex = 0.8))
}