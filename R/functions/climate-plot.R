climate_plot <- function(x, var, main) {
  x <- x[[var]]
  x <- terra::as.data.frame(x, xy = TRUE, na.rm = FALSE)
  colnames(x)[3] <- "value" 
  ggplot(data = x, aes(x = x, y = y, fill = value)) + 
    geom_tile() +
    scale_fill_viridis_c(na.value = "lightgrey") +
    labs(title = main) +
    theme_void() +
    theme(
      plot.margin = margin(2, 2, 2, 2, "mm"),
      axis.text = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_blank()) +
    guides(fill = guide_colourbar(barheight = unit(8, "mm"),
                                  barwidth = unit(120, "mm"),
                                  frame.colour = "black",
                                  ticks.colour = "white", 
                                  ticks.linewidth = 1,
                                  title.hjust = 0.5,
                                  title.position = "bottom"))
}