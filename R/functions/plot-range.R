plot_range <- function(n, path) {
  species <- readRDS(paste0("./results/virtual-species/",
                            path,
                            "/",
                            "species-",
                            n,
                            ".RDS"))
  r <- rast(res = 1)
  potential <- as.matrix.data.frame(species$potential_xy)
  potential <- rasterize(x = potential[, c("x", "y")],
                         y = r,
                         values = potential[, c("layer")])
  distribution <- as.matrix.data.frame(species$distribution_xy)
  distribution <- rasterize(x = distribution[, c("x", "y")],
                         y = r,
                         values = distribution[, c("layer")])
  
  sampled <- distribution
  sampled[!is.na(sampled)] <- 0
  if (is.data.frame(species$sampled_distribution_xy)) {
    cells <- cellFromXY(object = distribution, xy = species$sampled_distribution_xy)
    sampled[cells] <- 1
  }
  stk <- c(potential, distribution, sampled)
  names(stk) <- c("Potential", "Distribution", "Sampled")
  plot(stk)
  title(main = paste0("Species ", n), outer = TRUE, adj = 0.75, line = -15)
}
