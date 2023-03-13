# Convert to binary
binary_ras <- function(x, lower, upper) {
  x[x < lower] <- -9999
  x[x > upper] <- -9999
  x[x != -9999] <- 1
  x[x == -9999] <- 0
  names(x) <- "layer"
  x
}
