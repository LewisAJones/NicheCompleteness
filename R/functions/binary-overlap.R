# Calculate the binary overlap between two rasters
binary_overlap <- function(x, y){
  
  # Generate initial matrix for filling
  binary_met <- data.frame(cells = NA,
                           x_cells = NA,
                           y_cells = NA,
                           true_positive = NA,
                           false_positive = NA,
                           true_negative = NA,
                           false_negative = NA)
  
  # Number of cells (presence/absence)  
  binary_met$cells <- length(Which(x == 1 | x == 0, cells = TRUE))
  # Number of cells considered suitable in X
  binary_met$x_cells <- length(Which(x == 1, cells = TRUE))
  # Number of cells considered suitable in Y
  binary_met$y_cells <- length(Which(y == 1, cells = TRUE))
  
  # Binary calculations for the confusion matrix
  
  # True positive
  binary_met$true_positive <- length(Which((x + y) == 2, cells = TRUE)) /
    binary_met$cells
  # False positive
  binary_met$false_positive <- length(Which((y - x) == 1, cells = TRUE)) /
    binary_met$cells
  # True negative
  binary_met$true_negative <- length(Which((x + y) == 0, cells = TRUE)) /
    binary_met$cells
  # False negative
  binary_met$false_negative <- length(Which((y - x) == -1, cells = TRUE)) /
    binary_met$cells
  
  # Return data
  return(binary_met)
}
