# Calculate the binary overlap between two rasters using the confusion 
# matrix

# x = a binary raster of suitability values (0 = unsuitable, 1 = suitable)
# y = a binary raster of suitability values (0 = unsuitable, 1 = suitable)
binary_overlap <- function(x, y){
  
  # Binary calculations for the confusion matrix
  
  # True positives (cells in x and y are equal to 1)
  TP <- length(Which((x + y) == 2, cells = TRUE))
  # False positive (cells in x are 0 and cells in y are 1)
  FP <- length(Which((x - y) == -1, cells = TRUE))
  # True negative (cells in x and y have a value of 0)
  TN <- length(Which((x + y) == 0, cells = TRUE))
  # False negative (cells in y are 0 and cells in x are 1)
  FN <- length(Which((y - x) == -1, cells = TRUE))
  

  # TPR: TP/TP+FN
  TPR <- TP / (TP + FN)
  # FPR: FP/FP+TN
  FPR <- FP / (FP + TN)
  # TNR: TN/TN+FP
  TNR <- TN / (TN + FP)
  # FNR: FN/FN+TP
  FNR <- FN / (FN + TP)
    
  # Build dataframe
  df <- data.frame(TP, FP, TN, FN, TPR, FPR, TNR, FNR)
  
  # Return data
  return(df)
}
