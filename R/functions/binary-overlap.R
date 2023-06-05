# Calculate the binary overlap between two rasters using the confusion 
# matrix

# x = a binary raster of suitability values (0 = unsuitable, 1 = suitable)
# y = a binary raster of suitability values (0 = unsuitable, 1 = suitable)
binary_overlap <- function(x, y){
  
  # Binary calculations for the confusion matrix
  
  # True positives (cells in x and y are equal to 1)
  TP <- length(cells(x = (x + y), y = 2)[[1]])
  # False positive (cells in x are 0 and cells in y are 1)
  FP <- length(cells(x = (x - y), y = -1)[[1]])
  # True negative (cells in x and y have a value of 0)
  TN <- length(cells(x = (x + y), y = 0)[[1]])
  # False negative (cells in y are 0 and cells in x are 1)
  FN <- length(cells(x = (y - x), y = -1)[[1]])
  

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
