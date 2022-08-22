  ## -----------------------------------------------------------------------------
  ##
  ## Script name: fig-2.R
  ##
  ## Purpose of script: Generate figure of the confusion matrix
  ##
  ## Author: Dr Lewis Jones
  ##
  ## Last updated: 2022-08-17
  ##
  # Load libraries ---------------------------------------------------------------
  library(MetBrewer)
  # Generate figure --------------------------------------------------------------
  # Define colours for plotting
  cols <- MetBrewer::met.brewer(name = "Hiroshige", n = 4, direction = -1)
  # Generate image file
  jpeg("./figures/fig-2.jpg", 
       width = 80,  height = 50, units = "mm", res = 600)
  par(mar = c(0.5, 1.3, 1.2, 0.5))
  # Draw empty plot
  plot(1, 1, xlim = c(0, 2), ylim = c(0,2),
       col = "white", xlab = NA, ylab = NA, axes = FALSE)
  # Draw polygons
  polygon(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), col = cols[1])
  polygon(x = c(0, 1, 1, 0), y = c(1, 1, 2, 2), col = cols[2])
  polygon(x = c(1, 2, 2, 1), y = c(0, 0, 1, 1), col = cols[3])
  polygon(x = c(1, 2, 2, 1), y = c(1, 1, 2, 2), col = cols[4])
  polygon(x = c(0.75, 1.25, 1.25, 0.75), y = c(0.85, 0.85, 1.15, 1.15), col = "black")
  # Add labels
  # Bold text
  par(font.lab = 2, cex.lab = 0.6)
  title(xlab = "Actual", line = -9.5, outer = FALSE)
  title(ylab = "Predicted", line = 0.7, outer = FALSE)
  mtext("Positive (+)", font = 2, side = 3, adj = 0.22, cex = 0.5)
  mtext("Negative (-)", font = 2, side = 3, adj = 0.78, cex = 0.5)
  mtext("Positive (+)", font = 2, side = 2, adj = 0.82, cex = 0.5)
  mtext("Negative (-)", font = 2, side = 2, adj = 0.18, cex = 0.5)
  mtext(expression(1~"="~Presence~~0~"="~Absence~~R[1]~"="~Actual~~R[2]~"="~Prediction), 
        font = 2, side = 1, adj = 0.08, line = -0.7, cex = 0.35)
  # Add text inside plot
  text(x = 0.5, y = 1.5, font = 2, 
       labels = "True Positive (TP)", col = "black", cex = 0.5)
  text(x = 1.5, y = 1.5, font = 2, 
       labels = "False Positive (FP)", col = "white", cex = 0.5)
  text(x = 1.5, y = 1.65, font = 1, 
       labels = "Type I Error", col = "white", cex = 0.35)
  text(x = 0.5, y = 0.5, font = 2, 
       labels = "False Negative (FN)", col = "white", cex = 0.5)
  text(x = 0.5, y = 0.65, font = 1, 
       labels = "Type II Error", col = "white", cex = 0.35)
  text(x = 1.5, y = 0.5, font = 2, 
       labels = "True Negative (TN)", col = "black", cex = 0.5)
  
  # Matrix addition
  text(x = 0.8, y = 1, font = 2, 
       labels = expression(R[1]), col = "white", cex = 0.35)
  text(x = 0.9, y = 1, font = 2, 
       labels = "1 0\n0 1", col = "white", cex = 0.5)
  text(x = 1, y = 1, font = 2, 
       labels = "-", col = "white", cex = 0.5)
  text(x = 1.2, y = 1, font = 2, 
       labels = expression(R[2]), col = "white", cex = 0.35)
  text(x = 1.1, y = 1, font = 2, 
       labels = "1 0\n1 0", col = "white", cex = 0.5)
  
  # Absolute lines
  lines(x = c(0.84, 0.84), y = c(0.88, 1.12), col = "white")
  lines(x = c(1.16, 1.16), y = c(0.88, 1.12), col = "white")
  
  # TP
  points(x = 0.481, y = 1.345, pch = 15, col = "white", cex = 0.7)
  text(x = 0.5, y = 1.3, font = 2, 
       labels = "0 0\n1 1", col = "black", cex = 0.35)
  
  # FP
  points(x = 1.481, y = 1.25, pch = 15, col = "black", cex = 0.7)
  text(x = 1.5, y = 1.3, font = 2, 
       labels = "0 0\n1 1", col = "white", cex = 0.35)
  
  # FN
  points(x = 0.518, y = 0.2525, pch = 15, col = "black", cex = 0.7)
  text(x = 0.5, y = 0.3, font = 2, 
       labels = "0 0\n1 1", col = "white", cex = 0.35)
  # TN
  points(x = 1.518, y = 0.345, pch = 15, col = "white", cex = 0.7)
  text(x = 1.5, y = 0.3, font = 2, 
       labels = "0 0\n1 1", col = "black", cex = 0.35)
  # Save
  dev.off()
  
  
  
  

  