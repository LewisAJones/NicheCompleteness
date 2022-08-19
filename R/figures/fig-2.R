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
library(viridis)
# Generate figure --------------------------------------------------------------
# Define colours for plotting
cols <- viridis(n = 4, begin = 0.1, end = 1)
#cols <- MetBrewer::met.brewer(name = "Hokusai3", n = 2, direction = -1)
# Generate image file
jpeg("./figures/confusion_matrix.jpg", 
     width = 80,  height = 50, units = "mm", res = 600)
par(mar = c(0.5, 1.3, 1.2, 0.5))
# Draw empty plot
plot(1, 1, xlim = c(0, 2), ylim = c(0,2),
     col = "white", xlab = NA, ylab = NA, axes = FALSE)
# Draw polygons
polygon(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), col = cols[1])
polygon(x = c(0, 1, 1, 0), y = c(1, 1, 2, 2), col = cols[2])
polygon(x = c(1, 2, 2, 1), y = c(0, 0, 1, 1), col = cols[2])
polygon(x = c(1, 2, 2, 1), y = c(1, 1, 2, 2), col = cols[1])
# Add labels
# Bold text
par(font.lab = 2, cex.lab = 0.6)
title(xlab = "Actual", line = -9.5, outer = FALSE)
title(ylab = "Predicted", line = 0.7, outer = FALSE)
mtext("Positive (+)", font = 2, side = 3, adj = 0.22, cex = 0.5)
mtext("Negative (-)", font = 2, side = 3, adj = 0.78, cex = 0.5)
mtext("Positive (+)", font = 2, side = 2, adj = 0.82, cex = 0.5)
mtext("Negative (-)", font = 2, side = 2, adj = 0.18, cex = 0.5)
# Add text inside plot
text(x = 0.5, y = 1.5, font = 2, 
     labels = "True Positive (TP)", col = "white", cex = 0.5)
text(x = 1.5, y = 1.5, font = 2, 
     labels = "False Positive (FP)", col = "white", cex = 0.5)
text(x = 1.5, y = 1.35, font = 1, 
     labels = "Type I Error", col = "white", cex = 0.35)
text(x = 0.5, y = 0.5, font = 2, 
     labels = "False Negative (FN)", col = "white", cex = 0.5)
text(x = 0.5, y = 0.35, font = 1, 
     labels = "Type II Error", col = "white", cex = 0.35)
text(x = 1.5, y = 0.5, font = 2, 
     labels = "True Negative (TN)", col = "white", cex = 0.5)
# Save
dev.off()




