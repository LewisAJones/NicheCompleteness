## ---------------------------
##
## Script name: niche-plot.R
##
## Purpose of script: produce niche schematic
##
## Author: Dr Lewis Jones
##
## Last update: 2022-08-17
##
#--------- Load packages -------------------------------------------------------
library(MetBrewer)
library(raster)
source("./R/options.R")
#-------- Generate plot --------------------------------------------------------
# Colours for plotting
cols <- met.brewer(name="Hokusai3", n = 5, direction = -1)
cols[6] <- "black"; cols[7] <- "white"
# Generate image file
png("./figures/niche_completeness.png", 
     width = 125,  height = 125, units = "mm", res = 600)
# Set margins
par(mar = c(2, 2, 2, 2), bg="transparent")

plot(x = 1, y = 1, 
     xlim = c(0, 1),
     ylim = c(0, 1),
     type="n", yaxt="n",xaxt="n",xlab="",ylab="",bty="n")

##Plot the arrow in the x and y axis
arrows(x0=0, y0=0, x1 = 1, y1 = 0,lwd=2, code=2, length = 0.08, col = cols[6])
arrows(x0=0, y0=0, x1 = 0, y1 = 1,lwd=2, code=2, length = 0.08, col = cols[6])

#Add the labels beside the axes
mtext(side = 1, text = "PCA 1", line = 0.2, cex = 1.25, col = cols[6])
mtext(side = 2, text = "PCA 2", line = 0.2, cex = 1.25, col = cols[6])

points(x = 0.5, y = 0.5, cex = 48, pch = 21, bg = cols[1])
points(x = 0.53, y = 0.48, cex = 40, pch = 21, col = NA, bg = cols[2])
points(x = 0.55, y = 0.45, cex = 28, pch = 21, col = NA, bg = cols[4])
points(x = 0.6, y = 0.35, cex = 13, pch = 21, col = NA, bg = cols[5])

text(x = 0.255, y = 0.84, labels = expression('N'[F]), col= cols[7], font = 1, cex = 1.5)
text(x = 0.265, y = 0.64, labels = expression('N'[R]), col= cols[7], font = 1, cex = 1.5)
text(x = 0.45, y = 0.52, labels = expression('N'[O]), col= cols[6], font = 1, cex = 1.5)
text(x = 0.6, y = 0.35, labels = expression('N'[S]), col= cols[6], font = 1, cex = 1.5)

symbols(0.5, 0.5, circles = 1, inches = 1.8, fg = cols[6], lty=1, lwd = 2, add = TRUE)
symbols(0.53, 0.48, circles = 1, inches = 1.5, fg = cols[6], lty=2, lwd = 2, add = TRUE)
symbols(0.55, 0.45, circles = 1, inches = 1.05, fg = cols[6], lty=2, lwd = 2, add = TRUE)
symbols(0.6, 0.35, circles = 1, inches = 0.48, fg = cols[6], lty=2, lwd = 2, add = TRUE)
# Save plot
dev.off()
