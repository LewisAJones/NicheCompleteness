## ---------------------------
##
## Script name: niche-plot.R
##
## Purpose of script: produce niche schematic
##
## Author: Dr Lewis Jones
##
## Date Created: 2022-03-23
##
## Copyright (c) Lewis Jones, 2022
## Email: LewisA.Jones@outlook.com
##
#---------Load packages-----------
library(MetBrewer)
#---------------------------------

cols <- met.brewer(name="Hiroshige",n=5,type="discrete")
cols <- rev(cols)
cols[6] <- "black"; cols[7] <- "white"

jpeg("./figures/niche_completeness.jpg", width = 125,  height = 125, units = "mm", res = 600)
par(mar = c(2, 2, 0.1, 0.1))

plot(range(0,1),range(0,1),type="n", yaxt="n",xaxt="n",xlab="",ylab="",bty="n")

##Plot the arrow in the x and y axis
arrows(x0=0, y0=0, x1 = 1, y1 = 0,lwd=2, code=2, length = 0.08) #x axis
arrows(x0=0, y0=0, x1 = 0, y1 = 1,lwd=2, code=2, length = 0.08) #x axis

#Add the labels beside the axes
mtext(side = 1, text = "PCA 1", line = 0.2, cex = 1.25)
mtext(side = 2, text = "PCA 2", line = 0.2, cex = 1.25)

points(x = 0.5, y = 0.5, cex = 50, pch = 21, bg = cols[1])
points(x = 0.53, y = 0.48, cex = 40, pch = 21, col = NA, bg = cols[2])
points(x = 0.55, y = 0.45, cex = 28, pch = 21, col = NA, bg = cols[3])
points(x = 0.6, y = 0.35, cex = 13, pch = 21, col = NA, bg = cols[4])

text(x = 0.255, y = 0.8, labels = expression('N'[F]), col= cols[7], font = 1, cex = 1.5)
text(x = 0.265, y = 0.6, labels = expression('N'[R]), col= cols[7], font = 1, cex = 1.5)
text(x = 0.45, y = 0.5, labels = expression('N'[O]), col= cols[6], font = 1, cex = 1.5)
text(x = 0.6, y = 0.35, labels = expression('N'[S]), col= cols[6], font = 1, cex = 1.5)

symbols(0.5, 0.5, circles = 1, inches = 1.87, fg = 'black', lty=1, lwd = 2, add = TRUE)
symbols(0.53, 0.48, circles = 1, inches = 1.5, fg = cols[6], lty=2, lwd = 2, add = TRUE)
symbols(0.55, 0.45, circles = 1, inches = 1.05, fg = cols[6], lty=2, lwd = 2, add = TRUE)
symbols(0.6, 0.35, circles = 1, inches = 0.48, fg = cols[6], lty=2, lwd = 2, add = TRUE)

dev.off()