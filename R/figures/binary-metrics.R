## ---------------------------
##
## Script name: binary-metrics.R
##
## Purpose of script: produce schematic of binary metrics
##
## Author: Dr Lewis Jones
##
## Date Created: 2022-03-23
##
## Copyright (c) Lewis Jones, 2022
## Email: LewisA.Jones@outlook.com
##
#---------Load packages-----------
library(raster)
library(MetBrewer)
#---------------------------------
#get some pretty colours
cols <- met.brewer(name="Hiroshige",n=7,type="discrete")
cols <- rev(cols)


#full distribution 
r1 <- raster(nrow = 10, ncol = 15, vals = 0)
cells <- sample(x = 1:150, size = 50)
r1[cells] <- 1

#sampled distribution 
r2 <- raster(nrow = 10, ncol = 15, vals = 0)
cells <- sample(x = 1:150, size = 25)
r2[cells] <- 1

#deficit
def <- r2 - r1
def[def > 0] <- 0
def[def == -1] <- 1
plot(def)

#balance
bal <- r1 + r2
bal[bal != 2] <- 0
bal[bal == 2] <- 1
plot(bal)

#surplus
sur <- r1 - r2
sur[sur != -1] <- 0
sur[sur == -1] <- 1
plot(sur)

#generate image
jpeg("./figures/binary_metrics.jpg", width = 200,  height = 80, units = "mm", res = 600)
#plot multiple plots
par(mfrow=c(2,3), mar = c(1, 1, 1, 1))
plot(r1, legend = FALSE, col = c("grey95", cols[1]),
     main = expression(bold("Geographic predictions (ENM"[full]*")")), 
     axes = FALSE, box = FALSE)
plot(rasterToPolygons(r1), add=TRUE, border='black', lwd=1) 

plot(r1, legend = FALSE, col = NA, axes = FALSE, box = FALSE)
text(0, 60, "Binary suitability maps \nfrom ecological niche modelling")
arrows(x0 = 30, y0 = 10, x1 = 170, y1 = 10, length = 0.1)
arrows(x0 = -30, y0 = 10, x1 = -170, y1 = 10, length = 0.1)
text(-100, -30, "Calibrated with the\noccuppied niche")
text(-100, -70, expression("(ENM"[full]*")"))
text(100, -30, "Calibrated with the\nsampled niche")
text(100, -70, expression("(ENM"[samp]*")"))

plot(r2, legend = FALSE, col = c("grey95", cols[2]),
     main = expression(bold("Geographic predictions (ENM"[samp]*")")), 
     axes = FALSE, box = FALSE)
plot(rasterToPolygons(r2), add=TRUE, border='black', lwd=1) 

plot(def, legend = FALSE, col = c("grey95", cols[7]),
     main = expression(bold("Deficit (" * ENM[full.cells] %notin% ENM[samp.cells]*")")), 
     axes = FALSE, box = FALSE)
plot(rasterToPolygons(r2), add=TRUE, border='black', lwd=1) 

plot(bal, legend = FALSE, col = c("grey95", cols[6]),
     main = expression(bold("Balance (" * ENM[samp.cells] %in% ENM[full.cells]*")")), 
     axes = FALSE, box = FALSE)
plot(rasterToPolygons(r2), add=TRUE, border='black', lwd=1) 

plot(sur, legend = FALSE, col = c("grey95", cols[5]),
     main = expression(bold("Surplus (" * ENM[samp.cells] %notin% ENM[full.cells]*")")),
     axes = FALSE, box = FALSE)
plot(rasterToPolygons(r2), add=TRUE, border='black', lwd=1) 

dev.off()
