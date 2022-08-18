## ---------------------------
##
## Script name: disp-plot.R
##
## Purpose of script: dispersal frequency plot
##
## Author: Dr Lewis Jones
##
## Date Created: 2022-04-02
##
## Copyright (c) Lewis Jones, 2022
## Email: LewisA.Jones@outlook.com
##
#---------Plot-----------
good_disp = 4
poor_disp = 2
burn_in = 1000

jpeg("./figures/disp_freq_plot.jpg", width = 175,  height = 175, units = "mm", res = 600)
par(mfrow=c(2,1))

hist(sample(x = seq(0, good_disp, 1), size = burn_in, replace = TRUE, prob = exp(-1 * seq(0, good_disp, 1))),
     main = "Good dispersal",
     xlab = c("Search radius"),
     breaks = -1:4,
     xaxt = "n"
     )
axis(1, -0.5:3.5, labels = seq(0,4, 1), tick = FALSE, padj= -1.5)
hist(x = sample(x = seq(0, poor_disp, 1), size = burn_in, replace = TRUE, prob = exp(-1.5 * seq(0, poor_disp, 1))),
     main = "Poor dispersal",
     xlab = c("Search radius"),
     breaks = -1:4,
     xaxt = "n")
axis(1, -0.5:3.5, labels = seq(0,4, 1), tick = FALSE, padj= -1.5)

dev.off()
