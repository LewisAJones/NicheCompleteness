## ---------------------------
##
## Script name: dispersal-freq-plots.R
##
## Purpose of script: histograms of dispersal
## capacity and sampling
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
#cols for plotting
cols <- met.brewer(name = "Hiroshige", n = 5, type = "discrete")
#------------FILES-----------------
sant_files <- list.files("./results/virtual-species/sant/", full.names = TRUE)
camp_files <- list.files("./results/virtual-species/camp/", full.names = TRUE)
maas_files <- list.files("./results/virtual-species/maas/", full.names = TRUE)
#------------PLOT-----------------
jpeg("./figures/disp_cap_sim.jpg", width = 200,  height = 250, units = "mm", res = 300)
par(mfrow=c(3,2), mar=c(4,4,4,4))
#---------SANTONIAN---------------
poor <- vector("numeric")
good <- vector("numeric")

for(i in sant_files){
  tmp <- readRDS(i)
  
  if(tmp$dispersal_cap == "poor_disp"){
    val <- nrow(tmp$distribution)
    poor <- append(poor, val)
  }
  
  if(tmp$dispersal_cap == "good_disp"){
    val <- nrow(tmp$distribution)
    good <- append(good, val)
  }
}

hist(good, 
     main = "Santonian (good dispersal)",
     xlab = "Number of occurrences simulated per species",
     col = cols[5])
hist(poor, 
     main = "Santonian (poor dispersal)",
     xlab = "Number of occurrences simulated per species",
     col = cols[2])
#---------CAMPANIAN---------------
poor <- vector("numeric")
good <- vector("numeric")

for(i in camp_files){
  tmp <- readRDS(i)
  
  if(tmp$dispersal_cap == "poor_disp"){
    val <- nrow(tmp$distribution)
    poor <- append(poor, val)
  }
  
  if(tmp$dispersal_cap == "good_disp"){
    val <- nrow(tmp$distribution)
    good <- append(good, val)
  }
}

hist(good, 
     main = "Campanian (good dispersal)",
     xlab = "Number of occurrences simulated per species",
     col = cols[5])
hist(poor, 
     main = "Campanian (poor dispersal)",
     xlab = "Number of occurrences simulated per species",
     col = cols[2])
#---------MAASTRICHTIAN---------------
poor <- vector("numeric")
good <- vector("numeric")

for(i in maas_files){
  tmp <- readRDS(i)
  
  if(tmp$dispersal_cap == "poor_disp"){
    val <- nrow(tmp$distribution)
    poor <- append(poor, val)
  }
  
  if(tmp$dispersal_cap == "good_disp"){
    val <- nrow(tmp$distribution)
    good <- append(good, val)
  }
}

hist(good, 
     main = "Maastrichtian (good dispersal)",
     xlab = "Number of occurrences simulated per species",
     col = cols[5])
hist(poor, 
     main = "Maastrichtian (poor dispersal)",
     xlab = "Number of occurrences simulated per species",
     col = cols[2])
#---------FINISH---------------
dev.off()

