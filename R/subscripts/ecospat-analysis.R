## ---------------------------
##
## Script name: ecospat-analysis.R
##
## Purpose of script: runs tests for niche unfilling
##
## Author: Dr Lewis Jones
##
## Date Created: 2022-03-10
##
## Copyright (c) Lewis Jones, 2022
## Email: LewisA.Jones@outlook.com
##
#---------Load packages-----------
library(raster)
library(ecospat)
#------------Analyses-------------

#species files
sant_files <- list.files("./results/virtual-species/sant/", full.names = TRUE)
camp_files <- list.files("./results/virtual-species/camp/", full.names = TRUE)
maas_files <- list.files("./results/virtual-species/maas/", full.names = TRUE)

#-----------Santonian--------------
#get file paths
files <- list.files("./data/climate/sant/", full.names = TRUE)
  
#stack rasters
stk <- stack(files)

for(i in sant_files){
  #load data
  df <- readRDS(i)
  
  #next file if no sampled data exists
  if(df$sampled_distribution == "No sampled data"){next}
  if(nrow(df$sampled_distribution) == 1){next}
  
  #extract climate data for whole study area
  df$env <- na.omit(data.frame(getValues(stk)))
  #extract climate data to points for full distribution
  df$full_env <- extract(x = stk, y = df$distribution[,c("x", "y")])
  #extract climate data to points for sampled distribution
  df$sampled_env <- extract(x = stk, y = df$sampled_distribution[,c("x", "y")])
  
  #compute PCA for whole study area
  #pca.env <- dudi.pca(df$env, scannf=FALSE, nf=2)
  
  #plot PCA
  #ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)
  
  # PCA scores for the whole study area
  scores.globclim <- dudi.pca(df$env, scannf=FALSE, nf=2)$li
  #compute PCA scores for full distribution
  scores.full <- dudi.pca(df$full_env, scannf=FALSE, nf=2)$li
  #compute PCA scores for sampled distribution
  scores.sampled <- dudi.pca(df$sampled_env, scannf=FALSE, nf=2)$li
  
  #grid full distribution niche
  grid.clim.full <- ecospat.grid.clim.dyn(glob=scores.globclim,
                                         glob1=scores.globclim,
                                         sp=scores.sp.nat, R=100,
                                         th.sp=0)
  
  #grid sampled distribution niche
  grid.clim.samp <- ecospat.grid.clim.dyn(glob=scores.globclim,
                                         glob1=scores.globclim,
                                         sp=scores.sampled, R=100,
                                         th.sp=0)
  
  #quantify niche dynamics
  niche.dyn <- ecospat.niche.dyn.index (grid.clim.full, grid.clim.samp, intersection = 0.1)
}
  


ecospat.plot.niche.dyn(grid.clim.nat, grid.clim.inv, quant=0.25, interest=2,
                       title= "Niche Overlap", name.axis1="PC1",
                       name.axis2="PC2")

