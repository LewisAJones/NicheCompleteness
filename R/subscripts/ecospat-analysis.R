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

#generate empty dataframe
master <- data.frame()

for(i in sant_files){
  #load data
  df <- readRDS(i)
  
  #next file if no sampled data exists
  if(df$sampled_distribution == "No sampled data"){next}
  
  #minimum of five points needed for ecospat function
  if(nrow(df$sampled_distribution) < 5){next}

  #extract climate data for whole study area
  df$env <- na.omit(data.frame(getValues(stk)))
  #extract climate data to points for full distribution
  df$full_env <- extract(x = stk, y = df$distribution[,c("x", "y")])
  #extract climate data to points for sampled distribution
  df$sampled_env <- extract(x = stk, y = df$sampled_distribution[,c("x", "y")])
  
  #compute PCA for whole study area
  pca.env <- dudi.pca(df$env, scannf=FALSE, nf=2)
  
  #plot PCA
  #ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)
  
  # PCA scores for the whole study area
  scores.globclim <- pca.env$li
  
  #add full distribution
  scores.full <- suprow(pca.env,df$full_env)$li

  #add sampled distribution
  scores.sampled <- suprow(pca.env,df$sampled_env)$li
  
  #add all data for full distribution
  scores.clim.full <- suprow(pca.env,df$env)$li
  
  # PCA scores for the whole invaded study area
  scores.clim.samp <- suprow(pca.env,df$env)$li
  
  #grid full distribution niche
  grid.clim.full <- ecospat.grid.clim.dyn(glob=scores.globclim,
                                         glob1=scores.clim.full,
                                         sp=scores.full, R=100,
                                         th.sp = 0)
  
  #grid sampled distribution niche
  grid.clim.samp <- ecospat.grid.clim.dyn(glob=scores.globclim,
                                         glob1=scores.clim.samp,
                                         sp=scores.sampled, R=100,
                                         th.sp=0)
  
  #quantify niche dynamics
  niche.dyn <- ecospat.niche.dyn.index(grid.clim.full, grid.clim.samp, intersection = 0)

  #extract niche dynamics metric
  df <- t(data.frame(niche.dyn$dynamic.index.w))
  
  #add species to row name
  row.names(df) <- tools::file_path_sans_ext(basename(i))
  
  #bind data
  master <- rbind.data.frame(master, df)
  #plot
  #ecospat.plot.niche.dyn(grid.clim.full, grid.clim.samp, quant=0.25, interest=2, title= "Niche Overlap", name.axis1="PC1", name.axis2="PC2")
}

