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
source("./R/options.R")
rm(list=ls()[-which(ls() == "ecospat_res")]) #keep ecospat res
#------------Analyses-------------

#species files (those that have been sampled)
sant_files <- readRDS("./results/virtual-species/sampled/sant.RDS")
camp_files <- readRDS("./results/virtual-species/sampled/camp.RDS")
maas_files <- readRDS("./results/virtual-species/sampled/maas.RDS")

#-----------Santonian--------------
#get file paths
files <- list.files("./data/climate/sant/", pattern = ".grd", full.names = TRUE)

#stack rasters
stk <- stack(files)

#generate empty dataframe
ecospat_df <- data.frame()

for(i in sant_files){
  #load data
  df <- readRDS(i)
  
  #minimum of five points needed for ecospat function
  if(nrow(df$sampled_distribution) < 5){next}
  
  #extract climate data for whole study area
  df$env <- na.omit(data.frame(getValues(stk)))
  #which cells full distribution
  cells_dist <- cellFromXY(stk$max_precip, xy = df$distribution[,c("x", "y")])
  #which cells sampled distribution
  cells_samp <- cellFromXY(stk$max_precip, xy = df$sampled_distribution[,c("x", "y")])
  
  #extract climate data to points for full distribution
  df$full_env <- extract(x = stk, y = cells_dist)
  #extract climate data to points for sampled distribution
  df$sampled_env <- extract(x = stk, y = cells_samp)
  
  #compute PCA for whole study area
  pca.env <- dudi.pca(df$env, center = TRUE, scale = TRUE, scannf=FALSE, nf=2)
  
  #plot PCA
  #ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)
  
  # PCA scores for the whole study area
  scores.globclim <- pca.env$li
  
  #add full distribution
  scores.full <- suprow(pca.env,df$full_env)$li
  
  #add sampled distribution
  scores.sampled <- suprow(pca.env,df$sampled_env)$li
  
  #grid full distribution niche
  grid.clim.full <- ecospat.grid.clim.dyn(glob=scores.globclim, #entire global available to species for background pixels
                                          glob1=scores.globclim, #entire globe available to species for background pixels
                                          sp=scores.full, #environmental values for species occurrences
                                          R=ecospat_res, #resolution of grid
                                          th.sp = 0, #do not exclude low species density values
                                          th.env = 0) #do not exclude low species density values
  
  #grid sampled distribution niche
  grid.clim.samp <- ecospat.grid.clim.dyn(glob=scores.globclim, #entire globe available to species for background pixels
                                          glob1=scores.full, #background pixels of species (those only available for sampled species)
                                          sp=scores.sampled, #sampled environmental values
                                          R=ecospat_res, #resolution of grid
                                          th.sp = 0, #do not exclude low species density values
                                          th.env = 0) #do not exclude low species density values
  
  #quantify niche dynamics
  niche.dyn <- ecospat.niche.dyn.index(grid.clim.full, grid.clim.samp, intersection = NA)
  
  #extract niche dynamics metric
  df <- t(data.frame(niche.dyn$dynamic.index.w))
  
  #calculate niche overlap (D)
  overlap <- ecospat.niche.overlap(grid.clim.full, grid.clim.samp, cor = FALSE)$D
  
  #calculate and add centroid distance
  x1 <- cbind(median(scores.full[,1]), median(scores.full[,2]))
  x2 <- cbind(median(scores.sampled[,1]), median(scores.sampled[,2]))
  centroid <- as.numeric(sqrt(rowSums((x1 - x2)^2)))
  df <- cbind.data.frame(df, overlap, centroid)
  
  #add species name
  species <- tools::file_path_sans_ext(basename(i))
  species <- as.numeric(substr(species, 9, nchar(species)))
  df <- cbind.data.frame(df, species)
  
  #remove row names
  rownames(df) <- NULL
  
  #bind data
  ecospat_df <- rbind.data.frame(ecospat_df, df)
  #plot
  png(paste0("./results/ecospat/plots/sant/species-",species,".png"), width = 150,  height = 150, units = "mm", res = 300)
  ecospat.plot.niche.dyn(grid.clim.full, grid.clim.samp,
                         quant=0, interest=2, 
                         title= tools::file_path_sans_ext(basename(i)), 
                         name.axis1 = "PCA-1", 
                         name.axis2 = "PCA-2",
                         col.unf ="green", col.exp = "red", col.stab = "blue", colZ1 ="green3", colZ2 = "red3", transparency = 70)
  ecospat.shift.centroids(scores.full, scores.sampled, scores.globclim, scores.globclim)
  dev.off()
  #Sys.sleep(1)
}
ecospat_df <- ecospat_df[order(ecospat_df$species),]
saveRDS(object = ecospat_df, file = "./results/ecospat/sant.RDS")

#-----------Campanian--------------
#get file paths
files <- list.files("./data/climate/camp/", pattern = ".grd", full.names = TRUE)

#stack rasters
stk <- stack(files)

#generate empty dataframe
ecospat_df <- data.frame()

for(i in camp_files){
  #load data
  df <- readRDS(i)
  
  #minimum of five points needed for ecospat function
  if(nrow(df$sampled_distribution) < 5){next}
  
  #extract climate data for whole study area
  df$env <- na.omit(data.frame(getValues(stk)))
  #which cells full distribution
  cells_dist <- cellFromXY(stk$max_precip, xy = df$distribution[,c("x", "y")])
  #which cells sampled distribution
  cells_samp <- cellFromXY(stk$max_precip, xy = df$sampled_distribution[,c("x", "y")])
  
  #extract climate data to points for full distribution
  df$full_env <- extract(x = stk, y = cells_dist)
  #extract climate data to points for sampled distribution
  df$sampled_env <- extract(x = stk, y = cells_samp)
  
  #compute PCA for whole study area
  pca.env <- dudi.pca(df$env, center = TRUE, scale = TRUE, scannf=FALSE, nf=2)
  
  #plot PCA
  #ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)
  
  # PCA scores for the whole study area
  scores.globclim <- pca.env$li
  
  #add full distribution
  scores.full <- suprow(pca.env,df$full_env)$li
  
  #add sampled distribution
  scores.sampled <- suprow(pca.env,df$sampled_env)$li
  
  #grid full distribution niche
  grid.clim.full <- ecospat.grid.clim.dyn(glob=scores.globclim, #entire global available to species for background pixels
                                          glob1=scores.globclim, #entire globe available to species for background pixels
                                          sp=scores.full, #environmental values for species occurrences
                                          R=ecospat_res, #resolution of grid
                                          th.sp = 0, #do not exclude low species density values
                                          th.env = 0) #do not exclude low species density values
  
  #grid sampled distribution niche
  grid.clim.samp <- ecospat.grid.clim.dyn(glob=scores.globclim, #entire globe available to species for background pixels
                                          glob1=scores.full, #background pixels of species (those only available for sampled species)
                                          sp=scores.sampled, #sampled environmental values
                                          R=ecospat_res, #resolution of grid
                                          th.sp = 0, #do not exclude low species density values
                                          th.env = 0) #do not exclude low species density values
  
  #quantify niche dynamics
  niche.dyn <- ecospat.niche.dyn.index(grid.clim.full, grid.clim.samp, intersection = NA)
  
  #extract niche dynamics metric
  df <- t(data.frame(niche.dyn$dynamic.index.w))
  
  #calculate niche overlap (D)
  overlap <- ecospat.niche.overlap(grid.clim.full, grid.clim.samp, cor = FALSE)$D
  
  #calculate and add centroid distance
  x1 <- cbind(median(scores.full[,1]), median(scores.full[,2]))
  x2 <- cbind(median(scores.sampled[,1]), median(scores.sampled[,2]))
  centroid <- as.numeric(sqrt(rowSums((x1 - x2)^2)))
  df <- cbind.data.frame(df, overlap, centroid)
  
  #add species name
  species <- tools::file_path_sans_ext(basename(i))
  species <- as.numeric(substr(species, 9, nchar(species)))
  df <- cbind.data.frame(df, species)
  
  #remove row names
  rownames(df) <- NULL
  
  #bind data
  ecospat_df <- rbind.data.frame(ecospat_df, df)
  #plot
  png(paste0("./results/ecospat/plots/camp/species-",species,".png"), width = 150,  height = 150, units = "mm", res = 300)
  ecospat.plot.niche.dyn(grid.clim.full, grid.clim.samp,
                         quant=0, interest=2, 
                         title= tools::file_path_sans_ext(basename(i)), 
                         name.axis1 = "PCA-1", 
                         name.axis2 = "PCA-2",
                         col.unf ="green", col.exp = "red", col.stab = "blue", colZ1 ="green3", colZ2 = "red3", transparency = 70)
  ecospat.shift.centroids(scores.full, scores.sampled, scores.globclim, scores.globclim)
  dev.off()
  #Sys.sleep(1)
}
ecospat_df <- ecospat_df[order(ecospat_df$species),]
saveRDS(object = ecospat_df, file = "./results/ecospat/camp.RDS")

#-----------Maastrichtian--------------
#get file paths
files <- list.files("./data/climate/maas/", pattern = ".grd", full.names = TRUE)

#stack rasters
stk <- stack(files)

#generate empty dataframe
ecospat_df <- data.frame()

for(i in maas_files){
  #load data
  df <- readRDS(i)
  
  #minimum of five points needed for ecospat function
  if(nrow(df$sampled_distribution) < 5){next}
  
  #extract climate data for whole study area
  df$env <- na.omit(data.frame(getValues(stk)))
  #which cells full distribution
  cells_dist <- cellFromXY(stk$max_precip, xy = df$distribution[,c("x", "y")])
  #which cells sampled distribution
  cells_samp <- cellFromXY(stk$max_precip, xy = df$sampled_distribution[,c("x", "y")])
  
  #extract climate data to points for full distribution
  df$full_env <- extract(x = stk, y = cells_dist)
  #extract climate data to points for sampled distribution
  df$sampled_env <- extract(x = stk, y = cells_samp)
  
  #compute PCA for whole study area
  pca.env <- dudi.pca(df$env, center = TRUE, scale = TRUE, scannf=FALSE, nf=2)
  
  #plot PCA
  #ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)
  
  # PCA scores for the whole study area
  scores.globclim <- pca.env$li
  
  #add full distribution
  scores.full <- suprow(pca.env,df$full_env)$li
  
  #add sampled distribution
  scores.sampled <- suprow(pca.env,df$sampled_env)$li
  
  #grid full distribution niche
  grid.clim.full <- ecospat.grid.clim.dyn(glob=scores.globclim, #entire global available to species for background pixels
                                          glob1=scores.globclim, #entire globe available to species for background pixels
                                          sp=scores.full, #environmental values for species occurrences
                                          R=ecospat_res, #resolution of grid
                                          th.sp = 0, #do not exclude low species density values
                                          th.env = 0) #do not exclude low species density values
  
  #grid sampled distribution niche
  grid.clim.samp <- ecospat.grid.clim.dyn(glob=scores.globclim, #entire globe available to species for background pixels
                                          glob1=scores.full, #background pixels of species (those only available for sampled species)
                                          sp=scores.sampled, #sampled environmental values
                                          R=ecospat_res, #resolution of grid
                                          th.sp = 0, #do not exclude low species density values
                                          th.env = 0) #do not exclude low species density values
  
  #quantify niche dynamics
  niche.dyn <- ecospat.niche.dyn.index(grid.clim.full, grid.clim.samp, intersection = NA)
  
  #extract niche dynamics metric
  df <- t(data.frame(niche.dyn$dynamic.index.w))
  
  #calculate niche overlap (D)
  overlap <- ecospat.niche.overlap(grid.clim.full, grid.clim.samp, cor = FALSE)$D
  
  #calculate and add centroid distance
  x1 <- cbind(median(scores.full[,1]), median(scores.full[,2]))
  x2 <- cbind(median(scores.sampled[,1]), median(scores.sampled[,2]))
  centroid <- as.numeric(sqrt(rowSums((x1 - x2)^2)))
  df <- cbind.data.frame(df, overlap, centroid)
  
  #add species name
  species <- tools::file_path_sans_ext(basename(i))
  species <- as.numeric(substr(species, 9, nchar(species)))
  df <- cbind.data.frame(df, species)
  
  #remove row names
  rownames(df) <- NULL
  
  #bind data
  ecospat_df <- rbind.data.frame(ecospat_df, df)
  #plot
  png(paste0("./results/ecospat/plots/maas/species-",species,".png"), width = 150,  height = 150, units = "mm", res = 300)
  ecospat.plot.niche.dyn(grid.clim.full, grid.clim.samp,
                         quant=0, interest=2, 
                         title= tools::file_path_sans_ext(basename(i)), 
                         name.axis1 = "PCA-1", 
                         name.axis2 = "PCA-2",
                         col.unf ="green", col.exp = "red", col.stab = "blue", colZ1 ="green3", colZ2 = "red3", transparency = 70)
  ecospat.shift.centroids(scores.full, scores.sampled, scores.globclim, scores.globclim)
  dev.off()
  #Sys.sleep(1)
}
ecospat_df <- ecospat_df[order(ecospat_df$species),]
saveRDS(object = ecospat_df, file = "./results/ecospat/maas.RDS")
#-----------FINISH--------------
beepr::beep(sound = 2)
