## ---------------------------
##
## Script name: ENMTools-analysis.R
##
## Purpose of script: runs tests for niche breadth
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
library(ENMTools)
source("./R/options.R")
rm(list=ls()[-which(ls() == "tp")]) #keep ecospat res
#------------Analyses-------------
#species files (those that have been sampled and have more than 5 occurrences)
sant_files <- paste0("./results/virtual-species/sant/species-", readRDS("./results/ecospat/sant.RDS")$species, ".RDS")
camp_files <- paste0("./results/virtual-species/camp/species-", readRDS("./results/ecospat/camp.RDS")$species, ".RDS")
maas_files <- paste0("./results/virtual-species/maas/species-", readRDS("./results/ecospat/maas.RDS")$species, ".RDS")
#-----------Santonian--------------
#get file paths
files <- list.files("./data/climate/sant/", pattern = ".grd", full.names = TRUE)

#stack rasters
stk <- stack(files)

#sample whole world for background points (reduces influence of varying backgrounds)
bg.points <- data.frame(rasterToPoints(stk, spatial=FALSE)[,c("x", "y")])

overlap <- pbmclapply(sant_files, function(i){

  #load data
  df <- readRDS(i)
  
  #get species name
  species <- tools::file_path_sans_ext(basename(i))
  
  #format to enmtools.species object
  species_full <- enmtools.species(species.name = species,
                              presence.points = df$distribution,
                              background.points = bg.points)
  #check species
  species_full <- check.species(species_full)
  
  #format to enmtools.species object
  species_sampled <- enmtools.species(species.name = species,
                                   presence.points = df$sampled_distribution,
                                   background.points = bg.points)
  #check species
  species_sampled <- check.species(species_sampled)
  
  #------generate glm models-----
  species_full.glm <- enmtools.glm(species = species_full, env = stk, test.prop = tp)
  #species_full.glm
  species_sampled.glm <- enmtools.glm(species = species_sampled, env = stk, test.prop = tp)
  #species_sampled.glm
  
  #------generate domain models-----
  species_full.dm <- enmtools.dm(species = species_full, env = stk, test.prop = tp)
  #species_full.dm
  species_sampled.dm <- enmtools.dm(species = species_sampled, env = stk, test.prop = tp)
  #species_sampled.dm
  
  #-----generate bioclim models-----
  species_full.bc <- enmtools.bc(species = species_full, env = stk, test.prop = tp)
  #species_full.bc
  species_sampled.bc <- enmtools.bc(species = species_sampled, env = stk, test.prop = tp)
  #species_sampled.bc 
  
  #-----generate maxent models-----
  species_full.mx <- enmtools.maxent(species = species_full, env = stk, test.prop = tp)
  #species_full.mx
  species_sampled.mx <- enmtools.maxent(species = species_sampled, env = stk, test.prop = tp)
  #species_sampled.mx 
  
  #calculate overlap between each model
  GLM <- data.frame(raster.overlap(species_full.glm, species_sampled.glm))
  DOMAIN <- data.frame(raster.overlap(species_full.dm, species_sampled.dm))
  BIOCLIM <- data.frame(raster.overlap(species_full.bc, species_sampled.bc))
  MAXENT <- data.frame(raster.overlap(species_full.mx, species_sampled.mx))
  
  #bind data and format
  tmp <- rbind.data.frame(GLM, DOMAIN, BIOCLIM, MAXENT)
  tmp$model <- c("GLM", "DOMAIN", "BIOCLIM", "MAXENT")
  tmp$species <- species
  tmp$n <- nrow(df$distribution)
  tmp$n_samp <- nrow(df$sampled_distribution)
  #return data
  return(tmp)
}, mc.cores = detectCores()-1, mc.preschedule = FALSE, mc.cleanup = TRUE) #for parallel processing


#-----------Campanian--------------

#---------Maastrichtian------------