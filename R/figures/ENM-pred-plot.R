## ---------------------------
##
## Script name: ENM-pred-plot.R
##
## Purpose of script: plot example prediction plot
##
## Author: Dr Lewis Jones
##
## Date Created: 2022-06-20
##
## Email: LewisA.Jones@outlook.com
##
#---------Load packages-----------
library(raster)
library(dismo)
library(ENMTools)
source("./R/options.R")
#---------------------------------
maas_files <- paste0("./results/virtual-species/maas/species-", unique(readRDS("./results/ecospat/maas.RDS")$species), ".RDS")
#get file paths
files <- list.files("./data/climate/maas/", pattern = ".grd", full.names = TRUE)

#stack rasters
stk <- stack(files)

#sample whole world for background points (reduces influence of varying backgrounds)
bg.points <- data.frame(rasterToPoints(stk, spatial=FALSE)[,c("x", "y")])

#load data
df <- readRDS(maas_files[9])

#get species name
species <- "example_species"

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

#-----generate bioclim models-----
#generate model
species_full.bc <- dismo::bioclim(x = stk, p = species_full$presence.points)
#get prediction from model
species_full.bc.suitability <- dismo::predict(object = species_full.bc, x = stk)
#get threshold for bioclim
threshold <- min(raster::extract(x = species_full.bc.suitability, y = species_full$presence.points))
#generate binary predictions
species_full.bc.suitability.binary <- species_full.bc.suitability
#apply threshold
species_full.bc.suitability.binary[species_full.bc.suitability.binary >= threshold] <- 1
species_full.bc.suitability.binary[species_full.bc.suitability.binary < 1] <- 0
#plot(species_full.bc.suitability.binary)
#generate model
species_sampled.bc <- dismo::bioclim(x = stk, p = species_sampled$presence.points)
#get prediction from model
species_sampled.bc.suitability <- dismo::predict(object = species_sampled.bc, x = stk)
#get threshold for bioclim
threshold <- min(raster::extract(x = species_sampled.bc.suitability, y = species_sampled$presence.points))
#generate binary predictions
species_sampled.bc.suitability.binary <- species_sampled.bc.suitability
#apply threshold
species_sampled.bc.suitability.binary[species_sampled.bc.suitability.binary >= threshold] <- 1
species_sampled.bc.suitability.binary[species_sampled.bc.suitability.binary < 1] <- 0

plot(species_full.bc.suitability.binary)
plot(species_sampled.bc.suitability.binary)

#---------------------------------
#generate background for plot
r <- raster(res = 1, ext = extent(c(-180, 180, -90, 90)), vals = 1)
r <- projectRaster(from = r, crs = prj, res = res)
#generate border for plotting
shp <- rasterToPolygons(x = r, dissolve = TRUE, na.rm = TRUE)
shp <- smoothr::smooth(x = shp, method = "ksmooth", smoothness = 20)
#---------------------------------
jpeg("./figures/ENM_predictions_plot.jpg", width = 270,  height = 80, units = "mm", res = 300)
par(mfrow=c(1,2), mar = c(1, 1, 1, 1))
plot(shp, col = "#f7fbff", lwd = 2)
plot(species_full.bc.suitability.binary,
     add = TRUE,
     col = c("grey90", "forestgreen"),
     box = FALSE,
     axes = FALSE,
     legend = FALSE,
     legend.width = 0,
     legend.shrink = 0,
     legend.mar = 0,
     interpolate = FALSE)
plot(shp, col = NA, lwd = 2, add = TRUE)
plot(shp, col = "#f7fbff", lwd = 2)
plot(species_sampled.bc.suitability.binary,
     add = TRUE,
     col = c("grey90", "forestgreen"),
     box = FALSE,
     axes = FALSE,
     legend = FALSE,
     legend.width = 0,
     legend.shrink = 0,
     legend.mar = 0,
     interpolate = FALSE)
plot(shp, col = NA, lwd = 2, add = TRUE)
dev.off()
