## ---------------------------
##
## Script name: produce-tables.R
##
## Purpose of script: produces tables
##
## Author: Dr Lewis Jones
##
## Date Created: 2022-03-23
##
## Copyright (c) Lewis Jones, 2022
## Email: LewisA.Jones@outlook.com
##
#---------Load packages-----------
library(ggplot2)
library(gridExtra)
library(raster)
#---------------------------------
#Simulation summary table
Intervals <- c("Santonian", "Campanian", "Maastrichtian")
Simulated <- c(10000, 10000, 10000)
Sampled <- c(length(readRDS("./results/virtual-species/sampled/sant.RDS")),
             length(readRDS("./results/virtual-species/sampled/camp.RDS")),
             length(readRDS("./results/virtual-species/sampled/maas.RDS")))
Threshold <- c(nrow(readRDS("./results/ecospat/sant.RDS")),
               nrow(readRDS("./results/ecospat/camp.RDS")),
               nrow(readRDS("./results/ecospat/maas.RDS")))

#spatial coverage calculation
sant_samp <- sum(freq(x = raster("./results/sampling-window/sampling_raster_sant.grd"), useNA = 'no')[,c("count")]) / 
  sum(freq(x = raster("./data/climate/sant/max_precip.grd"), useNA = 'no')[,c("count")])

camp_samp <- sum(freq(x = raster("./results/sampling-window/sampling_raster_camp.grd"), useNA = 'no')[,c("count")]) / 
  sum(freq(x = raster("./data/climate/camp/max_precip.grd"), useNA = 'no')[,c("count")])

maas_samp <- sum(freq(x = raster("./results/sampling-window/sampling_raster_maas.grd"), useNA = 'no')[,c("count")]) / 
  sum(freq(x = raster("./data/climate/maas/max_precip.grd"), useNA = 'no')[,c("count")])
#bind data
SC <- rbind.data.frame(sant_samp, camp_samp, maas_samp)
#conver to percentages
SC <- SC * 100
#round data
SC <- round(SC, digits = 2)

#spatial clustering
r <- rasterToPoints(raster("./results/sampling-window/sampling_raster_sant.grd"))[,c("x", "y")]
  gcdists <- pointDistance(r, lonlat = FALSE)
  mst_sp <- vegan::spantree(gcdists)
  MST <- sum(mst_sp$dist)/1000
  sant <- round(MST, digits = 0)
  
r <- rasterToPoints(raster("./results/sampling-window/sampling_raster_camp.grd"))[,c("x", "y")]
  gcdists <- pointDistance(r, lonlat = FALSE)
  mst_sp <- vegan::spantree(gcdists)
  MST <- sum(mst_sp$dist)/1000
  camp <- round(MST, digits = 0)
  
r <- rasterToPoints(raster("./results/sampling-window/sampling_raster_maas.grd"))[,c("x", "y")]
  gcdists <- pointDistance(r, lonlat = FALSE)
  mst_sp <- vegan::spantree(gcdists)
  MST <- sum(mst_sp$dist)/1000
  maas <- round(MST, digits = 0)

#bind data
ext <- rbind(sant, camp, maas)
#bind data
df <- cbind.data.frame(Intervals, Simulated, Sampled, Threshold, SC, ext)
#assign col names
colnames(df) <- c("Intervals", "Simulated \n species (n)", "Sampled \n species (n)", "Threshold \n species (n)", "Sampling \n coverage (%)", "Sampling \n extent (km)")

#save
png("./figures/simulation_summary.png", height=35, width=175, res = 300, units = "mm")
p<-tableGrob(df, rows = NULL)
grid.arrange(p)
dev.off()
#remove environment
rm(list = ls())
#---------------------------------
#Summary statistics of niche unfilling, centroid distance
#intervals
Intervals <- c("Santonian", "", "", "Campanian", "", "", "Maastrichtian", "", "")
#metrics
Metric <- rep(c("Niche unfilling", "Schoener's D", "Centroid distance"),3)
#bind data
df <- cbind.data.frame(Intervals, Metric)
#calculate stats for unfilling
mean_unfilling <- c(mean(readRDS("./results/ecospat/sant.RDS")$unfilling),
                    mean(readRDS("./results/ecospat/camp.RDS")$unfilling),
                    mean(readRDS("./results/ecospat/maas.RDS")$unfilling))
sd_unfilling <- c(sd(readRDS("./results/ecospat/sant.RDS")$unfilling),
                  sd(readRDS("./results/ecospat/camp.RDS")$unfilling),
                  sd(readRDS("./results/ecospat/maas.RDS")$unfilling))
max_unfilling <- c(max(readRDS("./results/ecospat/sant.RDS")$unfilling),
                   max(readRDS("./results/ecospat/camp.RDS")$unfilling),
                   max(readRDS("./results/ecospat/maas.RDS")$unfilling))
min_unfilling <- c(min(readRDS("./results/ecospat/sant.RDS")$unfilling),
                   min(readRDS("./results/ecospat/camp.RDS")$unfilling),
                   min(readRDS("./results/ecospat/maas.RDS")$unfilling))

#calculate stats for overlap
mean_overlap <- c(mean(readRDS("./results/ecospat/sant.RDS")$overlap),
                    mean(readRDS("./results/ecospat/camp.RDS")$overlap),
                    mean(readRDS("./results/ecospat/maas.RDS")$overlap))
sd_overlap <- c(sd(readRDS("./results/ecospat/sant.RDS")$overlap),
                  sd(readRDS("./results/ecospat/camp.RDS")$overlap),
                  sd(readRDS("./results/ecospat/maas.RDS")$overlap))
max_overlap <- c(max(readRDS("./results/ecospat/sant.RDS")$overlap),
                   max(readRDS("./results/ecospat/camp.RDS")$overlap),
                   max(readRDS("./results/ecospat/maas.RDS")$overlap))
min_overlap <- c(min(readRDS("./results/ecospat/sant.RDS")$overlap),
                   min(readRDS("./results/ecospat/camp.RDS")$overlap),
                   min(readRDS("./results/ecospat/maas.RDS")$overlap))

#calculate stats for centroid distance
mean_centroid <- c(mean(readRDS("./results/ecospat/sant.RDS")$centroid),
                    mean(readRDS("./results/ecospat/camp.RDS")$centroid),
                    mean(readRDS("./results/ecospat/maas.RDS")$centroid))
sd_centroid <- c(sd(readRDS("./results/ecospat/sant.RDS")$centroid),
                  sd(readRDS("./results/ecospat/camp.RDS")$centroid),
                  sd(readRDS("./results/ecospat/maas.RDS")$centroid))
max_centroid <- c(max(readRDS("./results/ecospat/sant.RDS")$centroid),
                   max(readRDS("./results/ecospat/camp.RDS")$centroid),
                   max(readRDS("./results/ecospat/maas.RDS")$centroid))
min_centroid <- c(min(readRDS("./results/ecospat/sant.RDS")$centroid),
                   min(readRDS("./results/ecospat/camp.RDS")$centroid),
                   min(readRDS("./results/ecospat/maas.RDS")$centroid))

#bind data
Mean <- round(c(mean_unfilling[1], 
                mean_overlap[1], 
                mean_centroid[1], 
                mean_unfilling[2], 
                mean_overlap[2],
                mean_centroid[2], 
                mean_unfilling[3], 
                mean_overlap[3],
                mean_centroid[3]),3)
SD <- round(c(sd_unfilling[1], 
              sd_overlap[1], 
              sd_centroid[1], 
              sd_unfilling[2], 
              sd_overlap[2],
              sd_centroid[2], 
              sd_unfilling[3], 
              sd_overlap[3],
              sd_centroid[3]),3)
Max <- round(c(max_unfilling[1], 
               max_overlap[1], 
               max_centroid[1], 
               max_unfilling[2], 
               max_overlap[2],
               max_centroid[2], 
               max_unfilling[3], 
               max_overlap[3],
               max_centroid[3]),3)
Min <- round(c(min_unfilling[1], 
               min_overlap[1],
               min_centroid[1], 
               min_unfilling[2], 
               min_overlap[2],
               min_centroid[2],
               min_unfilling[3], 
               min_overlap[3],
               min_centroid[3]),3)

#bind data
df <- cbind.data.frame(df, Mean, SD, Max, Min)

#save plot
png("./figures/ecospat_summary.png", height=72, width=130, res = 300, units = "mm")
p<-tableGrob(df, rows = NULL)
grid.arrange(p)
dev.off()
#remove environment
rm(list = ls())
#---------------------------------
