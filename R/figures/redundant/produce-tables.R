## -----------------------------------------------------------------------------
##
## Script name: produce-tables.R
##
## Purpose of script: produces tables
##
## Author: Dr Lewis Jones
##
## Last updated: 2022-08-18
##
# Load packages ----------------------------------------------------------------
library(ggplot2)
library(gridExtra)
library(raster)
# Simulation summary table -----------------------------------------------------
df <- readRDS("./results/sampling-window/spatial-stats.RDS")
# Assign col names for table
colnames(df) <- c("Intervals",
                  "Simulated \n species (n)",
                  "Sampled \n species (n)",
                  "Threshold \n species (n)",
                  "Spatial sampling \n coverage (%)",
                  "Spatial sampling \n extent (km)")

# Save
jpeg("./figures/table-1.jpg",
     height=40, width=190, res = 300, units = "mm")
p <- tableGrob(df, rows = NULL,
             theme = ttheme_default())
grid.arrange(p)
dev.off()
# Remove environment
rm(list = ls())
#-------------------------------------------------------------------------------
# Summary statistics of niche unfilling, centroid distance
# What intervals?
intervals <- c(paste0(
  "Santonian (n = ", nrow(readRDS("./results/ecospat/sant.RDS"))/3, ")"), "",
  paste0(
  "Campanian (n = ", nrow(readRDS("./results/ecospat/camp.RDS"))/3, ")"), "", 
  paste0(
  "Maastrichtian (n = ", nrow(readRDS("./results/ecospat/maas.RDS"))/3, ")"), "")
# Metrics
metric <- rep(c("Niche unfilling", "Centroid distance"),3)
# Bind data
df <- cbind.data.frame(intervals, metric)

# Calculate stats for unfilling
sant <- readRDS("./results/ecospat/sant.RDS")
sant <- subset(sant, comparison == "full_sampled")
camp <- readRDS("./results/ecospat/camp.RDS")
camp <- subset(camp, comparison == "full_sampled")
maas <- readRDS("./results/ecospat/maas.RDS")
maas <- subset(maas, comparison == "full_sampled")

mean_unfilling <- c(mean(sant$unfilling),
                    mean(camp$unfilling),
                    mean(maas$unfilling))
sd_unfilling <- c(sd(sant$unfilling),
                  sd(camp$unfilling),
                  sd(maas$unfilling))
max_unfilling <- c(max(sant$unfilling),
                   max(camp$unfilling),
                   max(maas$unfilling))
min_unfilling <- c(min(sant$unfilling),
                   min(camp$unfilling),
                   min(maas$unfilling))

# Calculate stats for centroid distance
mean_centroid <- c(mean(sant$centroid),
                   mean(camp$centroid),
                   mean(maas$centroid))
sd_centroid <- c(sd(sant$centroid),
                 sd(camp$centroid),
                 sd(maas$centroid))
max_centroid <- c(max(sant$centroid),
                  max(camp$centroid),
                  max(maas$centroid))
min_centroid <- c(min(sant$centroid),
                  min(camp$centroid),
                  min(maas$centroid))

# Bind data
mean <- round(c(mean_unfilling[1], 
                mean_centroid[1], 
                mean_unfilling[2], 
                mean_centroid[2], 
                mean_unfilling[3], 
                mean_centroid[3]),3)
sd <- round(c(sd_unfilling[1], 
              sd_centroid[1], 
              sd_unfilling[2], 
              sd_centroid[2], 
              sd_unfilling[3], 
              sd_centroid[3]),3)
max <- round(c(max_unfilling[1], 
               max_centroid[1], 
               max_unfilling[2], 
               max_centroid[2], 
               max_unfilling[3], 
               max_centroid[3]),3)
min <- round(c(min_unfilling[1], 
               min_centroid[1], 
               min_unfilling[2], 
               min_centroid[2],
               min_unfilling[3], 
               min_centroid[3]),3)

# Bind data
df <- cbind.data.frame(df, mean, sd, max, min)

# Save plot
jpeg("./figures/ecospat_summary.jpg",
     height = 50, width = 150, res = 300, units = "mm")
p <- tableGrob(df, rows = NULL)
grid.arrange(p)
dev.off()
# Remove environment
rm(list = ls())
#-------------------------------------------------------------------------------
