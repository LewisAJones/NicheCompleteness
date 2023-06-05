## -----------------------------------------------------------------------------
##
## Script name: ecospat-summary.R
##
## Purpose of script: produce ecospat summary stats
##
## Author: Dr Lewis Jones
##
## Last updated: 2022-08-24
##
# Ecospat summary table --------------------------------------------------------
df <- data.frame(Interval = rep(c("Santonian", "Campanian", "Maastrichtian"), each = 2),
                 n = c(rep(nrow(readRDS("./results/ecospat/sant.RDS")) / 3, each = 2),
                 rep(nrow(readRDS("./results/ecospat/camp.RDS")) / 3, each = 2),
                 rep(nrow(readRDS("./results/ecospat/maas.RDS")) / 3, each = 2)),
                 metric = rep(c("Niche unfilling", "Centroid distance"), times = 3))


# Calculate stats for unfilling for full vs sampled distribution
sant <- readRDS("./results/ecospat/sant.RDS")
sant <- subset(sant, comparison == "potential_sampled")
camp <- readRDS("./results/ecospat/camp.RDS")
camp <- subset(camp, comparison == "potential_sampled")
maas <- readRDS("./results/ecospat/maas.RDS")
maas <- subset(maas, comparison == "potential_sampled")

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

# Calculate stats for centroid distance for full vs sampled distribution
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
# Save data
saveRDS(df, "./results/ecospat/ecospat-summary-potential.RDS")

# Calculate stats for unfilling for full vs sampled distribution
df <- data.frame(Interval = rep(c("Santonian", "Campanian", "Maastrichtian"), each = 2),
                 n = c(rep(nrow(readRDS("./results/ecospat/sant.RDS")) / 3, each = 2),
                       rep(nrow(readRDS("./results/ecospat/camp.RDS")) / 3, each = 2),
                       rep(nrow(readRDS("./results/ecospat/maas.RDS")) / 3, each = 2)),
                 metric = rep(c("Niche unfilling", "Centroid distance"), times = 3))

sant <- readRDS("./results/ecospat/sant.RDS")
sant <- subset(sant, comparison == "occupied_sampled")
camp <- readRDS("./results/ecospat/camp.RDS")
camp <- subset(camp, comparison == "occupied_sampled")
maas <- readRDS("./results/ecospat/maas.RDS")
maas <- subset(maas, comparison == "occupied_sampled")

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

# Calculate stats for centroid distance for full vs sampled distribution
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
# Save data
saveRDS(df, "./results/ecospat/ecospat-summary-occupied.RDS")

