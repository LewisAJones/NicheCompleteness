# -----------------------------------------------------------------------
# Project: NicheCompleteness
# File name: dismo-summary.R
# Last updated: 2023-02-27
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/NicheCompleteness
# -----------------------------------------------------------------------
# Load packages ---------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
# -----------------------------------------------------------------------
# Load data and format
sant <- readRDS("./results/dismo/sant.RDS")
sant <- bind_rows(sant)
sant$interval <- c("Santonian")

camp <- readRDS("./results/dismo/camp.RDS")
camp <- bind_rows(camp)
camp$interval <- c("Campanian")

maas <- readRDS("./results/dismo/maas.RDS")
maas <- bind_rows(maas)
maas$interval <- c("Maastrichtian")

# Bind data
data <- rbind.data.frame(sant, camp, maas)

# Set up comparison function
comparison <- function(x, interval, algorithm, known, predicted, variable) {
  df <- colMeans(x[which(x$known == known &
                         x$predicted == predicted &
                         x$interval == interval &
                         x$model == algorithm), variable])
  df <- t(data.frame(df))
  df <- cbind.data.frame(interval, algorithm, known, predicted, df)
  return(df)
}

# BIOCLIM comparisons

# Compare potential and sampled
sant_pot_occ <- comparison(x = data,
                           interval = "Santonian",
                           algorithm = "BIOCLIM",
                           known = "potential",
                           predicted = "occupied",
                           variable = c("TPR", "FPR", "TNR", "FNR"))

camp_pot_occ <- comparison(x = data,
                           interval = "Campanian",
                           algorithm = "BIOCLIM",
                           known = "potential",
                           predicted = "occupied",
                           variable = c("TPR", "FPR", "TNR", "FNR"))

maas_pot_occ <- comparison(x = data,
                           interval = "Maastrichtian",
                           algorithm = "BIOCLIM",
                           known = "potential",
                           predicted = "occupied",
                           variable = c("TPR", "FPR", "TNR", "FNR"))

# Compare potential and sampled
sant_pot_samp <- comparison(x = data,
                           interval = "Santonian",
                           algorithm = "BIOCLIM",
                           known = "potential",
                           predicted = "sampled",
                           variable = c("TPR", "FPR", "TNR", "FNR"))

camp_pot_samp <- comparison(x = data,
                           interval = "Campanian",
                           algorithm = "BIOCLIM",
                           known = "potential",
                           predicted = "sampled",
                           variable = c("TPR", "FPR", "TNR", "FNR"))

maas_pot_samp <- comparison(x = data,
                           interval = "Maastrichtian",
                           algorithm = "BIOCLIM",
                           known = "potential",
                           predicted = "sampled",
                           variable = c("TPR", "FPR", "TNR", "FNR"))

# Compare occupied and occupied
sant_occ_occ <- comparison(x = data,
                           interval = "Santonian",
                           algorithm = "BIOCLIM",
                           known = "occupied",
                           predicted = "occupied",
                           variable = c("TPR", "FPR", "TNR", "FNR"))

camp_occ_occ <- comparison(x = data,
                           interval = "Campanian",
                           algorithm = "BIOCLIM",
                           known = "occupied",
                           predicted = "occupied",
                           variable = c("TPR", "FPR", "TNR", "FNR"))

maas_occ_occ <- comparison(x = data,
                           interval = "Maastrichtian",
                           algorithm = "BIOCLIM",
                           known = "occupied",
                           predicted = "occupied",
                           variable = c("TPR", "FPR", "TNR", "FNR"))

# Compare occupied and sampled
sant_occ_samp <- comparison(x = data,
                            interval = "Santonian",
                            algorithm = "BIOCLIM",
                            known = "occupied",
                            predicted = "sampled",
                            variable = c("TPR", "FPR", "TNR", "FNR"))

camp_occ_samp <- comparison(x = data,
                            interval = "Campanian",
                            algorithm = "BIOCLIM",
                            known = "occupied",
                            predicted = "sampled",
                            variable = c("TPR", "FPR", "TNR", "FNR"))

maas_occ_samp <- comparison(x = data,
                            interval = "Maastrichtian",
                            algorithm = "BIOCLIM",
                            known = "occupied",
                            predicted = "sampled",
                            variable = c("TPR", "FPR", "TNR", "FNR"))

bc <- rbind.data.frame(sant_pot_occ, sant_pot_samp, sant_occ_occ, sant_occ_samp,
                       camp_pot_occ, camp_pot_samp, camp_occ_occ, camp_occ_samp,
                       maas_pot_occ, maas_pot_samp, maas_occ_occ, maas_occ_samp)

# MAXENT COMPARISONS

# Compare potential and sampled
sant_pot_occ <- comparison(x = data,
                           interval = "Santonian",
                           algorithm = "MAXENT",
                           known = "potential",
                           predicted = "occupied",
                           variable = c("TPR", "FPR", "TNR", "FNR"))

camp_pot_occ <- comparison(x = data,
                           interval = "Campanian",
                           algorithm = "MAXENT",
                           known = "potential",
                           predicted = "occupied",
                           variable = c("TPR", "FPR", "TNR", "FNR"))

maas_pot_occ <- comparison(x = data,
                           interval = "Maastrichtian",
                           algorithm = "MAXENT",
                           known = "potential",
                           predicted = "occupied",
                           variable = c("TPR", "FPR", "TNR", "FNR"))

# Compare potential and sampled
sant_pot_samp <- comparison(x = data,
                            interval = "Santonian",
                            algorithm = "MAXENT",
                            known = "potential",
                            predicted = "sampled",
                            variable = c("TPR", "FPR", "TNR", "FNR"))

camp_pot_samp <- comparison(x = data,
                            interval = "Campanian",
                            algorithm = "MAXENT",
                            known = "potential",
                            predicted = "sampled",
                            variable = c("TPR", "FPR", "TNR", "FNR"))

maas_pot_samp <- comparison(x = data,
                            interval = "Maastrichtian",
                            algorithm = "MAXENT",
                            known = "potential",
                            predicted = "sampled",
                            variable = c("TPR", "FPR", "TNR", "FNR"))

# Compare occupied and occupied
sant_occ_occ <- comparison(x = data,
                           interval = "Santonian",
                           algorithm = "MAXENT",
                           known = "occupied",
                           predicted = "occupied",
                           variable = c("TPR", "FPR", "TNR", "FNR"))

camp_occ_occ <- comparison(x = data,
                           interval = "Campanian",
                           algorithm = "MAXENT",
                           known = "occupied",
                           predicted = "occupied",
                           variable = c("TPR", "FPR", "TNR", "FNR"))

maas_occ_occ <- comparison(x = data,
                           interval = "Maastrichtian",
                           algorithm = "MAXENT",
                           known = "occupied",
                           predicted = "occupied",
                           variable = c("TPR", "FPR", "TNR", "FNR"))

# Compare occupied and sampled
sant_occ_samp <- comparison(x = data,
                            interval = "Santonian",
                            algorithm = "MAXENT",
                            known = "occupied",
                            predicted = "sampled",
                            variable = c("TPR", "FPR", "TNR", "FNR"))

camp_occ_samp <- comparison(x = data,
                            interval = "Campanian",
                            algorithm = "MAXENT",
                            known = "occupied",
                            predicted = "sampled",
                            variable = c("TPR", "FPR", "TNR", "FNR"))

maas_occ_samp <- comparison(x = data,
                            interval = "Maastrichtian",
                            algorithm = "MAXENT",
                            known = "occupied",
                            predicted = "sampled",
                            variable = c("TPR", "FPR", "TNR", "FNR"))

mx <- rbind.data.frame(sant_pot_occ, sant_pot_samp, sant_occ_occ, sant_occ_samp,
                       camp_pot_occ, camp_pot_samp, camp_occ_occ, camp_occ_samp,
                       maas_pot_occ, maas_pot_samp, maas_occ_occ, maas_occ_samp)

df <- rbind.data.frame(bc, mx)
row.names(df) <- NULL
saveRDS(df, "./results/dismo/dismo-summary.RDS")

# t test comparisons
# Potential and sampled (TPR & FNR)
sant <- data[which(data$known == "potential" & 
                       data$predicted == "sampled" &
                       data$interval == "Santonian" &
                       data$model == "BIOCLIM"),]
camp <- data[which(data$known == "potential" & 
                           data$predicted == "sampled" &
                           data$interval == "Campanian" &
                           data$model == "BIOCLIM"),]
maas <- data[which(data$known == "potential" & 
                       data$predicted == "sampled" &
                       data$interval == "Maastrichtian" &
                       data$model == "BIOCLIM"),]
t.test(x = sant$TPR, y = camp$TPR)
t.test(x = sant$TPR, y = maas$TPR)
t.test(x = sant$FNR, y = camp$FNR)
t.test(x = sant$FNR, y = maas$FNR)
