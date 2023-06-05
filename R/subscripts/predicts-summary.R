# Header ----------------------------------------------------------------
# Project: NicheCompleteness
# File name: predicts-summary.R
# Last updated: 2023-03-20
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/NicheCompleteness
# Load packages ---------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(miscTools)
# -----------------------------------------------------------------------
# Load data and format
sant <- readRDS("./results/predicts/sant.RDS")
sant <- bind_rows(sant)
sant$interval <- c("Santonian")

camp <- readRDS("./results/predicts/camp.RDS")
camp <- bind_rows(camp)
camp$interval <- c("Campanian")

maas <- readRDS("./results/predicts/maas.RDS")
maas <- bind_rows(maas)
maas$interval <- c("Maastrichtian")

# Bind data
data <- rbind.data.frame(sant, camp, maas)

# Set up comparison function
comparison <- function(x, interval, algorithm, known, predicted, variable) {
  df <- colMedians(x[which(x$known == known &
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
saveRDS(df, "./results/predicts/predicts-summary.RDS")

# Summary values
sub_df <- function(df, x, y, model, interval) {
  df <- df[which(df[, "known"] == x & 
                   df[, "predicted"] == y & 
                   df[, "interval"] == interval &
                   df[, "model"] == model), ]
  return(df[, c("TPR", "FPR", "TNR", "FNR")])
}

# Subset dataframes (potential and sampled)
sant_bioclim <- sub_df(df = data, x = "potential", y = "sampled", 
                       model = "BIOCLIM", interval = "Santonian")
sant_maxent <- sub_df(df = data, x = "potential", y = "sampled", 
                      model = "MAXENT", interval = "Santonian")
apply(sant_bioclim, 2, median)
apply(sant_maxent, 2, median)
camp_bioclim <- sub_df(df = data, x = "potential", y = "sampled", 
                       model = "BIOCLIM", interval = "Campanian")
camp_maxent <- sub_df(df = data, x = "potential", y = "sampled", 
                      model = "MAXENT", interval = "Campanian")
apply(camp_bioclim, 2, median)
apply(camp_maxent, 2, median)
maas_bioclim <- sub_df(df = data, x = "potential", y = "sampled", 
                       model = "BIOCLIM", interval = "Maastrichtian")
maas_maxent <- sub_df(df = data, x = "potential", y = "sampled", 
                      model = "MAXENT", interval = "Maastrichtian")
apply(maas_bioclim, 2, median)
apply(maas_maxent, 2, median)

# t-tests
t.test(x = sant_bioclim$TPR, y = sant_maxent$TPR)
t.test(x = sant_bioclim$FNR, y = sant_maxent$FNR)
t.test(x = sant_bioclim$FPR, y = sant_maxent$FPR)
t.test(x = sant_bioclim$TNR, y = sant_maxent$TNR)

t.test(x = camp_bioclim$TPR, y = camp_maxent$TPR)
t.test(x = camp_bioclim$FNR, y = camp_maxent$FNR)
t.test(x = camp_bioclim$FPR, y = camp_maxent$FPR)
t.test(x = camp_bioclim$TNR, y = camp_maxent$TNR)

t.test(x = maas_bioclim$TPR, y = maas_maxent$TPR)
t.test(x = maas_bioclim$FNR, y = maas_maxent$FNR)
t.test(x = maas_bioclim$FPR, y = maas_maxent$FPR)
t.test(x = maas_bioclim$TNR, y = maas_maxent$TNR)

# Subset dataframes (occupied and sampled)
sant_bioclim <- sub_df(df = data, x = "occupied", y = "sampled", 
                       model = "BIOCLIM", interval = "Santonian")
sant_maxent <- sub_df(df = data, x = "occupied", y = "sampled", 
                      model = "MAXENT", interval = "Santonian")
apply(sant_bioclim, 2, median)
apply(sant_maxent, 2, median)
camp_bioclim <- sub_df(df = data, x = "occupied", y = "sampled", 
                       model = "BIOCLIM", interval = "Campanian")
camp_maxent <- sub_df(df = data, x = "occupied", y = "sampled", 
                      model = "MAXENT", interval = "Campanian")
apply(camp_bioclim, 2, median)
apply(camp_maxent, 2, median)
maas_bioclim <- sub_df(df = data, x = "occupied", y = "sampled", 
                       model = "BIOCLIM", interval = "Maastrichtian")
maas_maxent <- sub_df(df = data, x = "occupied", y = "sampled", 
                      model = "MAXENT", interval = "Maastrichtian")
apply(maas_bioclim, 2, median)
apply(maas_maxent, 2, median)

# t-tests
t.test(x = sant_bioclim$TPR, y = sant_maxent$TPR)
t.test(x = sant_bioclim$FNR, y = sant_maxent$FNR)
t.test(x = sant_bioclim$FPR, y = sant_maxent$FPR)
t.test(x = sant_bioclim$TNR, y = sant_maxent$TNR)

t.test(x = camp_bioclim$TPR, y = camp_maxent$TPR)
t.test(x = camp_bioclim$FNR, y = camp_maxent$FNR)
t.test(x = camp_bioclim$FPR, y = camp_maxent$FPR)
t.test(x = camp_bioclim$TNR, y = camp_maxent$TNR)

t.test(x = maas_bioclim$TPR, y = maas_maxent$TPR)
t.test(x = maas_bioclim$FNR, y = maas_maxent$FNR)
t.test(x = maas_bioclim$FPR, y = maas_maxent$FPR)
t.test(x = maas_bioclim$TNR, y = maas_maxent$TNR)

