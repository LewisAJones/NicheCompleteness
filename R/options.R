##-----------------------------------------------------------------------##
##
## Script name: options.R
##
## Purpose of script: parameters for analyses
##
## Author: Dr Lewis Jones
##
## Last updated: 2022-12-16
##
##-----------------------------------------------------------------------##
# Define parameters -------------------------------------------------------
params <- list(
  # Stages
  stage = c("sant", "camp", "maas"),
  # Raster resolution
  res = 1,
  # Geographic extent
  ex = c(-180, 180, -90, 90),
  # Geographic coordinate system
  crs = "+proj=longlat +datum=WGS84 +no_defs",
  # Number of species to simulate
  n_species = 1E4,
  # Number of burn in steps
  burn_in = 1E3,
  # Dispersal capacity in m
  disp_opt = list(good_disp = 4E5,
                  poor_disp = 2E5),
  # Niche options (temperature values and multiplier precipitation values)  
  niche_opt = list(broad_niche = list(temp = 10, precip = 1.5),
                   narrow_niche = list(temp = 5, precip = 1.25)),
  # Buffer zone radius for sampling (unit in m)
  buffer = 5E4,
  # Plot outputs?
  plot_output = FALSE,
  # Ecospat res
  ecospat_res = 1000
)
