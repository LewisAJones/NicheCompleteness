## -----------------------------------------------------------------------------
##
## Script name: options.R
##
## Purpose of script: provide options for main analyses
##
## Author: Dr Lewis Jones
##
## Last updated: 2022-08-17
##
# Simulations options-----------------------------------------------------------
# Resolution
res = 1E5 
# Geographic extent
ex = c(-180, 180, -90, 90)
# Geographic coordinate system
gcrs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# Number of species
n_species = 1E2
# Number of burn in steps
burn_in = 1E3
# Dispersal capacity 
disp_cap = c("good_disp", "poor_disp")
  good_disp = 4
  poor_disp = 2 
# Niche options  
niche_opt = c("broad", "narrow")
# Plot outputs?
plot_output = FALSE
# ecospat options --------------------------------------------------------------
ecospat_res = 100 
  
# dismo options ----------------------------------------------------------------

  
