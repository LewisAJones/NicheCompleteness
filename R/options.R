## ---------------------------
##
## Script name: options.R
##
## Purpose of script: provide options for main analyses
##
## Author: Dr Lewis Jones
##
## Date Created: 2022-03-04
##
## Copyright (c) Lewis Jones, 2022
## Email: LewisA.Jones@outlook.com
##
#-------Analyses options---------
res = 1E5 #100 km, resolution of study
ex = c(-180, 180, -90, 90) #geographic extent of study
gcrs = sp::CRS("EPSG:4326") #WGS 84 geographic coordinate system
prj = sp::CRS("ESRI:54012") #EckertIV projection system

n_species = 1E4 #10,000 species
burn_in = 1E3 #1,000 time steps for burn in stage

disp_cap = c("good_disp", "poor_disp") #define dispersal capacities
  good_disp = 4 #definition of a good disperser
  poor_disp = 2 #definition of a poor disperser
niche_opt = c("broad", "narrow") #define of niche options
  broad = c("temp" = 10, "precip" = 5) #definition of a broad niche
  narrow = c("temp" = 5, "precip" = 2.5) #definition of a narrow niche
