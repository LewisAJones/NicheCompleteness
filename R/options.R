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
#-------Simulations options---------
res = 1E5 #100 km, resolution of study
ex = c(-180, 180, -90, 90) #geographic extent of study
gcrs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" #WGS 84 geographic coordinate system
prj = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84" #Mollweide projection system

n_species = 1E4 #10,000 species
burn_in = 1E3 #1,000 time steps for burn in stage

disp_cap = c("good_disp", "poor_disp") #define dispersal capacities
  good_disp = 4 #definition of a good disperser
  poor_disp = 2 #definition of a poor disperser
niche_opt = c("broad", "narrow") #define of niche options
  broad = c("temp" = 10, "precip" = 5) #definition of a broad niche
  narrow = c("temp" = 5, "precip" = 2.5) #definition of a narrow niche

#-------Ecospat options---------
ecospat_res = 100 
  
#-------ENMTools options---------
tp = 0.1 #test proportion  
  
#-------Plotting options---------  
#colour palette
col_pal = MetBrewer::met.brewer(name = "Hiroshige", n = 10, type = "discrete")
