## ---------------------------
##
## Script name: var-plots.R
##
## Purpose of script: plot climatic variables
##
## Author: Dr Lewis Jones
##
## Date Created: 2022-03-25
##
## Copyright (c) Lewis Jones, 2022
## Email: LewisA.Jones@outlook.com
##
#---------Load packages-----------
library(MetBrewer)
library(rasterVis)
library(raster)
#---------------------------------
#stack rasters
sant <- stack(list.files(paste0("./data/climate/sant/"), pattern = ".grd", full.names = TRUE))
camp <- stack(list.files(paste0("./data/climate/camp/"), pattern = ".grd", full.names = TRUE))
maas <- stack(list.files(paste0("./data/climate/maas/"), pattern = ".grd", full.names = TRUE))

#----------Santonian--------------
p1 <- levelplot(sant$max_precip, margin = list(FUN = 'median'), contour=TRUE, 
          main= "Maximum precipitation (mm/day)",
          xlab = "Eastings (m)",
          ylab = "Northings (m)",
          col.regions = rev(MetBrewer::met.brewer("Hiroshige", n = 100, type = c("continuous"))))

p2 <- levelplot(sant$min_precip, margin = list(FUN = 'median'), contour=TRUE, 
          main= "Minimum precipitation (mm/day)",
          xlab = "Eastings (m)",
          ylab = "Northings (m)",
          col.regions = rev(MetBrewer::met.brewer("Hiroshige", n = 100, type = c("continuous"))))

p3 <- levelplot(sant$max_temp, margin = list(FUN = 'median'), contour=TRUE, 
          main= "Maximum temperature (ºC)",
          xlab = "Eastings (m)",
          ylab = "Northings (m)",
          col.regions = rev(MetBrewer::met.brewer("Hiroshige", n = 100, type = c("continuous"))))

p4 <- levelplot(sant$min_temp, margin = list(FUN = 'median'), contour=TRUE, 
          main= "Minimum Temperature (ºC)",
          xlab = "Eastings (m)",
          ylab = "Northings (m)",
          col.regions = rev(MetBrewer::met.brewer("Hiroshige", n = 100, type = c("continuous"))))

p <- grid.arrange(p1, p2, p3, p4)
ggsave("./figures/sant_vars.png", p, width = 200, height = 150, units = "mm", scale = 2.5)

#----------Campanian--------------
p1 <- levelplot(camp$max_precip, margin = list(FUN = 'median'), contour=TRUE, 
                main= "Maximum precipitation (mm/day)",
                xlab = "Eastings (m)",
                ylab = "Northings (m)",
                col.regions = rev(MetBrewer::met.brewer("Hiroshige", n = 100, type = c("continuous"))))

p2 <- levelplot(camp$min_precip, margin = list(FUN = 'median'), contour=TRUE, 
                main= "Minimum precipitation (mm/day)",
                xlab = "Eastings (m)",
                ylab = "Northings (m)",
                col.regions = rev(MetBrewer::met.brewer("Hiroshige", n = 100, type = c("continuous"))))

p3 <- levelplot(camp$max_temp, margin = list(FUN = 'median'), contour=TRUE, 
                main= "Maximum temperature (ºC)",
                xlab = "Eastings (m)",
                ylab = "Northings (m)",
                col.regions = rev(MetBrewer::met.brewer("Hiroshige", n = 100, type = c("continuous"))))

p4 <- levelplot(camp$min_temp, margin = list(FUN = 'median'), contour=TRUE, 
                main= "Minimum Temperature (ºC)",
                xlab = "Eastings (m)",
                ylab = "Northings (m)",
                col.regions = rev(MetBrewer::met.brewer("Hiroshige", n = 100, type = c("continuous"))))

p <- grid.arrange(p1, p2, p3, p4)
ggsave("./figures/camp_vars.png", p, width = 200, height = 150, units = "mm", scale = 2.5)

#----------Maastrichtian--------------
p1 <- levelplot(maas$max_precip, margin = list(FUN = 'median'), contour=TRUE, 
                main= "Maximum precipitation (mm/day)",
                xlab = "Eastings (m)",
                ylab = "Northings (m)",
                col.regions = rev(MetBrewer::met.brewer("Hiroshige", n = 100, type = c("continuous"))))

p2 <- levelplot(maas$min_precip, margin = list(FUN = 'median'), contour=TRUE, 
                main= "Minimum precipitation (mm/day)",
                xlab = "Eastings (m)",
                ylab = "Northings (m)",
                col.regions = rev(MetBrewer::met.brewer("Hiroshige", n = 100, type = c("continuous"))))

p3 <- levelplot(maas$max_temp, margin = list(FUN = 'median'), contour=TRUE, 
                main= "Maximum temperature (ºC)",
                xlab = "Eastings (m)",
                ylab = "Northings (m)",
                col.regions = rev(MetBrewer::met.brewer("Hiroshige", n = 100, type = c("continuous"))))

p4 <- levelplot(maas$min_temp, margin = list(FUN = 'median'), contour=TRUE, 
                main= "Minimum Temperature (ºC)",
                xlab = "Eastings (m)",
                ylab = "Northings (m)",
                col.regions = rev(MetBrewer::met.brewer("Hiroshige", n = 100, type = c("continuous"))))

p <- grid.arrange(p1, p2, p3, p4)
ggsave("./figures/maas_vars.png", p, width = 200, height = 150, units = "mm", scale = 2.5)
