################################################################################
###                 Extrapolate Subsidence from LiDAR                        ###
###                        Code by HGR 1/2022                                ###
################################################################################

### Libraries ##################################################################
library(raster)
library(sf)
library(spatialEco)
library(envRaster)
library(tidyverse)

### Load Data ##################################################################
crop_extent <- extent(matrix(c(387000, 396000, 7080500, 7089500), nrow = 2, byrow = TRUE))

elev <- brick(stack(crop(raster('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/DTM_All/NEON_DTM_2017.tif'), crop_extent),
                    crop(raster('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/DTM_All/NEON_DTM_2018.tif'), crop_extent),
                    crop(raster('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/DTM_All/NEON_DTM_2019.tif'), crop_extent),
                    crop(raster('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/DTM_All/NEON_DTM_2021.tif'), crop_extent)))

eml_tpi <- tpi(elev[[1]], scale = 151)
# writeRaster(eml_tpi, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/subsidence_gap_filling/eml_tpi_151.tif')
# writeRaster(eml_tpi, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/subsidence_gap_filling/eml_tpi_101.tif')
eml_tpi <- raster('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/subsidence_gap_filling/eml_tpi_101.tif')
plot(eml_tpi)
ridges <- eml_tpi
ridges[ridges <= 3 | ridges >= 5] <- NA
plot(ridges)

high_elev <- elev[[1]]
high_elev[high_elev < 900] <- NA
# plot(high_elev)

ridges <- mask(ridges, high_elev)
ridges[!is.na(ridges)] <- 1
plot(ridges)



cip_bnd <- read_sf('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/CiPEHR_bnd_WGS84.shp') %>%
  st_transform(crs(elev))
crs(cip_bnd)

elev_cip <- crop(elev, cip_bnd)

test1 <- elev_cip[[2]] - elev_cip[[1]]
cellStats(test1, mean)
cellStats(test1, median)
test2 <- elev_cip[[3]] - elev_cip[[1]]
cellStats(test2, mean)
cellStats(test2, median)
test3 <- elev_cip[[4]] - elev_cip[[1]]
cellStats(test3, mean)
cellStats(test3, median)

plot(test1)
plot(test2)
test3[test3 < -3] <- NA
plot(test3)
