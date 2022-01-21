################################################################################
###                 Extrapolate Subsidence from LiDAR                        ###
###                        Code by HGR 1/2022                                ###
################################################################################

### To Do
# Figure out thaw depths on date of LiDAR flights to see if that could explain
# some of the offsets

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
neon_offsets <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/lidar_subsidence/neon_gps_offsets.csv')

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
ridges_sp <- ridges %>%
  as.data.frame(xy = TRUE) %>%
  filter(!is.na(eml_tpi_101)) %>%
  st_as_sf(coords = c('x', 'y'), crs = crs(elev)) %>%
  as("Spatial")

# retrieve cells within ridges mask in all years
ridge_offsets <- raster::extract(elev, ridges_df) %>%
  as.data.frame() %>%
  mutate(offset_17_17 = 0,
         offset_18_17 = NEON_DTM_2018 - NEON_DTM_2017,
         offset_19_17 = NEON_DTM_2019 - NEON_DTM_2017,
         offset_21_17 = NEON_DTM_2021 - NEON_DTM_2017)

ridge_offsets_summary <- ridge_offsets %>%
  summarise(across(.cols = offset_17_17:offset_21_17, .fns = ~ mean(.x))) %>%
  pivot_longer(offset_17_17:offset_21_17, names_to = 'year', values_to = "offset") %>%
  mutate(year = c(2017, 2018, 2019, 2021))

# correct elevation using neon - gps offsets when we have them (2017-2019), and ridge offsets when we don't (2021)
elev_ridge_corrected <- elev + c(neon_offsets$mean.neon.offset - neon_offsets$mean.neon.offset[1], ridge_offsets_summary$offset[4])

cip_bnd <- read_sf('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/CiPEHR_bnd_WGS84.shp') %>%
  st_transform(crs(elev))

elev_cip <- crop(elev_ridge_corrected, cip_bnd)

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
test3[test3 < -4] <- NA
plot(test3)

plots <- read_sf('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/plot_coordinates_from_2017.shp') %>%
  st_transform(crs = crs(elev))

# extract elevation values from 2021
plot_elev_2021 <- raster::extract(elev_cip[[c(1, 4)]], as(plots, "Spatial")) %>%
  as.data.frame() %>%
  rename(elev.lidar.17 = 1, elev.lidar.21 = 2) %>%
  cbind.data.frame(plots) %>%
  mutate(year = 2021,
         treatment = case_when(plot %in% c(2, 4) ~ "Control",
                               plot %in% c(1, 3) ~ "Air Warming",
                               plot %in% c(6, 8) ~ "Soil Warming",
                               plot %in% c(5, 7) ~ "Air + Soil Warming"),
         sub.17.21 = elev.lidar.21 - elev.lidar.17) %>%
  select(year, fence, plot, treatment, elev.lidar.17, elev.lidar.21, sub.17.21) 

plot(test3)
plot(plots, add = TRUE)

# write.csv(plot_elev_2021, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/elevation/plot_elev_2021_lidar.csv',
#           row.names = FALSE)
