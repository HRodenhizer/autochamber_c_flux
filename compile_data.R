#############################################################################################################################
###                           Compile Flux Data and Environmental Variables for Modeling                                  ###
###                                                code by HGR 2/2020                                                     ###
#############################################################################################################################

### Load Libraries ##########################################################################################################
library(lubridate)
library(tidypredict)
library(tidyverse)
#############################################################################################################################

### Load Data ###############################################################################################################
flux_cumulative_cip <- read.csv("Z:/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/Multiyear_Summaries/2009_2019/Flux_cumulative_2009_2019.csv")
flux_cumulative_dry <- read.csv("Z:/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/Multiyear_Summaries/2012_2019_Drypehr/Flux_cumulative_DryPEHR_2009_2019.csv")
flux_cumulative <- rbind.data.frame(flux_cumulative_cip, flux_cumulative_dry)
alt_sub <- read.table("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/ALT_Subsidence_Corrected_2009_2018.txt",
                      sep = '\t',
                      header = TRUE) %>%
  select(-geometry) %>%
  filter(exp == 'CiPEHR')
wtd <- read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/WTD/Compiled/WTD_2018_compiled.csv")

# automatically import and merge all of the moisture data from the soil sensors
# 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/soil_sensors/CiPEHR'
import_soil_moisture <- function(folder) {
  filenames <- list.files(paste(folder),
                          full.names = TRUE,
                          pattern = '.Rdata$')
  df <- data.frame()
  for (i in 1:length(filenames)) {
    year_start <- i + 2007
    year_end <- i + 2008
    variable_name <- load(filenames[[which(str_detect(filenames, pattern = paste(year_start, '.+', year_end, sep = '')))]]) # load the data and save the variable name string to it's own variable name
    temp_data <- get(variable_name) %>% # retrieve the data associated with the variable created in the previous line
      filter(as_date(date) >= as_date(paste(year_end, '_05_01', sep = '')) & as_date(date) <= as_date(paste(year_end, '_09_30', sep = ''))) %>%
      mutate(treatment = ifelse(plot == 2 | plot == 4,
                                'Control',
                                ifelse(plot == 1 | plot == 3,
                                       'Air Warming',
                                       ifelse(plot == 6 | plot == 8,
                                              'Soil Warming',
                                              'Air + Soil Warming'))),
             mean_GWC = ifelse(is.nan(mean_GWC),
                               NA,
                               mean_GWC),
             mean_VWC = ifelse(is.nan(mean_VWC),
                               NA,
                               mean_VWC))
    df <- df %>%
      rbind.data.frame(temp_data)
  }
  return(df)
}
moisture <- import_soil_moisture('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/soil_sensors/CiPEHR')

biomass <- read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/biomass/CiPEHR/EML_AK_CiPEHR_BiomassBySpecies_2009-2013__20150320_VGS.csv") %>%
  rbind.data.frame(read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/biomass/CiPEHR/CiPEHR_biomass_2017.csv")) %>%
  group_by(year, fence, plot) %>%
  summarise(biomass = sum(biomass, na.rm = TRUE))

ndvi <- read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/ndvi/NDVI_CiPEHR&DryPEHR_2019_Datacheck.csv") %>%
  filter(year != 2009 & year != 2012 & year != 2013) %>%
  group_by(year, block, fence, plot) %>%
  summarise(peak_NDVI = max(NDVI_relative, na.rm = TRUE)) %>%
  filter(!is.na(as.numeric(as.character(plot)))) %>%
  mutate(plot = as.numeric(as.character(plot))) %>%
  arrange(year, fence, plot)

filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/chamber_temps/',
                        full.names = TRUE)
t_chamb_daily <- map_dfr(filenames,
                         ~ read.csv(.x) %>%
                           mutate(date = ifelse(year < 2017, # some years have date or DOY instead of doy, this makes sure both are present (one still empty), so that grouping works
                                                NA,
                                                parse_date_time(as.character(date), c('%Y-%m-%d', '%m/%d/%Y', '%m/%d/%y'))),
                                  doy = ifelse(year >= 2017,
                                               yday(as_datetime(date)),
                                               ifelse(year <= 2015 & plot >= 9,
                                                      DOY,
                                                      doy))) %>%
                           group_by(year, doy, fence, plot) %>%
                           summarise(mean.t.chamb = mean(Tchamb_fill, na.rm = TRUE),
                                     min.t.chamb = min(Tchamb_fill, na.rm = TRUE),
                                     max.t.chamb = max(Tchamb_fill, na.rm = TRUE)) %>%
                           ungroup()) %>%
  mutate(date = as.Date(doy, origin = paste(as.character(year-1), '-12-31', sep = ''))) %>%
  arrange(year, doy, fence, plot) %>%
  filter(!(is.nan(mean.t.chamb) & min.t.chamb == Inf & max.t.chamb == -Inf)) 
#############################################################################################################################

### Gap Fill Biomass Data with NDVI #########################################################################################
bio_ndvi <- biomass %>%
  full_join(ndvi, by = c('year', 'fence', 'plot')) %>%
  mutate(block = ifelse(fence <= 2,
                        'A',
                        ifelse(fence <= 4,
                               'B',
                               'C')),
         treatment = ifelse(plot == 2 | plot == 4,
                            'Control',
                            ifelse(plot == 1 | plot == 3,
                                   'Air Warming',
                                   ifelse(plot == 6 | plot == 8,
                                          'Soil Warming',
                                          'Air + Soil warming')))) %>%
  filter(!is.na(peak_NDVI))

bio_ndvi_regress <- bio_ndvi %>%
  filter(!is.na(biomass))

ggplot(bio_ndvi_regress, aes(x = peak_NDVI, y = biomass, colour = treatment)) +
  geom_point()

ggplot(bio_ndvi_regress, aes(x = peak_NDVI, y = biomass, colour = year)) +
  geom_point()

ggplot(bio_ndvi, aes(x = year, y = peak_NDVI, colour = treatment)) +
  geom_point()

ggplot(bio_ndvi, aes(x = year, y = biomass, colour = treatment)) +
  geom_point()

bio_lm <- lm(biomass ~ peak_NDVI, bio_ndvi_regress)
summary(bio_lm)

bio_ndvi_filled <- bio_ndvi %>%
  tidypredict_to_column(bio_lm, add_interval = TRUE)

ggplot(bio_ndvi_filled, aes(x = biomass, y = fit, colour = treatment)) +
  geom_point()
#############################################################################################################################

### Calculate Growing Season Temperature Indices ############################################################################

#############################################################################################################################