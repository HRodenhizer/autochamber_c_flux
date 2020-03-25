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
alt_sub <- read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/ALT_Subsidence_Corrected_2009_2019.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE) %>%
  filter(exp == 'CiPEHR') %>%
  select(-exp) %>%
  mutate(plot = as.numeric(plot),
         block = toupper(block))
wtd <- read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/WTD/Compiled/WTD_2019_compiled.csv",
                stringsAsFactors = FALSE)

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

biomass <- read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/biomass/CiPEHR/EML_AK_CiPEHR_BiomassBySpecies_2009-2013__20150320_VGS.csv",
                    stringsAsFactors = FALSE) %>%
  rbind.data.frame(read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/biomass/CiPEHR/CiPEHR_biomass_2017.csv",
                            stringsAsFactors = FALSE)) %>%
  group_by(year, fence, plot) %>%
  summarise(biomass = sum(biomass, na.rm = TRUE))

ndvi <- read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/ndvi/NDVI_CiPEHR&DryPEHR_2019_Datacheck.csv",
                 stringsAsFactors = FALSE) %>%
  filter(year != 2009 & year != 2012 & year != 2013) %>%
  group_by(year, block, fence, plot) %>%
  summarise(peak_NDVI = max(NDVI_relative, na.rm = TRUE)) %>%
  filter(!is.na(as.numeric(plot))) %>%
  mutate(plot = as.numeric(plot)) %>%
  arrange(year, fence, plot)

filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/chamber_temps/',
                        full.names = TRUE)
t_chamb_daily <- map_dfr(filenames,
                         ~ read.csv(.x,
                                    stringsAsFactors = FALSE) %>%
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
  filter(!(is.nan(mean.t.chamb) & min.t.chamb == Inf & max.t.chamb == -Inf))  %>%
  filter(plot <= 8)
#############################################################################################################################

### Calculate Growing Season Temperature Indices ############################################################################
# mean daily mean
# mean daily min
# mean daily max
# min daily min?
# max daily max?

t_chamb_annual <- t_chamb_daily %>%
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
                                          'Air + Soil Warming')))) %>%
  group_by(year, block, fence, plot, treatment) %>%
  summarise(mean.t.chamb.mean = mean(mean.t.chamb),
            se.t.chamb.mean = sd(mean.t.chamb)/sqrt(n()),
            mean.t.chamb.max = mean(max.t.chamb),
            max.t.chamb.max = max(max.t.chamb),
            se.t.chamb.max = sd(max.t.chamb)/sqrt(n()),
            mean.t.chamb.min = mean(min.t.chamb),
            min.t.chamb.min = min(min.t.chamb),
            se.t.chamb.min = sd(min.t.chamb)/sqrt(n()))

ggplot(t_chamb_annual, aes(x = year, colour = treatment)) +
  geom_point(aes(y = mean.t.chamb.mean)) +
  geom_point(aes(y = mean.t.chamb.min)) +
  geom_point(aes(y = mean.t.chamb.max)) +
  geom_point(aes(y = min.t.chamb.min)) +
  geom_point(aes(y = max.t.chamb.max))
  
ggplot(t_chamb_annual, aes(x = year, y = mean.t.chamb.mean, colour = treatment)) +
  geom_point() +
  ggtitle('Mean Growing Season Temperature')

ggplot(t_chamb_annual, aes(x = year, y = mean.t.chamb.min, colour = treatment)) +
  geom_point() +
  ggtitle('Mean Growing Season Daily Min Temperature')

ggplot(t_chamb_annual, aes(x = year, y = mean.t.chamb.max, colour = treatment)) +
  geom_point() +
  ggtitle('Mean Growing Season Daily Max Temperature')

ggplot(t_chamb_annual, aes(x = year, y = min.t.chamb.min, colour = treatment)) +
  geom_point() +
  ggtitle('Min Growing Season Temperature')

ggplot(t_chamb_annual, aes(x = year, y = max.t.chamb.max, colour = treatment)) +
  geom_point() +
  ggtitle('Max Growing Season Temperature')
#############################################################################################################################

### Calculate Growing Season Soil Sensor Indices ############################################################################
moisture_annual <- moisture %>%
  mutate(fence = as.numeric(fence),
         plot = as.numeric(plot)) %>%
  group_by(year, fence, plot, treatment) %>%
  summarise(mean.GWC = mean(mean_GWC, na.rm = TRUE),
            se.GWC = sd(mean_GWC, na.rm = TRUE)/sqrt(n()),
            mean.VWC = mean(mean_VWC, na.rm = TRUE),
            se.VWC = sd(mean_VWC, na.rm = TRUE)/sqrt(n()))

soil_temp_annual <- moisture %>%
  mutate(fence = as.numeric(fence),
         plot = as.numeric(plot),
         mean_t = ifelse(is.nan(mean_t) | mean_t == Inf | mean_t == -Inf,
                         NA,
                         mean_t),
         t.max = ifelse(is.nan(max.t) | max.t == Inf | max.t == -Inf,
                       NA,
                       max.t),
         t.min = ifelse(is.nan(min.t) | min.t == Inf | min.t == -Inf,
                       NA,
                       min.t)) %>%
  group_by(year, fence, plot, treatment, Depth) %>%
  summarise(mean.t = mean(mean_t, na.rm = TRUE),
            se.t = sd(mean_t, na.rm = TRUE)/sqrt(n()),
            max.t = max(t.max, na.rm = TRUE),
            se.t.max = sd(t.max, na.rm = TRUE)/sqrt(n()),
            min.t = min(t.min, na.rm = TRUE),
            se.t.min = sd(t.min, na.rm = TRUE)/sqrt(n()))

soil_temp_5_annual <- soil_temp_annual %>%
  filter(Depth == 'T_five') %>%
  select(year, fence, plot, treatment, mean.t.5 = mean.t, se.mean.t.5 = se.t, max.t.5 = max.t, se.max.t.5 = se.t.max, min.t.5 = min.t, se.min.t.5 = se.t.min)

soil_temp_10_annual <- soil_temp_annual %>%
  filter(Depth == 'T_ten') %>%
  select(year, fence, plot, treatment, mean.t.10 = mean.t, se.mean.t.10 = se.t, max.t.10 = max.t, se.max.t.10 = se.t.max, min.t.10 = min.t, se.min.t.10 = se.t.min)

soil_temp_20_annual <- soil_temp_annual %>%
  filter(Depth == 'T_twenty') %>%
  select(year, fence, plot, treatment, mean.t.20 = mean.t, se.mean.t.20 = se.t, max.t.20 = max.t, se.max.t.20 = se.t.max, min.t.20 = min.t, se.min.t.20 = se.t.min)

soil_temp_40_annual <- soil_temp_annual %>%
  filter(Depth == 'T_forty') %>%
  select(year, fence, plot, treatment, mean.t.40 = mean.t, se.mean.t.40 = se.t, max.t.40 = max.t, se.max.t.40 = se.t.max, min.t.40 = min.t, se.min.t.40 = se.t.min)
#############################################################################################################################

### Calculate Growing season WTD Indices ####################################################################################
wtd_monthly <- wtd %>%
  mutate(month = month(date)) %>%
  group_by(year, block, fence, well, treatment, month) %>%
  summarise(mean.wtd = mean(WTD, na.rm = TRUE),
            se.wtd = sd(WTD, na.rm = TRUE)/sqrt(n()))

wtd_annual <- wtd %>%
  mutate(month = month(date)) %>%
  group_by(year, block, fence, well, treatment) %>%
  summarise(mean.wtd = mean(WTD, na.rm = TRUE),
            se.wtd = sd(WTD, na.rm = TRUE)/sqrt(n())) %>%
  full_join(wtd_monthly %>%
                     filter(month == 6) %>%
                     select(year, block, fence, well, treatment, mean.june.wtd = mean.wtd, se.june.wtd = se.wtd),
            by = c('year', 'block', 'fence', 'well', 'treatment')) %>%
  full_join(wtd_monthly %>%
              filter(month == 7) %>%
              select(year, block, fence, well, treatment, mean.july.wtd = mean.wtd, se.july.wtd = se.wtd),
            by = c('year', 'block', 'fence', 'well', 'treatment')) %>%
  full_join(wtd_monthly %>%
              filter(month == 8) %>%
              select(year, block, fence, well, treatment, mean.aug.wtd = mean.wtd, se.aug.wtd = se.wtd),
            by = c('year', 'block', 'fence', 'well', 'treatment')) %>%
  full_join(wtd_monthly %>%
              filter(month == 9) %>%
              select(year, block, fence, well, treatment, mean.sept.wtd = mean.wtd, se.sept.wtd = se.wtd),
            by = c('year', 'block', 'fence', 'well', 'treatment'))
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
                                          'Air + Soil Warming'))))

bio_ndvi_regress <- bio_ndvi %>%
  filter(!is.na(peak_NDVI)) %>%
  filter(!is.na(biomass))

ggplot(bio_ndvi_regress, aes(x = peak_NDVI, y = biomass, colour = treatment)) +
  geom_point()

ggplot(bio_ndvi_regress, aes(x = peak_NDVI, y = biomass, colour = as.factor(year))) +
  geom_point()

ggplot(bio_ndvi, aes(x = year, y = peak_NDVI, colour = treatment)) +
  geom_point()

ggplot(bio_ndvi, aes(x = year, y = biomass, colour = treatment)) +
  geom_point()

bio_lm <- lm(biomass ~ peak_NDVI, bio_ndvi_regress)
summary(bio_lm)

bio_ndvi_filled <- bio_ndvi %>%
  tidypredict_to_column(bio_lm, add_interval = TRUE) %>%
  arrange(year, fence, plot) %>%
  mutate(biomass.filled = ifelse(!is.na(biomass),
                                        biomass,
                                        fit))

ggplot(bio_ndvi_filled, aes(x = biomass, y = fit, colour = treatment)) +
  geom_point()
#############################################################################################################################

### Merge Datasets ##########################################################################################################
env_var <- alt_sub %>%
  mutate(well = ifelse(year > 2013 & fence == 1 & plot == 3 |
                         year > 2013 & fence == 2 & plot == 3 |
                         year > 2013 & fence == 3 & plot == 2 |
                         year > 2013 & fence == 4 & plot == 3 |
                         year > 2013 & fence == 5 & plot == 4 |
                         year > 2013 & fence == 6 & plot == 2,
                       2.5,
                       ifelse(year > 2013 & fence == 1 & plot == 5 |
                                year > 2013 & fence == 2 & plot == 5 |
                                year > 2013 & fence == 3 & plot == 7 |
                                year > 2013 & fence == 4 & plot == 8 |
                                year > 2013 & fence == 5 & plot == 6 |
                                year > 2013 & fence == 6 & plot == 7,
                              4.5,
                              ifelse(plot == 1 | plot == 4,
                                     1,
                                     ifelse(plot == 2 | plot == 3,
                                            2,
                                            ifelse(plot == 5 | plot == 8,
                                                   3,
                                                   4)))))) %>%
  select(year, block, fence, plot, well, treatment, subsidence, ALT, thaw.penetration = ALT.corrected) %>%
  full_join(t_chamb_annual, by = c('year', 'block', 'fence', 'plot', 'treatment')) %>%
  full_join(moisture_annual, by = c('year', 'fence', 'plot', 'treatment')) %>%
  full_join(soil_temp_5_annual, by = c('year', 'fence', 'plot', 'treatment')) %>%
  full_join(soil_temp_10_annual, by = c('year', 'fence', 'plot', 'treatment')) %>%
  full_join(soil_temp_20_annual, by = c('year', 'fence', 'plot', 'treatment')) %>%
  full_join(soil_temp_40_annual, by = c('year', 'fence', 'plot', 'treatment')) %>%
  full_join(select(bio_ndvi_filled, year, block, fence, plot, treatment, biomass.filled),
            by = c('year', 'block', 'fence', 'plot', 'treatment'))

# write.csv(env_var, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/annual_environmental_data_compiled.csv')
#############################################################################################################################