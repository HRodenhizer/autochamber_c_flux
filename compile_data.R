#############################################################################################################################
###                           Compile Flux Data and Environmental Variables for Modeling                                  ###
###                                                code by HGR 2/2020                                                     ###
#############################################################################################################################

### Load Libraries ##########################################################################################################
library(tidyverse)
#############################################################################################################################

### Load Data ###############################################################################################################
flux_cumulative_cip <- read.csv("Z:/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/Multiyear_Summaries/2009_2019/Flux_cumulative_2009_2019.csv")
flux_cumulative_dry <- read.csv("Z:/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/Multiyear_Summaries/2012_2019_Drypehr/Flux_cumulative_DryPEHR_2009_2019.csv")
flux_cumulative <- rbind.data.frame(flux_cumulative_cip, flux_cumulative_dry)
alt_sub <- read.table("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/ALT_Subsidence_Corrected_2009_2018.txt",
                      sep = '\t',
                      header = TRUE) %>%
  select(-geometry)
wtd <- read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/WTD/Compiled/WTD_2018_compiled.csv")

# this next part is a work in progress. I need to figure out why NaN keeps showing up after summarizing, despite switching them to NA
# and then removing NA's when summarizing
# automatically import and merge all of the moisture data from the soil sensors
filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/soil_sensors/CiPEHR',
                        full.names = TRUE,
                        pattern = '.Rdata$')
moisture <- data.frame()
for (i in 1:length(filenames)) {
  year_start <- i + 2007
  year_end <- i + 2008
  variable_name <- paste('cipehr.', year_start, '.', year_end, '.daily', sep = '')
  load(filenames[[which(str_detect(filenames, pattern = paste(year_start, '.+', year_end, sep = '')))]])
  temp_data <- get(variable_name) %>%
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
                             mean_VWC)) %>%
    group_by(year, treatment, Depth) %>%
    summarise(annual_mean_GWC = mean(mean_GWC, na.rm = TRUE),
              annual_se_GWC = sd(mean_GWC, na.rm = TRUE)/sqrt(n()),
              annual_mean_VWC = mean(mean_VWC, na.rm = TRUE),
              annual_se_VWC = sd(mean_VWC, na.rm = TRUE)/sqrt(n()))
  moisture <- moisture %>%
    rbind.data.frame(temp_data)
}

test <- read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/soil_sensors/CiPEHR/CiPEHR soil sensor_2009-10-01_to_2010-09-30_daily.csv")
load("Z:/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/Soil sensors/2018-2019/DryPEHR/Processed/DryPEHR soil sensor_2018-10-01_to_2019-09-30_half_hourly Bioserver.Rdata")
#############################################################################################################################