#############################################################################################################################
###                           Compile Flux Data and Environmental Variables for Modeling                                  ###
###                                                code by HGR 2/2020                                                     ###
#############################################################################################################################

### Load Libraries ##########################################################################################################
library(lubridate)
library(berryFunctions)
library(data.table)
library(readxl)
library(sf)
library(tidyselect)
library(tidyverse)
#############################################################################################################################

# make ALT be deepest TD and also incorporate day of ALT into modeling?

### co2 Data ##############################################################################
### Load Fluxes
co2 <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/fluxes/Flux_data_halfhourly_modelled_2009_2020.csv")
co2 <- co2[!is.na(doy)]

# make sure that hour is rounded down from flux time and goes from 0-23.5
# this way a measurement taken between 00:00 and 00:30 shows up as 0
co2[hour == 0, hour := 24]
co2[, hour := hour - 0.5]
co2[, doy.old := doy]
co2[, hour.old := hour]

### Load R2
# a function to load and return an Rdata file
loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# a function load all of the r2 data, whether csv or Rdata
load.r2 <- function(filenames) {
  
  data.dt <- data.table()
  
  for(i in 1:length(filenames)) {
    
    if (str_detect(filenames[i], 'csv$')) {
      
      temp <- read.table(filenames[i],
                         header = TRUE,
                         sep = ',',
                         fill = TRUE) %>%
        mutate(year = i + 2011,
               plot = as.numeric(plot),
               hourmin = ifelse(hourmin == 0,
                                24,
                                hourmin),
               hour = hourmin - 0.5) %>%
        select(year, fence, plot, doy.old = DOY, doy.new = DOY, hour.old = hour, hour.new = hour, r2 = rsquare, T_chamb = chambT.mean)
      
    } else if(str_detect(filenames[i], 'Rdata$')) {
      
      assign('temp',
             loadRData(filenames[i]) %>%
               mutate(year = ifelse(i <= 5,
                                    year(timestamp),
                                    year(timestamp1)),
                      doy.old = ifelse(year == 2018,
                                       yday(date),
                                       ifelse(year <= 2016,
                                              DOY,
                                              yday(timestamp1)))) %>%
               select(matches('timestamp'), doy.old, matches('hour')[1], fence, plot, rsquare, chambT.mean) %>%
               rename(timestamp = 1, doy.old = 2, hourmin = 3, fence = 4, plot = 5, rsquare = 6, chambT.mean = 7) %>%
               mutate(year = year(timestamp),
                      doy.new = yday(timestamp),
                      plot = as.numeric(plot),
                      hourmin = ifelse(hourmin == 0,
                                       24,
                                       hourmin),
                      hour.old = hourmin - 0.5,
                      hour.new = ifelse(year == 2018,
                                        hour(floor_date(timestamp, '30 mins')) + minute(floor_date(timestamp, '30 mins'))/60,
                                        hour.old)) %>%
               select(year, fence, plot, doy.old, doy.new, hour.old, hour.new, r2 = rsquare, T_chamb = chambT.mean)
      )
      
    } else {
      
      print('Your file does not match any of the supported file types (csv, Rdata)')
      
    }
    
    data.dt <- rbind(data.dt, temp)
    
  }
  
  return(data.dt)
  
}

filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/ITEX/warmxresp/2020_09_Dataset request/flux_data/co2/flux_slopes/',
                        pattern = 'csv$|Rdata',
                        full.names = TRUE)
r2 <- load.r2(filenames)

### Join flux and r2 data
# use doy.old because both datasets had incorrect doy in some entries in 2018
# and correct doy variable could only be made in r2 since flux didn't have a timestamp
co2 <- merge(co2, r2, by = c('year', 'doy.old', 'hour.old', 'fence', 'plot'), all.x = TRUE)
rm(r2)

### Format data
# Create doy variable with correct (new) dates
co2[year >= 2012 & !is.na(doy.new), doy := doy.new]
co2[year >= 2012 & !is.na(hour.new), hour := hour.new]

# Filter out modeled data and empty rows
# co2 <- co2[filled == 1,]
co2 <- co2[!is.na(Reco_g)]
co2[,.N, by = c('year')] # make sure there are about the equal numbers in all years except 2019, which should have fewer

# Date
co2[, date := parse_date_time(as_date(doy-1, origin = paste0(year, '-01-01')), orders = c('Y!-m!*-d!'))] # doy-1 because as_date is 0 indexed, while lubridate::yday() (used to create doy variable) is 1 indexed
co2[hour == round(hour), minute := 0]
co2[hour != round(hour), minute := 30]
co2[, ts := parse_date_time(paste(date, paste(floor(hour), minute, sep = ':')), orders = c('Y!-m!*-d! H!:M!'))]
co2[, day := mday(date)]
co2[, Flux_Date := paste(day, month, year, sep = '/')]

# treatment
co2[WW == 'C' & SW == 'C', treatment := 'Control']
co2[WW == 'C' & (SW == 'S' | SW == 'SW'), treatment := 'Air Warming']
co2[(WW == 'W' | WW == 'WW') & SW == 'C', treatment := 'Soil Warming']
co2[(WW == 'W' | WW == 'WW') & (SW == 'S' | SW == 'SW'), treatment := 'Air + Soil Warming']

# Plot ID
co2[, plot.id := paste(fence, plot, sep = '_')]
co2 <- co2[order(date, plot.id)]

# 2009-2011 chamber temps
filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/ITEX/warmxresp/2020_09_Dataset request/flux_data/co2/chamb_t_2009-2011/',
                        pattern = 'csv$',
                        full.names = TRUE)
chambt <- map_dfr(filenames,
                  ~ read.csv(.x))
chambt <- data.table(chambt)
chambt <- chambt[!is.na(Tchamb_fill), .(year, doy, hour, fence, plot, Tchamb_fill)]

# treatment
chambt[plot == 2 | plot == 4, treatment := 'Control']
chambt[plot == 1 | plot == 3, treatment := 'Air Warming']
chambt[plot == 6 | plot == 8, treatment := 'Soil Warming']
chambt[plot == 5 | plot == 7, treatment := 'Air + Soil Warming']

# Remove Plot information for early 2009 measurements (before plots had been established)
chambt <- chambt[year == 2009 & doy <= 152, plot := NA]

# summarize any duplicates
chambt <- chambt[, .(Tchamb_fill = mean(Tchamb_fill, na.rm = TRUE)), by = c('year', 'doy', 'hour', 'fence', 'plot')]

### Join flux and chamber temp data ###
# use doy and hour because the doy and hour variables were correct from the start for 2009-2011
co2 <- merge(co2, chambt, by = c('year', 'doy', 'hour', 'fence', 'plot'), all.x = TRUE)
co2[is.na(T_chamb) & !is.na(Tchamb_fill), T_chamb := Tchamb_fill]
# Remove ridiculous chamber temperatures
co2[T_chamb > 50, T_chamb := NA]

# Select columns needed for ITEX format
co2 <- co2[, .(ts, date, year, month, week, doy, hourmin = hour, fence, plot, plot.id, treatment, filled, nee = NEE_g, reco = Reco_g, gpp = GPP_g, r2, t.chamb = T_chamb)]
co2[,.N, by = year(date)] # make sure there are about the equal numbers in all years except 2019, which should have fewer

## add in hour variable to join with par later
co2[, hour := floor(hourmin)]

# order
co2 <- co2[order(date, plot.id, treatment, hourmin)]
###########################################################################################

### Weather Data ##########################################################################
filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/weather/',
                        pattern = 'csv$',
                        full.names = TRUE)
weather <- map_dfr(filenames,
                   ~ read.csv(.x,
                              header = TRUE) %>%
                     select(year = matches('[Y|y]ear'), DOY,
                            hourmin = matches('hour'), Tair, par = PAR,
                            precip = matches('[P|p]recip'), rh = RH))

weather <- data.table(weather)

# Format time variables
weather[, date := parse_date_time(as.Date(DOY-1, origin = paste(year, '-01-01', sep = '')), orders = c('Y!-m!*-d!'))] # doy-1 because as.Date is 0 indexed, while lubridate::yday() (used to create doy variable) is 1 indexed
weather[, year := year(date)]
weather[, month := month(date)]
weather[, day := mday(date)]
weather[, hour := floor(hourmin)]
weather <- weather[order(date, hour)]

# Neaten
weather <- weather[, .(date, hour, Tair, par, precip, rh)]
weather <- weather[,
                   .(par = mean(par, na.rm = TRUE),
                     Tair = mean(Tair, na.rm = TRUE),
                     precip = mean(precip, na.rm = TRUE),
                     rh = mean(rh, na.rm = TRUE)),
                   by = .(date, hour)]
###########################################################################################

### Soil Sensor Data ######################################################################
filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/soil_sensors/CiPEHR',
                        pattern = 'csv$|txt$',
                        full.names = TRUE)
soil.sensor <- map_dfr(filenames,
                       ~ read.table(.x,
                                    header = TRUE,
                                    sep = ',',
                                    fill = TRUE) %>%
                         select(ts = 1, year = matches('[Y|y]ear'), fence = matches('[F|f]ence'),
                                plot = matches('[P|p]lot'), hourmin = matches('hour'),
                                T_five, T_ten, T_twenty, T_forty, VWC, GWC))

soil.sensor <- data.table(soil.sensor)

# Format time variables
soil.sensor[, date := as_date(parse_date_time(ts, orders = c('Y!-m!*-d!', 'Y!-m!*-d! H!:M!:S!')))]
soil.sensor[, year := year(date)]
soil.sensor[, month := month(date)]
soil.sensor[, week := week(date)]
soil.sensor[, doy := yday(date)]
soil.sensor[, day := mday(date)]
soil.sensor[, date := parse_date_time(paste(year, month, day, sep = '-'), orders = c('Y!-m!*-d!'))]
soil.sensor[, hour := floor(hourmin)]
soil.sensor[, minute := hourmin%%1*60]
soil.sensor[, ts := parse_date_time(paste(date, paste(hour, minute)), orders = c('Y!-m!*-d! H!:M!'))]

# Treatment
soil.sensor[plot == 2 | plot == 4, treatment := 'Control']
soil.sensor[plot == 1 | plot == 3, treatment := 'Air Warming']
soil.sensor[plot == 6 | plot == 8, treatment := 'Soil Warming']
soil.sensor[plot == 5 | plot == 7, treatment := 'Air + Soil Warming']

# Format Plot IDs
soil.sensor[, plot.id := paste(fence, plot, sep = '_')]

# Neaten
soil.sensor <- soil.sensor[, .(ts, date, year, month, week, doy, hour, hourmin,
                               treatment, fence, plot, plot.id, t5 = T_five,
                               t10 = T_ten, t20 = T_twenty, t40 = T_forty,
                               vwc = VWC, gwc = GWC)]
###########################################################################################

### WTD Data ##############################################################################
### Load WTD
wtd <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/WTD/Compiled/WTD_2020_compiled.csv")

### Format WTD
# Remove rows without water table depth
wtd <- wtd[!is.na(WTD)]
wtd <- wtd[exp == 'CiPEHR', ]
wtd[, well := as.numeric(well)]

# Format time variables
wtd[, date := parse_date_time(date, orders = c('Y!/m!*/d!'))]
wtd[, year := year(date)]
wtd[, month := month(date)]
wtd[, day := mday(date)]
wtd[, WTD_Date := ymd(paste(year, month, day, sep = '/'))]

# Assign plots to wells
well.assignment <- expand.grid(year = seq(2009, 2020),
                               fence = seq(1, 6),
                               plot = seq(1, 8))
well.assignment <- data.table(well.assignment)
well.assignment <- well.assignment[order(year, fence, plot)]
# original designations
well.assignment[plot == 1 | plot == 4, well := 1]
well.assignment[plot == 2 | plot == 3, well := 2]
well.assignment[plot == 5 | plot == 8, well := 3]
well.assignment[plot == 6 | plot == 7, well := 4]
# 2013-2018 designations
well.assignment[year >= 2013 & fence == 1 & plot == 3, well := 2.5]
well.assignment[year >= 2013 & fence == 1 & plot == 5, well := 4.5]
well.assignment[year >= 2013 & fence == 2 & plot == 3, well := 2.5]
well.assignment[year >= 2013 & fence == 2 & plot == 5, well := 4.5]
well.assignment[year >= 2013 & fence == 3 & plot == 2, well := 2.5]
well.assignment[year >= 2013 & fence == 3 & plot == 7, well := 4.5]
well.assignment[year >= 2013 & fence == 4 & plot == 3, well := 2.5]
well.assignment[year >= 2013 & fence == 4 & plot == 8, well := 4.5]
well.assignment[year >= 2013 & fence == 5 & plot == 4, well := 2.5]
well.assignment[year >= 2013 & fence == 5 & plot == 6, well := 4.5]
well.assignment[year >= 2013 & fence == 6 & plot == 2, well := 2.5]
well.assignment[year >= 2013 & fence == 6 & plot == 7, well := 4.5]
# 2019 designations
well.assignment[year >= 2019 & fence == 1 & plot == 4, well := 4.17]
well.assignment[year >= 2019 & fence == 2 & plot == 2, well := 2.5]
well.assignment[year >= 2019 & fence == 2 & plot == 4, well := 2.5]
well.assignment[year >= 2019 & fence == 2 & plot == 5, well := 6.17]
well.assignment[year >= 2019 & fence == 2 & plot == 6, well := 6.17]
well.assignment[year >= 2019 & fence == 2 & plot == 8, well := 4.5]
well.assignment[year >= 2019 & fence == 3 & plot == 1, well := 2.5]
well.assignment[year >= 2019 & fence == 3 & plot == 3, well := 3.17]
well.assignment[year >= 2019 & fence == 3 & plot == 6, well := 6.17]
well.assignment[year >= 2019 & fence == 3 & plot == 8, well := 8.17]
well.assignment[year >= 2019 & fence == 4 & plot == 1, well := 1.17]
well.assignment[year >= 2019 & fence == 4 & plot == 5, well := 5.17]
well.assignment[year >= 2019 & fence == 4 & plot == 8, well := 8.17]
well.assignment[year >= 2019 & fence == 5 & plot == 3, well := 3.17]
well.assignment[year >= 2019 & fence == 5 & plot == 7, well := 4.5]
well.assignment[year >= 2019 & fence == 5 & plot == 8, well := 8.17]
well.assignment[year >= 2019 & fence == 6 & plot == 3, well := 3.17]
well.assignment[year >= 2019 & fence == 6 & plot == 4, well := 1.17]
well.assignment[year >= 2019 & fence == 6 & plot == 8, well := 8.17]

# join well assignments with wtd
wtd <- merge(wtd, well.assignment, by = c('year', 'fence', 'well'), allow.cartesian = TRUE)

# Treatment
wtd[plot == 2 | plot == 4, treatment := 'Control']
wtd[plot == 1 | plot == 3, treatment := 'Air Warming']
wtd[plot == 6 | plot == 8, treatment := 'Soil Warming']
wtd[plot == 5 | plot == 7, treatment := 'Air + Soil Warming']

# Duplicate 2009 data with NA in plot so that it can be joined with 2009 fluxes
# that don't have plot information
wtd.2009 <- wtd[year == 2009]
wtd.2009[, plot := NA]
wtd <- rbind(wtd.2009, wtd)

# Format Plot IDs
wtd[, plot.id := paste(fence, plot, sep = '_')]

# Neaten
wtd <- wtd[, .(date, fence,  WTD_Date, treatment, plot.id, wtd = WTD)]
###########################################################################################

### Load Thaw Depth Data ##################################################################
td <- fread("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/Thaw Depth/Processed/2020/Thaw_depth_2009-2020.csv")

# Remove rows without plot data or thaw depth data
td <- td[!is.na(plot)]
td <- td[!is.na(td)]
# Remove DryPEHR Data
td <- td[!is.na(as.numeric(plot))]

# Date
td[, date := parse_date_time(date, orders = c('Y!-m!*-d!'))]
td[, year := year(date)]
td[, month := month(date)]
td[, day := mday(date)]
td[, TD_Date := dmy(paste(day, month, year, sep = '/'))]

# Duplicate 2009 data with NA in plot so that it can be joined with 2009 fluxes
# that don't have plot information
td.2009 <- td[year == 2009]
td.2009[, plot := NA]
td <- rbind(td.2009, td)

# Treatment
td[ww == 'c' & sw == 'c', treatment := 'Control']
td[ww == 'c' & (sw == 's' | sw == 'sw'), treatment := 'Air Warming']
td[(ww == 'w' | ww == 'ww') & sw == 'c', treatment := 'Soil Warming']
td[(ww == 'w' | ww == 'ww') & (sw == 's' | sw == 'sw'), treatment := 'Air + Soil Warming']

# Plot ID
td[, plot.id := paste(fence, plot, sep = '_')]
td <- td[order(date, plot.id)]
td <- td[,.(date, fence, TD_Date, treatment, plot.id, td)]
###########################################################################################

### Load Subsidence Data ##################################################################
alt_sub <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/ALT_Subsidence_Corrected_2009_2020.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE)
alt_sub <- alt_sub[exp == 'CiPEHR']
alt_sub[, ':='(exp = NULL,
               plot = as.numeric(plot))]
alt_sub <- alt_sub[, .(year, block, fence, plot, treatment, subsidence, tp = TP, alt = ALT)]
###########################################################################################

### Load Vegetation Data ##################################################################
biomass <- read.csv("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/biomass/CiPEHR/EML_AK_CiPEHR_BiomassBySpecies_2009-2013__20150320_VGS.csv",
                    stringsAsFactors = FALSE) %>%
  rbind.data.frame(read.csv("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/biomass/CiPEHR/CiPEHR_biomass_2017.csv",
                            stringsAsFactors = FALSE)) %>%
  group_by(year, fence, plot) %>%
  summarise(biomass = sum(biomass, na.rm = TRUE)) # sum of all species
biomass <- as.data.table(biomass)

ndvi <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/ndvi/NDVI_CiPEHR&DryPEHR_2019_Datacheck.csv",
                 stringsAsFactors = FALSE)
ndvi <- ndvi[plot_type == 'flux' & !is.na(as.numeric(plot))]
ndvi[, ':=' (date = parse_date_time(date, orders = c('m!*/d!/Y!')),
             ndvi.date = mdy(date),
             plot = as.numeric(plot),
             ndvi = NDVI_relative)]
ndvi <- ndvi[, .(date, ndvi.date, year, fence, plot, ndvi)]
###########################################################################################

### Merge Datasets ########################################################################
### plot frame to join environmental data that doesn't have plot information with
plot.frame <- expand_grid(fence = seq(1, 6),
                          plot = seq(1, 8),
                          date = parse_date_time(seq(ymd('2008-09-01'), ymd('2020-09-30'), by = 'days'),
                                                 orders = c('Y!-m!*-d!')),
                          hourmin = as.numeric(seq(0, 23.5, by = 0.5))) %>%
  mutate(hour = floor(hourmin),
         mins = ifelse(hourmin == floor(hourmin),
                      0,
                      30),
         ts = parse_date_time(paste(date, paste(hour, mins, sep = ':')), orders = c('Y!-m!*-d! H!:M!')),
         year = year(ts),
         month = month(ts),
         week = week(ts),
         doy = yday(ts),
         treatment = case_when(
           plot == 2 | plot == 4 ~ 'Control',
           plot == 1 | plot == 3 ~ 'Air Warming',
           plot == 6 | plot == 8 ~ 'Soil Warming',
           plot == 5 | plot == 7 ~ 'Air + Soil Warming'
         ),
         plot.id = paste(fence, plot, sep = '_')) %>%
  select(-mins)
plot.frame <- as.data.table(plot.frame)

### PAR and Tair
weather <- weather[date >= ymd('2008-09-01'),]
weather <- merge(weather, plot.frame, by = c('date', 'hour'), allow.cartesian = TRUE)
flux <- merge(co2, weather,
              by = c('ts', 'year', 'month', 'week', 'doy', 'date', 'hour', 'hourmin', 'fence', 'plot', 'plot.id', 'treatment'),
              all = TRUE)
flux[is.nan(precip),
     precip := NA]
flux[is.nan(rh),
     rh := NA]
flux[is.nan(par),
     par := NA]

### Soil sensors
flux <- merge(flux, 
              soil.sensor,
              by = c('ts', 'year', 'month', 'week', 'doy', 'date', 'hour', 'hourmin', 'fence', 'plot', 'plot.id', 'treatment'),
              all = TRUE)

### Water Table Depth
# set the key to allow proper rolling join
setkey(wtd, fence, treatment, plot.id, date)
setkey(flux, fence, treatment, plot.id, date)

# Rolling join of water table depth and flux
# This joins by matching treatment and plot id and finding the closest date match between
# wtd and flux. In cases where there are two thaw depths equally distant in time
# from the flux measurement, it will return both, resulting in a longer data table than 
# the flux input. 
flux <- wtd[flux, roll = 'nearest']

# Remove duplicate wtd measurements for one flux measurement by selecting the earlier thaw
# depth
setkey(flux, date, fence, treatment, plot.id, hourmin, hour, ts, year, month, week, doy,
       plot, filled, nee, reco, gpp, r2, t.chamb, par, Tair, t5, t10, t20, t40, vwc, gwc)
flux <- flux[, first(.SD), by = .(date, fence, treatment, plot.id, hourmin, hour, ts, year, month, week, doy,
                                  plot, filled, nee, reco, gpp, r2, t.chamb, par, Tair, t5, t10, t20, t40, vwc, gwc)]

# If WTD date is more than 1 week from date, remove wtd
flux[abs(interval(date, WTD_Date)/ddays(1)) > 14,
     wtd := NA]
flux[is.na(wtd), .N]

### Thaw Depth
# set the key to allow proper rolling join
setkey(td, fence, treatment, plot.id, date)
setkey(flux, fence, treatment, plot.id, date)

# Rolling join of thaw depth and flux
# This joins by matching treatment and plot id and finding the closest date match between
# thaw depth and flux. In cases where there are two thaw depths equally distant in time
# from the flux measurement, it will return both, resulting in a longer data table than 
# the flux input. 
flux <- td[flux, roll = 'nearest']

# Remove duplicate thaw depths for one flux measurement by selecting the earlier thaw
# depth
setkey(flux, date, fence, treatment, plot.id, hourmin, hour, ts, year, month,
       week, doy, plot, filled, nee, reco, gpp, r2, t.chamb, par, Tair, t5,
       t10, t20, t40, vwc, gwc, WTD_Date, wtd)
flux <- flux[, first(.SD),
             by = .(date, fence, treatment, plot.id, hourmin, hour, ts, year, month,
                    week, doy, plot, filled, nee, reco, gpp, r2, t.chamb, par, Tair, t5,
                    t10, t20, t40, vwc, gwc, WTD_Date, wtd)]

# If TD date is more than 1 week from date, remove td
flux[abs(interval(date, TD_Date)/ddays(1)) > 14,
     td := NA]
flux[is.na(td), .N]

### Biomass
flux <- merge(flux,
              biomass,
              by = c('year', 'fence', 'plot'),
              all = TRUE)

### NDVI
# set the key to allow proper rolling join
setkey(ndvi, year, fence, plot, date)
setkey(flux, year, fence, plot, date)

# Rolling join of thaw depth and flux
# This joins by matching year, fence, and plot and finding the closest date match between
# NDVI and flux. In cases where there are two NDVIs equally distant in time
# from the flux measurement, it will return both, resulting in a longer data table than 
# the flux input. 
flux <- ndvi[flux, roll = 'nearest']

# Remove duplicate NDVI values for one flux measurement by selecting the earlier thaw
# depth
setkey(flux, date, fence, treatment, plot.id, hourmin, hour, ts, year, month,
       week, doy, plot, filled, nee, reco, gpp, r2, t.chamb, par, Tair, t5,
       t10, t20, t40, vwc, gwc, WTD_Date, wtd, td, biomass)
flux <- flux[, first(.SD),
             by = .(date, fence, treatment, plot.id, hourmin, hour, ts, year, month,
                    week, doy, plot, filled, nee, reco, gpp, r2, t.chamb, par, Tair, t5,
                    t10, t20, t40, vwc, gwc, WTD_Date, wtd, td, biomass)]

# If NDVI date is more than 1 week from date, remove ndvi
flux[abs(interval(date, ndvi.date)/ddays(1)) > 14,
     ndvi := NA]
flux[is.na(ndvi), .N]

### Subsidence, ALT, TP
flux <- merge(flux,
              alt_sub,
              by = c('year', 'fence', 'plot', 'treatment'),
              all = TRUE)

flux <- flux[order(ts, plot.id)]
flux[, tp.to.date := td - subsidence]

# # Plot to make sure things look right
# # Chamber temps vs. air temps
# ggplot(flux, aes(x = Tair, y = t.chamb, colour = year), alpha = 0.2) +
#   geom_point() +
#   facet_grid(.~treatment)
# 
# # reco
# ggplot(flux, aes(x = doy, y = reco, colour = year), alpha = 0.2) +
#   geom_point()
# # gpp
# ggplot(flux, aes(x = doy, y = gpp, colour = year), alpha = 0.2) +
#   geom_point()
# # nee
# ggplot(flux, aes(x = doy, y = nee, colour = year), alpha = 0.2) +
#   geom_point()
# 
# # soil temp and nee
# ggplot(flux, aes(x = t5, y = nee, colour = year), alpha = 0.2) +
#   geom_point()
# ggplot(flux, aes(x = t10, y = nee, colour = year), alpha = 0.2) +
#   geom_point()
# ggplot(flux, aes(x = t20, y = nee, colour = year), alpha = 0.2) +
#   geom_point()
# ggplot(flux, aes(x = t40, y = nee, colour = year), alpha = 0.2) +
#   geom_point()
# 
# # air temp and nee
# ggplot(flux, aes(x = t.chamb, y = nee, colour = year), alpha = 0.2) +
#   geom_point()
# 
# # soil moisture and nee
# ggplot(flux, aes(x = vwc, y = nee, colour = year), alpha = 0.2) +
#   geom_point() +
#   facet_grid(.~treatment)
# ggplot(flux, aes(x = gwc, y = nee, colour = year), alpha = 0.2) +
#   geom_point() +
#   facet_grid(.~treatment)
# 
# # td and nee
# ggplot(flux, aes(x = td, y = nee, colour = year), alpha = 0.2) +
#   geom_point()
# 
# # wtd and nee
# ggplot(flux, aes(x = wtd, y = nee, colour = year), alpha = 0.2) +
#   geom_point()
# 
# # one erroneously high value
# flux <- flux[co2 <= 0.5 | is.na(co2)]
###########################################################################################

### Gap Fill ##############################################################################
### Clean up
rm(alt_sub, biomass, chambt, co2, ndvi, plot.frame, soil.sensor,
   td, td.2009, weather, well.assignment, wtd, wtd.2009)
### Chamber Temps
# determine period when chambers are deployed
deployed <- flux[!is.na(nee),
                 .(first = first(date), last = last(date)),
                 by = 'year']
flux <- merge(flux, deployed, by = 'year', all = TRUE)
flux[, deployed := ifelse(date >= first & date <= last,
                          1,
                          0)]
flux[, ':=' (first = NULL, last = NULL)]
# Remove really low chamber temps when air temp is much higher
flux[(Tair-t.chamb) > 10, t.chamb := NA]
ggplot(flux, aes(x = Tair, y = t.chamb, colour = year), alpha = 0.2) +
  geom_point() +
  facet_grid(.~treatment)

tchamb.m <- lm(t.chamb ~ Tair + treatment, data = flux)
summary(tchamb.m)

flux[treatment == 'Control',
     t.chamb.m := tchamb.m$coefficients[1] + Tair*tchamb.m$coefficients[2]]
flux[treatment == 'Air Warming',
     t.chamb.m := tchamb.m$coefficients[1] + Tair*(tchamb.m$coefficients[2] + tchamb.m$coefficients[3])]
flux[treatment == 'Air + Soil Warming',
     t.chamb.m := tchamb.m$coefficients[1] + Tair*(tchamb.m$coefficients[2] + tchamb.m$coefficients[4])]
flux[treatment == 'Soil Warming',
     t.chamb.m := tchamb.m$coefficients[1] + Tair*(tchamb.m$coefficients[2] + tchamb.m$coefficients[5])]

# ggplot(flux, aes(x = Tair, y = t.chamb.m, colour = year), alpha = 0.2) +
#   geom_point() +
#   facet_grid(.~treatment)

flux[is.na(t.chamb) & deployed == 1, t.chamb := t.chamb.m]
flux[, tair := ifelse(!is.na(t.chamb),
                          t.chamb,
                          Tair)]
flux[is.na(tair), .N, by = 'year']
flux[, ':=' (t.chamb = NULL,
             Tair = NULL)]
# ggplot(flux, aes(x = tair, y = t.chamb, colour = year), alpha = 0.2) +
#   geom_point() +
#   facet_grid(.~treatment)
rm(deployed, tchamb.m)

### Probably won't gap fill anything else
flux[is.na(par), .N, by = 'year']
flux[is.na(rh), .N, by = 'year'] # missing a lot
flux[is.na(t5), .N, by = 'year']
flux[is.na(t10), .N, by = 'year']
flux[is.na(t20) & plot %in% c(1,2,5,6), .N, by = 'year']
flux[is.na(t40) & plot %in% c(1,2,5,6), .N, by = 'year']
flux[is.na(vwc) & plot %in% c(1,2,5,6), .N, by = 'year'] # missing a lot
flux[is.na(gwc), .N, by = 'year'] # missing a lot
flux[is.na(td), .N, by = 'year'] # missing a lot - but not during growing season
flux[is.na(wtd), .N, by = 'year'] # missing a lot - but not during growing season
flux[is.na(biomass), .N, by = 'year']
flux[is.na(ndvi), .N, by = 'year']
flux[is.na(subsidence), .N, by = 'year']
flux[is.na(alt), .N, by = 'year']
flux[is.na(tp), .N, by = 'year']
flux[is.na(tp.to.date), .N, by = 'year'] # missing a lot - but not during growing season
###########################################################################################

### Create Summaries and Derived Variables ################################################
flux.final <- flux[, .(ts, date, year, month, week, doy, hour, hourmin, block,
                       fence, plot, plot.id, treatment, deployed, filled, nee,
                       reco, gpp, r2, par, tair, t5, t10, t20, t40, vwc, gwc,
                       wtd, td, biomass, ndvi, precip, rh, subsidence, alt, tp,
                       tp.to.date)]

flux.daily <- flux.final[,
                         .(nee.sum = sum(nee, na.rm = TRUE),
                           reco.sum = sum(reco, na.rm = TRUE),
                           gpp.sum = sum(gpp, na.rm = TRUE),
                           tair.max = max(tair, na.rm = TRUE),
                           tair.mean = mean(tair, na.rm = TRUE),
                           tair.min = min(tair, na.rm = TRUE),
                           tair.sd = sd(tair, na.rm = TRUE),
                           growing.days = mean(tair, na.rm = TRUE) - 5,
                           freezing.days = mean(tair, na.rm = TRUE)*-1,
                           t5.max = max(t5, na.rm = TRUE),
                           t5.mean = mean(t5, na.rm = TRUE),
                           t5.min = min(t5, na.rm = TRUE),
                           t5.sd = sd(t5, na.rm = TRUE),
                           t10.max = max(t10, na.rm = TRUE),
                           t10.mean = mean(t10, na.rm = TRUE),
                           t10.min = min(t10, na.rm = TRUE),
                           t10.sd = sd(t10, na.rm = TRUE),
                           t20.max = max(t20, na.rm = TRUE),
                           t20.mean = mean(t20, na.rm = TRUE),
                           t20.min = min(t20, na.rm = TRUE),
                           t20.sd = sd(t20, na.rm = TRUE),
                           t40.max = max(t40, na.rm = TRUE),
                           t40.mean = mean(t40, na.rm = TRUE),
                           t40.min = min(t40, na.rm = TRUE),
                           t40.sd = sd(t40, na.rm = TRUE),
                           vwc.max = max(vwc, na.rm = TRUE),
                           vwc.mean = mean(vwc, na.rm = TRUE),
                           vwc.min = min(vwc, na.rm = TRUE),
                           vwc.sd = sd(vwc, na.rm = TRUE),
                           gwc.max = max(gwc, na.rm = TRUE),
                           gwc.mean = mean(gwc, na.rm = TRUE),
                           gwc.min = min(gwc, na.rm = TRUE),
                           gwc.sd = sd(gwc, na.rm = TRUE),
                           wtd = mean(wtd, na.rm = TRUE),
                           td = mean(td, na.rm = TRUE),
                           biomass = mean(biomass, na.rm = TRUE),
                           ndvi = mean(ndvi, na.rm = TRUE),
                           precip = sum(precip, na.rm = TRUE),
                           rh = mean(rh, na.rm = TRUE),
                           subsidence = mean(subsidence, na.rm = TRUE),
                           alt = mean(alt, na.rm = TRUE),
                           tp = mean(tp, na.rm = TRUE)),
                         by = c('date', 'year', 'month', 'week', 'doy', 'fence',
                                'plot', 'plot.id', 'treatment')]
flux.daily <- flux.daily[, lapply(.SD, function(x) replace(x, list = is.infinite(x), values = NA))]
flux.daily <- flux.daily[, lapply(.SD, function(x) replace(x, list = is.nan(x), values = NA))]

flux.daily[growing.days < 0,
           growing.days := 0]
flux.daily[freezing.days < 0,
           freezing.days := 0]
flux.daily[is.na(precip),
           precip := 0]
flux.daily[, flux.year := ifelse(month >= 10,
                                 year +1,
                                 year)]
flux.daily[, ':=' (gdd = cumsum(growing.days),
                   fdd = cumsum(freezing.days),
                   precip.cum = cumsum(precip)),
           by = c('flux.year', 'fence', 'plot')]

flux.daily[,
           ':=' (gdd.2d = frollsum(growing.days, n = 2, align = 'right', na.rm = TRUE), # day prior + current day
                 gdd.3d = frollsum(growing.days, n = 3, align = 'right', na.rm = TRUE), # 2 days prior + current day
                 gdd.4d = frollsum(growing.days, n = 4, align = 'right', na.rm = TRUE), # 3 days prior + current day
                 gdd.5d = frollsum(growing.days, n = 5, align = 'right', na.rm = TRUE), # day prior + current day
                 gdd.6d = frollsum(growing.days, n = 6, align = 'right', na.rm = TRUE), # day prior + current day
                 gdd.1w = frollsum(growing.days, n = 7, align = 'right', na.rm = TRUE), # full week prior including current day
                 gdd.2w = frollsum(growing.days, n = 14, align = 'right', na.rm = TRUE), # 2 weeks prior including current day
                 fdd.2d = frollsum(freezing.days, n = 2, align = 'right', na.rm = TRUE), # day prior + current day
                 fdd.3d = frollsum(freezing.days, n = 3, align = 'right', na.rm = TRUE), # 2 days prior + current day
                 fdd.4d = frollsum(freezing.days, n = 4, align = 'right', na.rm = TRUE), # 3 days prior + current day
                 fdd.5d = frollsum(freezing.days, n = 5, align = 'right', na.rm = TRUE), # day prior + current day
                 fdd.6d = frollsum(freezing.days, n = 6, align = 'right', na.rm = TRUE), # day prior + current day
                 fdd.1w = frollsum(freezing.days, n = 7, align = 'right', na.rm = TRUE), # full week prior including current day
                 fdd.2w = frollsum(freezing.days, n = 14, align = 'right', na.rm = TRUE), # 2 weeks prior including current day
                 precip.2d = frollsum(precip, n = 2, align = 'right', na.rm = TRUE), # day prior + current day
                 precip.3d = frollsum(precip, n = 3, align = 'right', na.rm = TRUE), # 2 days prior + current day
                 precip.4d = frollsum(precip, n = 4, align = 'right', na.rm = TRUE), # 3 days prior + current day
                 precip.5d = frollsum(precip, n = 5, align = 'right', na.rm = TRUE), # day prior + current day
                 precip.6d = frollsum(precip, n = 6, align = 'right', na.rm = TRUE), # day prior + current day
                 precip.1w = frollsum(precip, n = 7, align = 'right', na.rm = TRUE), # full week prior including current day
                 precip.2w = frollsum(precip, n = 14, align = 'right', na.rm = TRUE), # 2 weeks prior including current day
                 vwc.mean.2d = frollmean(vwc.mean, n = 2, align = 'right', na.rm = TRUE), # day prior + current day
                 vwc.mean.3d = frollmean(vwc.mean, n = 3, align = 'right', na.rm = TRUE), # 2 days prior + current day
                 vwc.mean.4d = frollmean(vwc.mean, n = 4, align = 'right', na.rm = TRUE), # 3 days prior + current day
                 vwc.mean.5d = frollmean(vwc.mean, n = 5, align = 'right', na.rm = TRUE), # day prior + current day
                 vwc.mean.6d = frollmean(vwc.mean, n = 6, align = 'right', na.rm = TRUE), # day prior + current day
                 vwc.mean.1w = frollmean(vwc.mean, n = 7, align = 'right', na.rm = TRUE), # full week prior including current day
                 vwc.mean.2w = frollmean(vwc.mean, n = 14, align = 'right', na.rm = TRUE), # 2 weeks prior including current day
                 vwc.max.2d = frollapply(vwc.max, n = 2, align = 'right', FUN = max, na.rm = TRUE), # day prior + current day
                 vwc.max.3d = frollapply(vwc.max, n = 3, align = 'right', FUN = max, na.rm = TRUE), # 2 days prior + current day
                 vwc.max.4d = frollapply(vwc.max, n = 4, align = 'right', FUN = max, na.rm = TRUE), # 3 days prior + current day
                 vwc.max.5d = frollapply(vwc.max, n = 5, align = 'right', FUN = max, na.rm = TRUE), # day prior + current day
                 vwc.max.6d = frollapply(vwc.max, n = 6, align = 'right', FUN = max, na.rm = TRUE), # day prior + current day
                 vwc.max.1w = frollapply(vwc.max, n = 7, align = 'right', FUN = max, na.rm = TRUE), # full week prior including current day
                 vwc.max.2w = frollapply(vwc.max, n = 14, align = 'right', FUN = max, na.rm = TRUE), # 2 weeks prior including current day
                 vwc.min.2d = frollapply(vwc.min, n = 2, align = 'right', FUN = min, na.rm = TRUE), # day prior + current day
                 vwc.min.3d = frollapply(vwc.min, n = 3, align = 'right', FUN = min, na.rm = TRUE), # 2 days prior + current day
                 vwc.min.4d = frollapply(vwc.min, n = 4, align = 'right', FUN = min, na.rm = TRUE), # 3 days prior + current day
                 vwc.min.5d = frollapply(vwc.min, n = 5, align = 'right', FUN = min, na.rm = TRUE), # day prior + current day
                 vwc.min.6d = frollapply(vwc.min, n = 6, align = 'right', FUN = min, na.rm = TRUE), # day prior + current day
                 vwc.min.1w = frollapply(vwc.min, n = 7, align = 'right', FUN = min, na.rm = TRUE), # full week prior including current day
                 vwc.min.2w = frollapply(vwc.min, n = 14, align = 'right', FUN = min, na.rm = TRUE), # 2 weeks prior including current day
                 gwc.mean.2d = frollmean(gwc.mean, n = 2, align = 'right', na.rm = TRUE), # day prior + current day
                 gwc.mean.3d = frollmean(gwc.mean, n = 3, align = 'right', na.rm = TRUE), # 2 days prior + current day
                 gwc.mean.4d = frollmean(gwc.mean, n = 4, align = 'right', na.rm = TRUE), # 3 days prior + current day
                 gwc.mean.5d = frollmean(gwc.mean, n = 5, align = 'right', na.rm = TRUE), # day prior + current day
                 gwc.mean.6d = frollmean(gwc.mean, n = 6, align = 'right', na.rm = TRUE), # day prior + current day
                 gwc.mean.1w = frollmean(gwc.mean, n = 7, align = 'right', na.rm = TRUE), # full week prior including current day
                 gwc.mean.2w = frollmean(gwc.mean, n = 14, align = 'right', na.rm = TRUE), # 2 weeks prior including current day
                 gwc.max.2d = frollapply(gwc.max, n = 2, align = 'right', FUN = max, na.rm = TRUE), # day prior + current day
                 gwc.max.3d = frollapply(gwc.max, n = 3, align = 'right', FUN = max, na.rm = TRUE), # 2 days prior + current day
                 gwc.max.4d = frollapply(gwc.max, n = 4, align = 'right', FUN = max, na.rm = TRUE), # 3 days prior + current day
                 gwc.max.5d = frollapply(gwc.max, n = 5, align = 'right', FUN = max, na.rm = TRUE), # day prior + current day
                 gwc.max.6d = frollapply(gwc.max, n = 6, align = 'right', FUN = max, na.rm = TRUE), # day prior + current day
                 gwc.max.1w = frollapply(gwc.max, n = 7, align = 'right', FUN = max, na.rm = TRUE), # full week prior including current day
                 gwc.max.2w = frollapply(gwc.max, n = 14, align = 'right', FUN = max, na.rm = TRUE), # 2 weeks prior including current day
                 gwc.min.2d = frollapply(gwc.min, n = 2, align = 'right', FUN = min, na.rm = TRUE), # day prior + current day
                 gwc.min.3d = frollapply(gwc.min, n = 3, align = 'right', FUN = min, na.rm = TRUE), # 2 days prior + current day
                 gwc.min.4d = frollapply(gwc.min, n = 4, align = 'right', FUN = min, na.rm = TRUE), # 3 days prior + current day
                 gwc.min.5d = frollapply(gwc.min, n = 5, align = 'right', FUN = min, na.rm = TRUE), # day prior + current day
                 gwc.min.6d = frollapply(gwc.min, n = 6, align = 'right', FUN = min, na.rm = TRUE), # day prior + current day
                 gwc.min.1w = frollapply(gwc.min, n = 7, align = 'right', FUN = min, na.rm = TRUE), # full week prior including current day
                 gwc.min.2w = frollapply(gwc.min, n = 14, align = 'right', FUN = min, na.rm = TRUE)), # 2 weeks prior including current day
           by = c('fence', 'plot')]
# remove NaN, -Inf, and Inf introduced by calculations
flux.daily <- flux.daily[, lapply(.SD, function(x) replace(x, list = is.infinite(x), values = NA))]
flux.daily <- flux.daily[, lapply(.SD, function(x) replace(x, list = is.nan(x), values = NA))]

# check that everything worked as expected
View(flux.daily[order(plot.id), .SD, .SDcols = c('date', 'fence', 'plot', 'tair.mean', grep('gdd', colnames(flux.daily), value = TRUE))])
View(flux.daily[order(plot.id), .SD, .SDcols = c('date', 'fence', 'plot', 'tair.mean', grep('fdd', colnames(flux.daily), value = TRUE))])
View(flux.daily[order(plot.id), .SD, .SDcols = c('date', 'fence', 'plot', grep('precip', colnames(flux.daily), value = TRUE))])
View(flux.daily[order(plot.id), .SD, .SDcols = c('date', 'fence', 'plot', grep('vwc.mean', colnames(flux.daily), value = TRUE))])
View(flux.daily[order(plot.id), .SD, .SDcols = c('date', 'fence', 'plot', grep('vwc.max', colnames(flux.daily), value = TRUE))])
View(flux.daily[order(plot.id), .SD, .SDcols = c('date', 'fence', 'plot', grep('vwc.min', colnames(flux.daily), value = TRUE))])
View(flux.daily[order(plot.id), .SD, .SDcols = c('date', 'fence', 'plot', grep('gwc.mean', colnames(flux.daily), value = TRUE))])
View(flux.daily[order(plot.id), .SD, .SDcols = c('date', 'fence', 'plot', grep('gwc.max', colnames(flux.daily), value = TRUE))])
View(flux.daily[order(plot.id), .SD, .SDcols = c('date', 'fence', 'plot', grep('gwc.min', colnames(flux.daily), value = TRUE))])

flux.weekly <- flux.daily[,
                          .(),
                          by = c('date', 'flux.year', 'month', 'week', 'fence',
                                 'plot', 'plot.id', 'treatment')]

# write.csv(flux.final, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/ITEX_WarmXResp/2020_09_Dataset request/flux_data/itex_flux_winter.csv',
#           row.names = FALSE)





### Load Data ###############################################################################################################
flux_cumulative_cip <- read.csv("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/co2 fluxes/Autochamber/Multiyear_Summaries/2009_2020/Flux_cumulative_plot_2009_2020.csv") %>%
  mutate(treatment = ifelse(plot == 2 | plot == 4,
                            'Control',
                            ifelse(plot == 1 | plot == 3,
                                   'Air Warming',
                                   ifelse(plot == 6 | plot == 8,
                                          'Soil Warming',
                                          'Air + Soil Warming'))))
# flux_cumulative_dry <- read.csv("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/co2 fluxes/Autochamber/Multiyear_Summaries/2012_2019_Drypehr/Flux_cumulative_DryPEHR_2009_2019.csv")
# flux_cumulative <- rbind.data.frame(flux_cumulative_cip, flux_cumulative_dry)
alt_sub <- read.csv("/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/ALT_Subsidence_Corrected_2009_2020.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE) %>%
  filter(exp == 'CiPEHR') %>%
  select(-exp) %>%
  mutate(plot = as.numeric(plot),
         block = toupper(block))
wtd <- read.csv("/home/heidi/Documents/School/NAU/Schuur Lab/WTD/Compiled/WTD_2020_compiled.csv",
                stringsAsFactors = FALSE)

# automatically import and merge all of the moisture data from the soil sensors
# '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/soil_sensors/CiPEHR'
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
moisture <- import_soil_moisture('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/soil_sensors/CiPEHR')

biomass <- read.csv("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/biomass/CiPEHR/EML_AK_CiPEHR_BiomassBySpecies_2009-2013__20150320_VGS.csv",
                    stringsAsFactors = FALSE) %>%
  rbind.data.frame(read.csv("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/biomass/CiPEHR/CiPEHR_biomass_2017.csv",
                            stringsAsFactors = FALSE)) %>%
  group_by(year, fence, plot) %>%
  summarise(biomass = sum(biomass, na.rm = TRUE))

ndvi <- read.csv("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/ndvi/NDVI_CiPEHR&DryPEHR_2019_Datacheck.csv",
                 stringsAsFactors = FALSE) %>%
  filter(year != 2009 & year != 2012 & year != 2013) %>%
  group_by(year, block, fence, plot) %>%
  summarise(peak_NDVI = max(NDVI_relative, na.rm = TRUE)) %>%
  filter(!is.na(as.numeric(plot))) %>%
  mutate(plot = as.numeric(plot)) %>%
  arrange(year, fence, plot)

filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/chamber_temps/',
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
                                     max.t.chamb = max(Tchamb_fill, na.rm = TRUE),
                                     growing.days = ifelse(mean.t.chamb >= 5,
                                                           mean.t.chamb - 5,
                                                           NA),
                                     freezing.days = ifelse(mean.t.chamb <= 0,
                                                            mean.t.chamb,
                                                            NA)) %>%
                           ungroup()) %>%
  mutate(date = as.Date(doy, origin = paste(as.character(year-1), '-12-31', sep = ''))) %>%
  arrange(year, doy, fence, plot) %>%
  filter(!(is.nan(mean.t.chamb) & min.t.chamb == Inf & max.t.chamb == -Inf))  %>%
  filter(plot <= 8)
#############################################################################################################################

### Calculate Growing Season Air Temperature Indices #########################################################################
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
            se.t.chamb.min = sd(min.t.chamb)/sqrt(n()),
            gdd = sum(growing.days),
            fdd = sum(freezing.days*-1))

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
env_var <- flux_cumulative_cip %>%
  full_join(alt_sub %>%
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
              select(year, block, fence, plot, well, treatment, subsidence, ALT, thaw.penetration = ALT.corrected),
            by = c('year', 'fence', 'plot', 'treatment')) %>%
  select(year, block, fence, plot, well, treatment, NEE.sum, Reco.sum, GPP.sum, subsidence, ALT, thaw.penetration) %>%
  full_join(t_chamb_annual, by = c('year', 'block', 'fence', 'plot', 'treatment')) %>%
  full_join(moisture_annual, by = c('year', 'fence', 'plot', 'treatment')) %>%
  full_join(soil_temp_5_annual, by = c('year', 'fence', 'plot', 'treatment')) %>%
  full_join(soil_temp_10_annual, by = c('year', 'fence', 'plot', 'treatment')) %>%
  full_join(soil_temp_20_annual, by = c('year', 'fence', 'plot', 'treatment')) %>%
  full_join(soil_temp_40_annual, by = c('year', 'fence', 'plot', 'treatment')) %>%
  full_join(select(bio_ndvi_filled, year, block, fence, plot, treatment, biomass.filled),
            by = c('year', 'block', 'fence', 'plot', 'treatment'))

# write.csv(env_var, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/annual_environmental_data_compiled.csv',
#           row.names = FALSE)
#############################################################################################################################