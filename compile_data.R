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
# substitute deep soil temps from nearby plots for ones without probes?

### co2 Data ##############################################################################
### Load Fluxes
co2 <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/fluxes/Flux_data_halfhourly_modelled_2009_2020.csv")
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
co2[, week := week(ts)]
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
filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/weather/',
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
weather[, flux.year := fifelse(month >= 10,
                               year + 1,
                               year)]
weather[, season := fifelse(month <= 4 | month >= 10,
                            'ngs',
                            'gs')]
weather[, day := mday(date)]
weather[, hour := floor(hourmin)]
weather <- weather[order(date, hour)]

# Neaten
# weather data to use for environmental summary
weather.env <- weather[, .(flux.year, season, date, hour, Tair, par, precip, rh)]
weather.env <- weather.env[flux.year >= 2009][, .(tair.mean = mean(Tair, na.rm = TRUE),
                                                  precip = sum(precip, na.rm = TRUE),
                                                  par = mean(par, na.rm = TRUE)),
                                              by = c('flux.year', 'season')]
# weather data to use for flux data
weather.f <- weather[, .(date, hour, Tair, par, precip, rh)]
weather.f <- weather.f[,
                   .(par = mean(par, na.rm = TRUE),
                     Tair = mean(Tair, na.rm = TRUE),
                     precip = mean(precip, na.rm = TRUE),
                     rh = mean(rh, na.rm = TRUE)),
                   by = .(date, hour)]
###########################################################################################

### Soil Sensor Data ######################################################################
filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/soil_sensors/CiPEHR',
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

# Gap fill missing vwc and soil temps
soil.sensor[, ':=' (T_twenty = fifelse(is.na(T_twenty),
                                          mean(T_twenty, na.rm = TRUE),
                                          T_twenty),
                    T_forty  = fifelse(is.na(T_forty),
                                       mean(T_forty, na.rm = TRUE),
                                       T_forty),
                    VWC  = fifelse(is.na(VWC),
                                       mean(VWC, na.rm = TRUE),
                                       VWC)),
                    by = .(year, doy, hourmin, fence, treatment)]
# remove NaN, -Inf, and Inf introduced by calculations
soil.sensor <- soil.sensor[, lapply(.SD, function(x) replace(x, list = is.infinite(x), values = NA))]
soil.sensor <- soil.sensor[, lapply(.SD, function(x) replace(x, list = is.nan(x), values = NA))]

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
wtd.f <- merge(wtd, well.assignment, by = c('year', 'fence', 'well'), allow.cartesian = TRUE)

# Treatment
wtd.f[plot == 2 | plot == 4, treatment := 'Control']
wtd.f[plot == 1 | plot == 3, treatment := 'Air Warming']
wtd.f[plot == 6 | plot == 8, treatment := 'Soil Warming']
wtd.f[plot == 5 | plot == 7, treatment := 'Air + Soil Warming']

# Duplicate 2009 data with NA in plot so that it can be joined with 2009 fluxes
# that don't have plot information
wtd.2009 <- wtd.f[year == 2009]
wtd.2009[, plot := NA]
wtd.f <- rbind(wtd.2009, wtd.f)

# Format Plot IDs
wtd.f[, plot.id := paste(fence, plot, sep = '_')]

# Neaten
# for flux data
wtd.f <- wtd.f[, .(date, fence,  WTD_Date, treatment, plot.id, wtd = WTD)]

# for environmental data
wtd.env <- wtd[, ':=' (treatment = fifelse(well %in% c(1, 2, 2.5, 1.17, 2.17, 3.17, 4.17),
                                      'Control',
                                      'Soil Warming'),
                       flux.year = year)]
wtd.env <- wtd.env[, .(wtd.mean = round(mean(WTD, na.rm = TRUE), 2)),
                   by = .(flux.year, treatment)]
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
td[, flux.year := fifelse(month >= 10,
                          year + 1,
                          year)]
td[, day := mday(date)]
td[, TD_Date := dmy(paste(day, month, year, sep = '/'))]

# Treatment
td[ww == 'c' & sw == 'c', treatment := 'Control']
td[ww == 'c' & (sw == 's' | sw == 'sw'), treatment := 'Air Warming']
td[(ww == 'w' | ww == 'ww') & sw == 'c', treatment := 'Soil Warming']
td[(ww == 'w' | ww == 'ww') & (sw == 's' | sw == 'sw'), treatment := 'Air + Soil Warming']

# Plot ID
td[, plot.id := paste(fence, plot, sep = '_')]
td <- td[order(date, plot.id)]
td.f <- td[,.(date, year, fence, TD_Date, treatment, plot.id, td)]

# ALT
alt.f <- td[, .(alt = max(td, na.rm = TRUE),
              alt.date = TD_Date[which(td == max(td, na.rm = TRUE))]),
          by = .(fence, plot.id, treatment, year)]
alt.f <- alt.f[, .(alt = first(alt),
               alt.date = first(alt.date)),
           by = .(fence, plot.id, treatment, year)]
alt.env <- alt.f[, flux.year := year]
alt.env[, alt.doy := yday(alt.date)]
alt.env <- alt.env[, .(alt = round(mean(alt, na.rm = TRUE), 2),
            alt.doy = round(mean(alt.doy, na.rm = TRUE))),
        by = .(flux.year, treatment)]
###########################################################################################

### Load Subsidence Data ##################################################################
sub <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/ALT_Subsidence_Corrected_2009_2020.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE)
sub <- sub[exp == 'CiPEHR']
sub[, ':='(exp = NULL,
           plot = as.numeric(plot),
           subsidence = subsidence * 100)]
sub <- sub[, .(year, fence, plot, treatment, subsidence)]
###########################################################################################

### Load Vegetation Data ##################################################################
biomass <- read.csv("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/biomass/CiPEHR/EML_AK_CiPEHR_BiomassBySpecies_2009-2013__20150320_VGS.csv",
                    stringsAsFactors = FALSE) %>%
  rbind.data.frame(read.csv("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/biomass/CiPEHR/CiPEHR_biomass_2017.csv",
                            stringsAsFactors = FALSE)) %>%
  group_by(year, fence, plot) %>%
  summarise(biomass = sum(biomass, na.rm = TRUE)) # sum of all species
biomass <- as.data.table(biomass)

ndvi <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/ndvi/NDVI_CiPEHR&DryPEHR_2019_Datacheck.csv",
                 stringsAsFactors = FALSE)
ndvi <- ndvi[plot_type == 'flux' & !is.na(as.numeric(plot))]
ndvi[, ':=' (date = parse_date_time(date, orders = c('m!*/d!/Y!')),
             ndvi.date = mdy(date),
             plot = as.numeric(plot),
             ndvi = NDVI_relative)]
ndvi <- ndvi[, .(date, ndvi.date, year, fence, plot, ndvi)]
###########################################################################################

### Load Snow Data ########################################################################
snow <- fread('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/snow_depth/plot_snow_depth_2009_2020.csv')

snow <- snow[exp == 'CiPEHR', ]
snow[, ':=' (exp = NULL,
                     date = parse_date_time(date, orders = c('m!/d!/Y!')),
                     plot = as.integer(plot))]
snow[, month := month(date)]
snow[, flux.year := fifelse(month >= 10,
                            year + 1,
                            year)]

# Treatment
snow[plot == 2 | plot == 4, treatment := 'Control']
snow[plot == 1 | plot == 3, treatment := 'Air Warming']
snow[plot == 6 | plot == 8, treatment := 'Soil Warming']
snow[plot == 5 | plot == 7, treatment := 'Air + Soil Warming']

snow.env <- snow[, .(snow.depth = round(mean(snow.depth, na.rm = TRUE), 2)),
                                             by = c('flux.year', 'treatment')]

snow.f <- snow[, date := NULL]
snow.f[, flux.year := NULL]
snow.f[, month := NULL]

# Load Snow Free Date
snow.free.2009.2016 <- read.csv('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/CO2 fluxes/Snow free/LTER_Data/2016/CiPEHR_dates_snowfree_2010_2016.csv',
                                na.strings = c('N/A', 'ND')) %>%
  filter(Plot.Type == 'F') %>%
  select(flux.year = Year, fence = Fence, plot = Plot, treatment = Treatment,
         doy.snow.free = DOY.Snow.Free) %>%
  mutate(treatment = case_when(plot == 2 | plot == 4 ~ 'Control',
                               plot == 1 | plot == 3 ~ 'Air Warming',
                               plot == 6 | plot == 8 ~ 'Soil Warming',
                               plot == 5 | plot == 7 ~ 'Air + Soil Warming'))

snow.free.2017 <- read_excel('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/Computer_Backups/Healy cabin computer backup/2017/CiPEHR_DryPEHR/SnowFree/Date Plots Snow Free_2017.xlsx',
                             sheet = 1) %>%
  slice(-1) %>%
  select(plot = `Plot Number`, flux = Flux) %>%
  mutate(flux = as_date(as.numeric(flux), origin = '1899-12-30')) %>% # excel uses 1900-01-01, but I think there is a difference in indexing that is causing the 2 day offset?
  separate(plot, into = c('fence', 'plot'), sep = '_',  convert = TRUE) %>%
  mutate(flux.year = 2017,
         treatment = case_when(plot == 2 | plot == 4 ~ 'Control',
                               plot == 1 | plot == 3 ~ 'Air Warming',
                               plot == 6 | plot == 8 ~ 'Soil Warming',
                               plot == 5 | plot == 7 ~ 'Air + Soil Warming'),
         doy.snow.free = yday(flux)) %>% 
  select(colnames(snow.free.2009.2016))

snow.free.2018 <- read_excel('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/Computer_Backups/Healy cabin computer backup/2018/CiPEHR_DryPEHR/Winter_2017_2018/Date Plots Snow Free_2018.xlsx',
                             sheet = 1) %>%
  slice(-1) %>%
  select(plot = `Plot Number`, flux = Flux) %>%
  mutate(flux = as_date(as.numeric(flux), origin = '1899-12-30')) %>% # excel uses 1900-01-01, but I think there is a difference in indexing that is causing the 2 day offset?
  separate(plot, into = c('fence', 'plot'), sep = '_',  convert = TRUE) %>%
  mutate(flux.year = year(flux),
         treatment = case_when(plot == 2 | plot == 4 ~ 'Control',
                               plot == 1 | plot == 3 ~ 'Air Warming',
                               plot == 6 | plot == 8 ~ 'Soil Warming',
                               plot == 5 | plot == 7 ~ 'Air + Soil Warming'),
         doy.snow.free = yday(flux)) %>% 
  select(colnames(snow.free.2009.2016))

snow.free.2019 <- read_excel('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/Computer_Backups/Healy cabin computer backup/2019/CiPEHR_DryPEHR/Phenology/SnowFree/Date Plots Snow Free_2019.xlsx',
                             sheet = 1) %>%
  slice(-1) %>%
  select(plot = `Plot Number`, flux = Flux) %>%
  mutate(flux = as_date(as.numeric(flux), origin = '1899-12-30')) %>% # excel uses 1900-01-01, but I think there is a difference in indexing that is causing the 2 day offset?
  separate(plot, into = c('fence', 'plot'), sep = '_',  convert = TRUE) %>%
  mutate(flux.year = year(flux),
         treatment = case_when(plot == 2 | plot == 4 ~ 'Control',
                               plot == 1 | plot == 3 ~ 'Air Warming',
                               plot == 6 | plot == 8 ~ 'Soil Warming',
                               plot == 5 | plot == 7 ~ 'Air + Soil Warming'),
         doy.snow.free = yday(flux)) %>% 
  select(colnames(snow.free.2009.2016))

snow.free.2020 <- read_excel('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/Computer_Backups/Healy cabin computer backup/2020/CiPEHR_DryPEHR/Phenology/SnowFree/Date Plots Snow Free_2020.xlsx',
                             sheet = 1) %>%
  slice(-1) %>%
  select(plot = `Plot Number`, flux = Flux) %>%
  mutate(flux = as_date(as.numeric(flux), origin = '1899-12-30')) %>% # excel uses 1900-01-01, but I think there is a difference in indexing that is causing the 2 day offset?
  separate(plot, into = c('fence', 'plot'), sep = '_',  convert = TRUE) %>%
  mutate(flux.year = year(flux),
         treatment = case_when(plot == 2 | plot == 4 ~ 'Control',
                               plot == 1 | plot == 3 ~ 'Air Warming',
                               plot == 6 | plot == 8 ~ 'Soil Warming',
                               plot == 5 | plot == 7 ~ 'Air + Soil Warming'),
         doy.snow.free = yday(flux)) %>% 
  select(colnames(snow.free.2009.2016))

snow.free <- snow.free.2009.2016 %>%
  rbind.data.frame(snow.free.2017) %>%
  rbind.data.frame(snow.free.2018) %>%
  rbind.data.frame(snow.free.2019) %>%
  rbind.data.frame(snow.free.2020)
snow.free <- as.data.table(snow.free)
snow.free <- snow.free[, .(doy.snow.free = round(mean(doy.snow.free, na.rm = TRUE))),
                       by = c('flux.year', 'treatment')]
rm(snow.free.2009.2016, snow.free.2017, snow.free.2018, snow.free.2019, 
   snow.free.2020)

snow.env <- merge(snow.env, snow.free, 
                  by = c('flux.year', 'treatment'),
                  all = TRUE)
###########################################################################################

### Merge Datasets ########################################################################
### Merge with CO2 Data
### plot frame to join environmental data that doesn't have plot information with
plot.frame <- expand_grid(fence = seq(1, 6),
                          plot = seq(1, 8),
                          date = parse_date_time(seq(ymd('2008-09-01'),
                                                     ymd('2020-09-30'),
                                                     by = 'days'),
                                                 orders = c('Y!-m!*-d!')),
                          hourmin = as.numeric(seq(0, 23.5, by = 0.5))) %>%
  mutate(hour = floor(hourmin),
         mins = ifelse(hourmin == floor(hourmin),
                      0,
                      30),
         ts = parse_date_time(paste(date, paste(hour, mins, sep = ':')),
                              orders = c('Y!-m!*-d! H!:M!')),
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
weather.f <- weather.f[date >= ymd('2008-09-01'),]
weather.f <- merge(weather.f, plot.frame, by = c('date', 'hour'),
                 allow.cartesian = TRUE)
flux <- merge(co2, weather.f,
              by = c('ts', 'year', 'month', 'week', 'doy', 'date', 'hour',
                     'hourmin', 'fence', 'plot', 'plot.id', 'treatment'),
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
              by = c('ts', 'year', 'month', 'week', 'doy', 'date', 'hour',
                     'hourmin', 'fence', 'plot', 'plot.id', 'treatment'),
              all = TRUE)

### Water Table Depth
# set the key to allow proper rolling join
setkey(wtd.f, fence, treatment, plot.id, date)
setkey(flux, fence, treatment, plot.id, date)

# Rolling join of water table depth and flux
# This joins by matching treatment and plot id and finding the closest date match between
# wtd and flux. In cases where there are two thaw depths equally distant in time
# from the flux measurement, it will return both, resulting in a longer data table than 
# the flux input. 
flux <- wtd.f[flux, roll = 'nearest']

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
setkey(td.f, year, fence, treatment, plot.id, date)
setkey(flux, year, fence, treatment, plot.id, date)

# Rolling join of thaw depth and flux
# This joins by matching treatment and plot id and finding the closest date match between
# thaw depth and flux. In cases where there are two thaw depths equally distant in time
# from the flux measurement, it will return both, resulting in a longer data table than 
# the flux input. 
flux <- td.f[flux, roll = 'nearest']

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

### Subsidence
flux <- merge(flux,
              sub,
              by = c('year', 'fence', 'plot', 'treatment'),
              all = TRUE)

### ALT
flux <- merge(flux,
              alt.f,
              by = c('year', 'fence', 'plot.id', 'treatment'),
              all = TRUE)

flux <- flux[order(ts, plot.id)]
flux[, tp := alt - subsidence]
flux[, tp.to.date := td - subsidence]

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

### Add block and season variables
flux[,
     season := ifelse(month <= 4 | month >= 10,
                     0,
                     1)]
flux[,
     block := ifelse(fence <= 2,
                     'a',
                     ifelse(fence <= 4,
                            'b',
                            'c'))]

### Snow
flux <- merge(flux,
              snow.f,
              by = c('year','block', 'fence', 'plot', 'treatment'),
              all = TRUE)

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

### Merge Environmental Data Only
# Join all data separated by treatment
env.treat <- merge(snow.env, alt.env,
                   by = c('flux.year', 'treatment'),
                   all = TRUE)
env.treat <- merge(env.treat, wtd.env,
                   by = c('flux.year', 'treatment'),
                   all = TRUE)

# Join all data separated by season
###########################################################################################

### Gap Fill ##############################################################################
### Clean up
rm(alt, biomass, chambt, co2, ndvi, plot.frame, soil.sensor, sub,
   td, td.2009, weather, well.assignment, wtd, wtd.2009, snow)
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

### Could maybe assign deep soil temps to neighboring plots of same treatment?
###########################################################################################

### Create Summaries and Derived Variables ################################################
flux.final <- flux[, .(ts, date, year, season, month, week, doy, hour, hourmin, block,
                       fence, plot, plot.id, treatment, deployed, filled, nee,
                       reco, gpp, r2, par, tair, t5, t10, t20, t40, vwc, gwc,
                       wtd, td, biomass, ndvi, ndvi.date, precip, rh, subsidence, alt,
                       alt.date, tp, tp.to.date, snow.depth)]
### Daily Summary
flux.daily <- flux.final[,
                         .(deployed = first(deployed),
                           nee.sum = fifelse(all(is.na(nee)),
                                            NaN,
                                            sum(nee, na.rm = TRUE)),
                           reco.sum = fifelse(all(is.na(reco)),
                                             NaN,
                                             sum(reco, na.rm = TRUE)),
                           gpp.sum = fifelse(all(is.na(gpp)),
                                            NaN,
                                            sum(gpp, na.rm = TRUE)),
                           tair.max = max(tair, na.rm = TRUE),
                           tair.mean = mean(tair, na.rm = TRUE),
                           tair.min = min(tair, na.rm = TRUE),
                           tair.sd = fifelse(all(is.na(tair)),
                                             NaN,
                                             sd(tair, na.rm = TRUE)),
                           growing.days = mean(tair, na.rm = TRUE) - 5,
                           freezing.days = mean(tair, na.rm = TRUE)*-1,
                           t5.max = max(t5, na.rm = TRUE),
                           t5.mean = mean(t5, na.rm = TRUE),
                           t5.min = min(t5, na.rm = TRUE),
                           t5.sd = fifelse(all(is.na(t5)),
                                           NaN,
                                           sd(t5, na.rm = TRUE)),
                           t10.max = max(t10, na.rm = TRUE),
                           t10.mean = mean(t10, na.rm = TRUE),
                           t10.min = min(t10, na.rm = TRUE),
                           t10.sd = fifelse(all(is.na(t10)),
                                            NaN,
                                            sd(t10, na.rm = TRUE)),
                           t20.max = max(t20, na.rm = TRUE),
                           t20.mean = mean(t20, na.rm = TRUE),
                           t20.min = min(t20, na.rm = TRUE),
                           t20.sd = fifelse(all(is.na(t20)),
                                            NaN,
                                            sd(t20, na.rm = TRUE)),
                           t40.max = max(t40, na.rm = TRUE),
                           t40.mean = mean(t40, na.rm = TRUE),
                           t40.min = min(t40, na.rm = TRUE),
                           t40.sd = fifelse(all(is.na(t40)),
                                            NaN,
                                            sd(t40, na.rm = TRUE)),
                           vwc.max = max(vwc, na.rm = TRUE),
                           vwc.mean = mean(vwc, na.rm = TRUE),
                           vwc.min = min(vwc, na.rm = TRUE),
                           vwc.sd = fifelse(all(is.na(vwc)),
                                            NaN,
                                            sd(vwc, na.rm = TRUE)),
                           gwc.max = max(gwc, na.rm = TRUE),
                           gwc.mean = mean(gwc, na.rm = TRUE),
                           gwc.min = min(gwc, na.rm = TRUE),
                           gwc.sd = fifelse(all(is.na(gwc)),
                                            NaN,
                                            sd(gwc, na.rm = TRUE)),
                           wtd = first(wtd),
                           precip = fifelse(all(is.na(precip)),
                                           NaN,
                                           sum(precip, na.rm = TRUE)),
                           rh.max = max(rh, na.rm = TRUE),
                           rh.mean = mean(rh, na.rm = TRUE),
                           rh.min = min(rh, na.rm = TRUE),
                           rh.sd = fifelse(all(is.na(rh)),
                                           NaN,
                                           sd(rh, na.rm = TRUE)),
                           subsidence.annual = first(subsidence),
                           td = first(td),
                           tp.to.date = first(tp.to.date),
                           alt.annual = first(alt),
                           alt.doy = yday(first(alt.date)),
                           tp.annual = first(tp),
                           biomass.annual = first(biomass),
                           ndvi = first(ndvi),
                           ndvi.doy = yday(first(ndvi.date)),
                           winter.snow.depth = max(snow.depth, na.rm = TRUE)),
                         by = c('date', 'year', 'season', 'month', 'week',
                                'doy', 'block', 'fence', 'plot', 'plot.id',
                                'treatment')]
flux.daily <- flux.daily[, lapply(.SD, function(x) replace(x, list = is.infinite(x), values = NA))]
flux.daily <- flux.daily[, lapply(.SD, function(x) replace(x, list = is.nan(x), values = NA))]

flux.daily[growing.days < 0,
           growing.days := 0]
flux.daily[freezing.days < 0,
           freezing.days := 0]
flux.daily[is.na(precip),
           precip := 0]
flux.daily[, ':=' (flux.year = ifelse(month >= 10,
                                      year +1,
                                      year))]
flux.daily[, ':=' (gdd = cumsum(growing.days),
                   fdd = cumsum(freezing.days),
                   precip.cum = cumsum(precip)),
           by = c('flux.year', 'block', 'fence', 'plot')]

flux.daily[,
           ':=' (tair.spread = tair.max - tair.min,
                 gdd.2d = frollsum(growing.days, n = 2, align = 'right', na.rm = TRUE), # 2 days prior + current day
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
                 rh.mean.2d = frollmean(rh.mean, n = 2, align = 'right', na.rm = TRUE), # day prior + current day
                 rh.mean.3d = frollmean(rh.mean, n = 3, align = 'right', na.rm = TRUE), # 2 days prior + current day
                 rh.mean.4d = frollmean(rh.mean, n = 4, align = 'right', na.rm = TRUE), # 3 days prior + current day
                 rh.mean.5d = frollmean(rh.mean, n = 5, align = 'right', na.rm = TRUE), # day prior + current day
                 rh.mean.6d = frollmean(rh.mean, n = 6, align = 'right', na.rm = TRUE), # day prior + current day
                 rh.mean.1w = frollmean(rh.mean, n = 7, align = 'right', na.rm = TRUE), # full week prior including current day
                 rh.mean.2w = frollmean(rh.mean, n = 14, align = 'right', na.rm = TRUE), # 2 weeks prior including current day
                 rh.max.2d = frollapply(rh.max, n = 2, align = 'right', FUN = max, na.rm = TRUE), # day prior + current day
                 rh.max.3d = frollapply(rh.max, n = 3, align = 'right', FUN = max, na.rm = TRUE), # 2 days prior + current day
                 rh.max.4d = frollapply(rh.max, n = 4, align = 'right', FUN = max, na.rm = TRUE), # 3 days prior + current day
                 rh.max.5d = frollapply(rh.max, n = 5, align = 'right', FUN = max, na.rm = TRUE), # day prior + current day
                 rh.max.6d = frollapply(rh.max, n = 6, align = 'right', FUN = max, na.rm = TRUE), # day prior + current day
                 rh.max.1w = frollapply(rh.max, n = 7, align = 'right', FUN = max, na.rm = TRUE), # full week prior including current day
                 rh.max.2w = frollapply(rh.max, n = 14, align = 'right', FUN = max, na.rm = TRUE), # 2 weeks prior including current day
                 rh.min.2d = frollapply(rh.min, n = 2, align = 'right', FUN = min, na.rm = TRUE), # day prior + current day
                 rh.min.3d = frollapply(rh.min, n = 3, align = 'right', FUN = min, na.rm = TRUE), # 2 days prior + current day
                 rh.min.4d = frollapply(rh.min, n = 4, align = 'right', FUN = min, na.rm = TRUE), # 3 days prior + current day
                 rh.min.5d = frollapply(rh.min, n = 5, align = 'right', FUN = min, na.rm = TRUE), # day prior + current day
                 rh.min.6d = frollapply(rh.min, n = 6, align = 'right', FUN = min, na.rm = TRUE), # day prior + current day
                 rh.min.1w = frollapply(rh.min, n = 7, align = 'right', FUN = min, na.rm = TRUE), # full week prior including current day
                 rh.min.2w = frollapply(rh.min, n = 14, align = 'right', FUN = min, na.rm = TRUE), # 2 weeks prior including current day
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
           by = c('block', 'fence', 'plot')]
# remove NaN, -Inf, and Inf introduced by calculations
flux.daily <- flux.daily[, lapply(.SD, function(x) replace(x, list = is.infinite(x), values = NA))]
flux.daily <- flux.daily[, lapply(.SD, function(x) replace(x, list = is.nan(x), values = NA))]

# check that everything worked as expected
# View(flux.daily[order(plot.id), .SD, .SDcols = c('date', 'fence', 'plot', 'tair.mean', 'growing.days', grep('gdd', colnames(flux.daily), value = TRUE))])
# View(flux.daily[order(plot.id), .SD, .SDcols = c('date', 'fence', 'plot', 'tair.mean', 'freezing.days', grep('fdd', colnames(flux.daily), value = TRUE))])
# View(flux.daily[order(plot.id), .SD, .SDcols = c('date', 'fence', 'plot', grep('precip', colnames(flux.daily), value = TRUE))])
# View(flux.daily[order(plot.id), .SD, .SDcols = c('date', 'fence', 'plot', grep('vwc.mean', colnames(flux.daily), value = TRUE))])
# View(flux.daily[order(plot.id), .SD, .SDcols = c('date', 'fence', 'plot', grep('vwc.max', colnames(flux.daily), value = TRUE))])
# View(flux.daily[order(plot.id), .SD, .SDcols = c('date', 'fence', 'plot', grep('vwc.min', colnames(flux.daily), value = TRUE))])
# View(flux.daily[order(plot.id), .SD, .SDcols = c('date', 'fence', 'plot', grep('gwc.mean', colnames(flux.daily), value = TRUE))])
# View(flux.daily[order(plot.id), .SD, .SDcols = c('date', 'fence', 'plot', grep('gwc.max', colnames(flux.daily), value = TRUE))])
# View(flux.daily[order(plot.id), .SD, .SDcols = c('date', 'fence', 'plot', grep('gwc.min', colnames(flux.daily), value = TRUE))])
# View(flux.daily[order(plot.id), .SD, .SDcols = c('date', 'fence', 'plot', grep('t5.mean', colnames(flux.daily), value = TRUE))])
# View(flux.daily[order(plot.id), .SD, .SDcols = c('date', 'fence', 'plot', grep('t5.max', colnames(flux.daily), value = TRUE))])
# View(flux.daily[order(plot.id), .SD, .SDcols = c('date', 'fence', 'plot', grep('t5.min', colnames(flux.daily), value = TRUE))])

# write.csv(flux.daily, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_daily.csv',
#           row.names = FALSE)


# only keep rows with flux data for output
flux.daily.neat <- flux.daily[season == 1 & year >= 2009]
# write.csv(flux.daily.neat, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_daily_neat.csv',
#           row.names = FALSE)

### Winter environmental conditions
env.winter <- flux.daily[season == 0,
                         .(winter.fdd = sum(freezing.days, na.rm = TRUE),
                           winter.min.tair.min = min(tair.min, na.rm = TRUE),
                           winter.mean.tair.min = mean(tair.min, na.rm = TRUE),
                           winter.tair.mean = mean(tair.mean, na.rm = TRUE),
                           winter.tair.sd = fifelse(all(is.na(tair.sd)),
                                                    NaN,
                                                    sqrt(sum(tair.sd, na.rm = TRUE)/.N)),
                           winter.max.tair.spread = max(tair.spread, na.rm = TRUE),
                           winter.mean.tair.spread = mean(tair.spread, na.rm = TRUE),
                           winter.min.tair.spread = min(tair.spread, na.rm = TRUE),
                           winter.min.t5.min = min(t5.min, na.rm = TRUE),
                           winter.t5.mean = mean(t5.mean, na.rm = TRUE),
                           winter.t5.sd = fifelse(all(is.na(t5.sd)),
                                                  NaN,
                                                  sqrt(sum(t5.sd, na.rm = TRUE)/.N)),
                           winter.min.t10.min = min(t10.min, na.rm = TRUE),
                           winter.t10.mean = mean(t10.mean, na.rm = TRUE),
                           winter.t10.sd = fifelse(all(is.na(t10.sd)),
                                                   NaN,
                                                   sqrt(sum(t10.sd, na.rm = TRUE)/.N)),
                           winter.min.t20.min = min(t20.min, na.rm = TRUE),
                           winter.t20.mean = mean(t20.mean, na.rm = TRUE),
                           winter.t20.sd = fifelse(all(is.na(t20.sd)),
                                                   NaN,
                                                   sqrt(sum(t20.sd, na.rm = TRUE)/.N)),
                           winter.min.t40.min = min(t40.min, na.rm = TRUE),
                           winter.t40.mean = mean(t40.mean, na.rm = TRUE),
                           winter.t40.sd = fifelse(all(is.na(t40.sd)),
                                                   NaN,
                                                   sqrt(sum(t40.sd, na.rm = TRUE)/.N))),
                         by = c('flux.year', 'block', 'fence', 'plot',
                                'plot.id', 'treatment')]
# remove NaN, -Inf, and Inf introduced by calculations
env.winter <- env.winter[, lapply(.SD, function(x) replace(x, list = is.infinite(x), values = NA))]
env.winter <- env.winter[, lapply(.SD, function(x) replace(x, list = is.nan(x), values = NA))]

env.winter[is.na(winter.t20.mean),
           winter.t20.sd := NA]
env.winter[is.na(winter.t40.mean),
           winter.t40.sd := NA]
# write.csv(env.winter, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/env_winter.csv',
#           row.names = FALSE)

### Weekly Summary
flux.weekly <- flux.daily[,
                          .(deployed = max(deployed),
                            year = first(year),
                            season = ceiling(mean(season)),
                            month = first(month),
                            nee.sum = ifelse(all(is.na(nee.sum)),
                                             NaN,
                                             sum(nee.sum, na.rm = TRUE)),
                            reco.sum = ifelse(all(is.na(reco.sum)),
                                              NaN,
                                              sum(reco.sum, na.rm = TRUE)),
                            gpp.sum = ifelse(all(is.na(gpp.sum)),
                                             NaN,
                                             sum(gpp.sum, na.rm = TRUE)),
                            max.tair.max = max(tair.max, na.rm = TRUE),
                            mean.tair.max = mean(tair.max, na.rm = TRUE),
                            tair.mean = mean(tair.mean, na.rm = TRUE),
                            mean.tair.min = mean(tair.min, na.rm = TRUE),
                            min.tair.min = min(tair.min, na.rm = TRUE),
                            tair.sd = fifelse(all(is.na(tair.sd)),
                                              NaN,
                                              sqrt(sum(tair.sd, na.rm = TRUE)/.N)),
                            max.tair.spread = max(tair.spread, na.rm = TRUE),
                            mean.tair.spread = mean(tair.spread, na.rm = TRUE),
                            min.tair.spread = min(tair.spread, na.rm = TRUE),
                            gdd.current = sum(growing.days, na.rm = TRUE),
                            fdd.current = sum(freezing.days, na.rm = TRUE),
                            gdd = max(gdd, na.rm = TRUE),
                            fdd = max(fdd, na.rm = TRUE),
                            max.t5.max = max(t5.max, na.rm = TRUE),
                            mean.t5.max = mean(t5.max, na.rm = TRUE),
                            t5.mean = mean(t5.mean, na.rm = TRUE),
                            mean.t5.min = mean(t5.min, na.rm = TRUE),
                            min.t5.min = min(t5.min, na.rm = TRUE),
                            t5.sd = fifelse(all(is.na(t5.sd)),
                                            NaN,
                                            sqrt(sum(t5.sd, na.rm = TRUE)/.N)),
                            max.t10.max = max(t10.max, na.rm = TRUE),
                            mean.t10.max = mean(t10.max, na.rm = TRUE),
                            t10.mean = mean(t10.mean, na.rm = TRUE),
                            mean.t10.min = mean(t10.min, na.rm = TRUE),
                            min.t10.min = min(t10.min, na.rm = TRUE),
                            t10.sd = fifelse(all(is.na(t10.sd)),
                                             NaN,
                                             sqrt(sum(t10.sd, na.rm = TRUE)/.N)),
                            max.t20.max = max(t20.max, na.rm = TRUE),
                            mean.t20.max = mean(t20.max, na.rm = TRUE),
                            t20.mean = mean(t20.mean, na.rm = TRUE),
                            mean.t20.min = mean(t20.min, na.rm = TRUE),
                            min.t20.min = min(t20.min, na.rm = TRUE),
                            t20.sd = fifelse(all(is.na(t20.sd)),
                                             NaN,
                                             sqrt(sum(t20.sd, na.rm = TRUE)/.N)),
                            max.t40.max = max(t40.max, na.rm = TRUE),
                            mean.t40.max = mean(t40.max, na.rm = TRUE),
                            t40.mean = mean(t40.mean, na.rm = TRUE),
                            mean.t40.min = mean(t40.min, na.rm = TRUE),
                            min.t40.min = min(t40.min, na.rm = TRUE),
                            t40.sd = fifelse(all(is.na(t40.sd)),
                                             NaN,
                                             sqrt(sum(t40.sd, na.rm = TRUE)/.N)),
                            max.vwc.max = max(vwc.max, na.rm = TRUE),
                            mean.vwc.max = mean(vwc.max, na.rm = TRUE),
                            vwc.mean = mean(vwc.mean, na.rm = TRUE),
                            mean.vwc.min = mean(vwc.min, na.rm = TRUE),
                            min.vwc.min = min(vwc.min, na.rm = TRUE),
                            vwc.sd = fifelse(all(is.na(vwc.sd)),
                                             NaN,
                                             sqrt(sum(vwc.sd, na.rm = TRUE)/.N)),
                            max.gwc.max = max(gwc.max, na.rm = TRUE),
                            mean.gwc.max = mean(gwc.max, na.rm = TRUE),
                            gwc.mean = mean(gwc.mean, na.rm = TRUE),
                            mean.gwc.min = mean(gwc.min, na.rm = TRUE),
                            min.gwc.min = min(gwc.min, na.rm = TRUE),
                            gwc.sd = fifelse(all(is.na(gwc.sd)),
                                             NaN,
                                             sqrt(sum(gwc.sd, na.rm = TRUE)/.N)),
                            wtd.mean = mean(wtd, na.rm = TRUE),
                            wtd.sd = sd(wtd, na.rm = TRUE),
                            wtd.n = length(unique(!is.na(wtd))),
                            max.rh.max = max(rh.max, na.rm = TRUE),
                            mean.rh.max = mean(rh.max, na.rm = TRUE),
                            rh.mean = mean(rh.mean, na.rm = TRUE),
                            mean.rh.min = mean(rh.min, na.rm = TRUE),
                            min.rh.min = min(rh.min, na.rm = TRUE),
                            rh.sd = fifelse(all(is.na(rh.sd)),
                                            NaN,
                                            sqrt(sum(rh.sd, na.rm = TRUE)/.N)),
                            precip = ifelse(all(is.na(precip)),
                                                NaN,
                                                sum(precip, na.rm = TRUE)),
                            precip.cum = last(precip.cum),
                            subsidence.annual = first(subsidence.annual),
                            td = max(td, na.rm = TRUE),
                            tp.to.date = max(tp.to.date, na.rm = TRUE),
                            alt.annual = first(alt.annual),
                            alt.doy = first(alt.doy),
                            tp.annual = first(tp.annual),
                            biomass.annual = first(biomass.annual),
                            ndvi = max(ndvi),
                            ndvi.doy = first(ndvi.doy),
                            winter.snow.depth = max(winter.snow.depth, na.rm = TRUE)),
                          by = c('flux.year', 'week', 'block', 'fence',
                                 'plot', 'plot.id', 'treatment')]
# remove NaN, -Inf, and Inf introduced by calculations
flux.weekly <- flux.weekly[, lapply(.SD, function(x) replace(x, list = is.infinite(x), values = NA))]
flux.weekly <- flux.weekly[, lapply(.SD, function(x) replace(x, list = is.nan(x), values = NA))]

flux.weekly[,
           ':=' (gdd.2w = frollsum(gdd, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                 gdd.3w = frollsum(gdd, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week 
                 gdd.1m = frollsum(gdd, n = 4, align = 'right', na.rm = TRUE), # full month including current week
                 fdd.2w = frollsum(fdd, n = 2, align = 'right', na.rm = TRUE), # previous week + current week 
                 fdd.3w = frollsum(fdd, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week 
                 fdd.1m = frollsum(fdd, n = 4, align = 'right', na.rm = TRUE), # full month including current week
                 precip.2w = frollsum(precip, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                 precip.3w = frollsum(precip, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                 precip.1m = frollsum(precip, n = 4, align = 'right', na.rm = TRUE), # full month including current week
                 rh.mean.2w = frollmean(rh.mean, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                 rh.mean.3w = frollmean(rh.mean, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                 rh.mean.1m = frollmean(rh.mean, n = 4, align = 'right', na.rm = TRUE), # full month including current week
                 max.rh.max.2w = frollmean(max.rh.max, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                 max.rh.max.3w = frollmean(max.rh.max, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                 max.rh.max.1m = frollmean(max.rh.max, n = 4, align = 'right', na.rm = TRUE), # full month including current week
                 mean.rh.max.2w = frollmean(mean.rh.max, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                 mean.rh.max.3w = frollmean(mean.rh.max, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                 mean.rh.max.1m = frollmean(mean.rh.max, n = 4, align = 'right', na.rm = TRUE), # full month including current week
                 min.rh.min.2w = frollmean(min.rh.min, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                 min.rh.min.3w = frollmean(min.rh.min, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                 min.rh.min.1m = frollmean(min.rh.min, n = 4, align = 'right', na.rm = TRUE), # full month including current week
                 mean.rh.min.2w = frollmean(mean.rh.min, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                 mean.rh.min.3w = frollmean(mean.rh.min, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                 mean.rh.min.1m = frollmean(mean.rh.min, n = 4, align = 'right', na.rm = TRUE), # full month including current week
                 vwc.mean.2w = frollmean(vwc.mean, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                 vwc.mean.3w = frollmean(vwc.mean, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                 vwc.mean.1m = frollmean(vwc.mean, n = 4, align = 'right', na.rm = TRUE), # full month including current week
                 max.vwc.max.2w = frollmean(max.vwc.max, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                 max.vwc.max.3w = frollmean(max.vwc.max, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                 max.vwc.max.1m = frollmean(max.vwc.max, n = 4, align = 'right', na.rm = TRUE), # full month including current week
                 mean.vwc.max.2w = frollmean(mean.vwc.max, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                 mean.vwc.max.3w = frollmean(mean.vwc.max, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                 mean.vwc.max.1m = frollmean(mean.vwc.max, n = 4, align = 'right', na.rm = TRUE), # full month including current week
                 min.vwc.min.2w = frollmean(min.vwc.min, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                 min.vwc.min.3w = frollmean(min.vwc.min, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                 min.vwc.min.1m = frollmean(min.vwc.min, n = 4, align = 'right', na.rm = TRUE), # full month including current week
                 mean.vwc.min.2w = frollmean(mean.vwc.min, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                 mean.vwc.min.3w = frollmean(mean.vwc.min, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                 mean.vwc.min.1m = frollmean(mean.vwc.min, n = 4, align = 'right', na.rm = TRUE), # full month including current week
                 gwc.mean.2w = frollmean(gwc.mean, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                 gwc.mean.3w = frollmean(gwc.mean, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                 gwc.mean.1m = frollmean(gwc.mean, n = 4, align = 'right', na.rm = TRUE), # full month including current week
                 max.gwc.max.2w = frollmean(max.gwc.max, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                 max.gwc.max.3w = frollmean(max.gwc.max, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                 max.gwc.max.1m = frollmean(max.gwc.max, n = 4, align = 'right', na.rm = TRUE), # full month including current week
                 mean.gwc.max.2w = frollmean(mean.gwc.max, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                 mean.gwc.max.3w = frollmean(mean.gwc.max, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                 mean.gwc.max.1m = frollmean(mean.gwc.max, n = 4, align = 'right', na.rm = TRUE), # full month including current week
                 min.gwc.min.2w = frollmean(min.gwc.min, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                 min.gwc.min.3w = frollmean(min.gwc.min, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                 min.gwc.min.1m = frollmean(min.gwc.min, n = 4, align = 'right', na.rm = TRUE), # full month including current week
                 mean.gwc.min.2w = frollmean(mean.gwc.min, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                 mean.gwc.min.3w = frollmean(mean.gwc.min, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                 mean.gwc.min.1m = frollmean(mean.gwc.min, n = 4, align = 'right', na.rm = TRUE)), # full month including current week
           by = c('fence', 'plot')]

# remove NaN, -Inf, and Inf introduced by calculations
flux.weekly <- flux.weekly[, lapply(.SD, function(x) replace(x, list = is.infinite(x), values = NA))]
flux.weekly <- flux.weekly[, lapply(.SD, function(x) replace(x, list = is.nan(x), values = NA))]

# write.csv(flux.weekly, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_weekly.csv',
#           row.names = FALSE)

# only keep growing season for data export
flux.weekly.neat <- flux.weekly[season == 1 & year >= 2009]
# write.csv(flux.weekly.neat, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_weekly_neat.csv',
#           row.names = FALSE)

### Monthly Summary
flux.monthly <- flux.daily[,
                          .(year = first(year),
                            season = ceiling(mean(season)),
                            nee.sum = ifelse(all(is.na(nee.sum)),
                                             NaN,
                                             sum(nee.sum, na.rm = TRUE)),
                            reco.sum = ifelse(all(is.na(reco.sum)),
                                              NaN,
                                              sum(reco.sum, na.rm = TRUE)),
                            gpp.sum = ifelse(all(is.na(gpp.sum)),
                                             NaN,
                                             sum(gpp.sum, na.rm = TRUE)),
                            max.tair.max = max(tair.max, na.rm = TRUE),
                            mean.tair.max = mean(tair.max, na.rm = TRUE),
                            tair.mean = mean(tair.mean, na.rm = TRUE),
                            mean.tair.min = mean(tair.min, na.rm = TRUE),
                            min.tair.min = min(tair.min, na.rm = TRUE),
                            tair.sd = fifelse(all(is.na(tair.sd)),
                                              NaN,
                                              sqrt(sum(tair.sd, na.rm = TRUE)/.N)),
                            max.tair.spread = max(tair.spread, na.rm = TRUE),
                            mean.tair.spread = mean(tair.spread, na.rm = TRUE),
                            min.tair.spread = min(tair.spread, na.rm = TRUE),
                            gdd.current = sum(growing.days, na.rm = TRUE),
                            fdd.current = sum(freezing.days, na.rm = TRUE),
                            gdd = max(gdd, na.rm = TRUE),
                            fdd = max(fdd, na.rm = TRUE),
                            max.t5.max = max(t5.max, na.rm = TRUE),
                            mean.t5.max = mean(t5.max, na.rm = TRUE),
                            t5.mean = mean(t5.mean, na.rm = TRUE),
                            mean.t5.min = mean(t5.min, na.rm = TRUE),
                            min.t5.min = min(t5.min, na.rm = TRUE),
                            t5.sd = fifelse(all(is.na(t5.sd)),
                                            NaN,
                                            sqrt(sum(t5.sd, na.rm = TRUE)/.N)),
                            max.t10.max = max(t10.max, na.rm = TRUE),
                            mean.t10.max = mean(t10.max, na.rm = TRUE),
                            t10.mean = mean(t10.mean, na.rm = TRUE),
                            mean.t10.min = mean(t10.min, na.rm = TRUE),
                            min.t10.min = min(t10.min, na.rm = TRUE),
                            t10.sd = fifelse(all(is.na(t10.sd)),
                                             NaN,
                                             sqrt(sum(t10.sd, na.rm = TRUE)/.N)),
                            max.t20.max = max(t20.max, na.rm = TRUE),
                            mean.t20.max = mean(t20.max, na.rm = TRUE),
                            t20.mean = mean(t20.mean, na.rm = TRUE),
                            mean.t20.min = mean(t20.min, na.rm = TRUE),
                            min.t20.min = min(t20.min, na.rm = TRUE),
                            t20.sd = fifelse(all(is.na(t20.sd)),
                                             NaN,
                                             sqrt(sum(t20.sd, na.rm = TRUE)/.N)),
                            max.t40.max = max(t40.max, na.rm = TRUE),
                            mean.t40.max = mean(t40.max, na.rm = TRUE),
                            t40.mean = mean(t40.mean, na.rm = TRUE),
                            mean.t40.min = mean(t40.min, na.rm = TRUE),
                            min.t40.min = min(t40.min, na.rm = TRUE),
                            t40.sd = fifelse(all(is.na(t40.sd)),
                                             NaN,
                                             sqrt(sum(t40.sd, na.rm = TRUE)/.N)),
                            max.vwc.max = max(vwc.max, na.rm = TRUE),
                            mean.vwc.max = mean(vwc.max, na.rm = TRUE),
                            vwc.mean = mean(vwc.mean, na.rm = TRUE),
                            mean.vwc.min = mean(vwc.min, na.rm = TRUE),
                            min.vwc.min = min(vwc.min, na.rm = TRUE),
                            vwc.sd = fifelse(all(is.na(vwc.sd)),
                                             NaN,
                                             sqrt(sum(vwc.sd, na.rm = TRUE)/.N)),
                            max.gwc.max = max(gwc.max, na.rm = TRUE),
                            mean.gwc.max = mean(gwc.max, na.rm = TRUE),
                            gwc.mean = mean(gwc.mean, na.rm = TRUE),
                            mean.gwc.min = mean(gwc.min, na.rm = TRUE),
                            min.gwc.min = min(gwc.min, na.rm = TRUE),
                            gwc.sd = fifelse(all(is.na(gwc.sd)),
                                             NaN,
                                             sqrt(sum(gwc.sd, na.rm = TRUE)/.N)),
                            wtd.mean = mean(wtd, na.rm = TRUE),
                            wtd.sd = sd(wtd, na.rm = TRUE),
                            wtd.n = length(unique(!is.na(wtd))),
                            max.rh.max = max(rh.max, na.rm = TRUE),
                            mean.rh.max = mean(rh.max, na.rm = TRUE),
                            rh.mean = mean(rh.mean, na.rm = TRUE),
                            mean.rh.min = mean(rh.min, na.rm = TRUE),
                            min.rh.min = min(rh.min, na.rm = TRUE),
                            rh.sd = fifelse(all(is.na(rh.sd)),
                                            NaN,
                                            sqrt(sum(rh.sd, na.rm = TRUE)/.N)),
                            precip = ifelse(all(is.na(precip)),
                                            NaN,
                                            sum(precip, na.rm = TRUE)),
                            precip.cum = last(precip.cum),
                            subsidence.annual = first(subsidence.annual),
                            td = max(td, na.rm = TRUE),
                            tp.to.date = max(tp.to.date, na.rm = TRUE),
                            alt.annual = first(alt.annual),
                            alt.doy = first(alt.doy),
                            tp.annual = first(tp.annual),
                            biomass.annual = first(biomass.annual),
                            ndvi = max(ndvi),
                            ndvi.doy = first(ndvi.doy),
                            winter.snow.depth = max(winter.snow.depth, na.rm = TRUE)),
                          by = c('flux.year', 'month', 'block', 'fence',
                                 'plot', 'plot.id', 'treatment')]
# remove NaN, -Inf, and Inf introduced by calculations
flux.monthly <- flux.monthly[, lapply(.SD, function(x) replace(x, list = is.infinite(x), values = NA))]
flux.monthly <- flux.monthly[, lapply(.SD, function(x) replace(x, list = is.nan(x), values = NA))]

flux.monthly[,
            ':=' (gdd.2m = frollsum(gdd, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                  gdd.3m = frollsum(gdd, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week 
                  fdd.2m = frollsum(fdd, n = 2, align = 'right', na.rm = TRUE), # previous week + current week 
                  fdd.3m = frollsum(fdd, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week 
                  precip.2m = frollsum(precip, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                  precip.3m = frollsum(precip, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                  rh.mean.2m = frollmean(rh.mean, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                  rh.mean.3m = frollmean(rh.mean, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                  max.rh.max.2m = frollmean(max.rh.max, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                  max.rh.max.3m = frollmean(max.rh.max, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                  mean.rh.max.2m = frollmean(mean.rh.max, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                  mean.rh.max.3m = frollmean(mean.rh.max, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                  min.rh.min.2m = frollmean(min.rh.min, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                  min.rh.min.3m = frollmean(min.rh.min, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                  mean.rh.min.2m = frollmean(mean.rh.min, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                  mean.rh.min.3m = frollmean(mean.rh.min, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                  vwc.mean.2m = frollmean(vwc.mean, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                  vwc.mean.3m = frollmean(vwc.mean, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                  max.vwc.max.2m = frollmean(max.vwc.max, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                  max.vwc.max.3m = frollmean(max.vwc.max, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                  mean.vwc.max.2m = frollmean(mean.vwc.max, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                  mean.vwc.max.3m = frollmean(mean.vwc.max, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                  min.vwc.min.2m = frollmean(min.vwc.min, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                  min.vwc.min.3m = frollmean(min.vwc.min, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                  mean.vwc.min.2m = frollmean(mean.vwc.min, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                  mean.vwc.min.3m = frollmean(mean.vwc.min, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                  gwc.mean.2m = frollmean(gwc.mean, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                  gwc.mean.3m = frollmean(gwc.mean, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                  max.gwc.max.2m = frollmean(max.gwc.max, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                  max.gwc.max.3m = frollmean(max.gwc.max, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                  mean.gwc.max.2m = frollmean(mean.gwc.max, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                  mean.gwc.max.3m = frollmean(mean.gwc.max, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                  min.gwc.min.2m = frollmean(min.gwc.min, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                  min.gwc.min.3m = frollmean(min.gwc.min, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                  mean.gwc.min.2m = frollmean(mean.gwc.min, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                  mean.gwc.min.3m = frollmean(mean.gwc.min, n = 3, align = 'right', na.rm = TRUE)), # 2 previous weeks + current week
            by = c('fence', 'plot')]

# remove NaN, -Inf, and Inf introduced by calculations
flux.monthly <- flux.monthly[, lapply(.SD, function(x) replace(x, list = is.infinite(x), values = NA))]
flux.monthly <- flux.monthly[, lapply(.SD, function(x) replace(x, list = is.nan(x), values = NA))]

# write.csv(flux.monthly, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_monthly.csv',
#           row.names = FALSE)

# only keep growing season for data export
flux.monthly.neat <- flux.monthly[season == 1 & year >= 2009]
# write.csv(flux.monthly.neat, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_monthly_neat.csv',
#           row.names = FALSE)


### Annual
flux.annual <- flux.weekly[season == 1,
                          .(nee.sum = fifelse(all(is.na(nee.sum)),
                                             NaN,
                                             sum(nee.sum, na.rm = TRUE)),
                            reco.sum = fifelse(all(is.na(reco.sum)),
                                              NaN,
                                              sum(reco.sum, na.rm = TRUE)),
                            gpp.sum = fifelse(all(is.na(gpp.sum)),
                                             NaN,
                                             sum(gpp.sum, na.rm = TRUE)),
                            max.tair.max = max(max.tair.max, na.rm = TRUE),
                            mean.tair.max = mean(mean.tair.max, na.rm = TRUE),
                            tair.mean = mean(tair.mean, na.rm = TRUE),
                            mean.tair.min = mean(mean.tair.min, na.rm = TRUE),
                            min.tair.min = min(min.tair.min, na.rm = TRUE),
                            tair.sd = fifelse(all(is.na(tair.sd)),
                                              NaN,
                                              sqrt(sum(tair.sd, na.rm = TRUE)/.N)),
                            max.tair.spread = max(max.tair.spread, na.rm = TRUE),
                            mean.tair.spread = mean(mean.tair.spread, na.rm = TRUE),
                            min.tair.spread = min(min.tair.spread, na.rm = TRUE),
                            gdd = max(gdd, na.rm = TRUE),
                            fdd = max(fdd, na.rm = TRUE),
                            max.t5.max = max(max.t5.max, na.rm = TRUE),
                            mean.t5.max = mean(mean.t5.max, na.rm = TRUE),
                            t5.mean = mean(t5.mean, na.rm = TRUE),
                            mean.t5.min = mean(mean.t5.min, na.rm = TRUE),
                            min.t5.min = min(min.t5.min, na.rm = TRUE),
                            t5.sd = fifelse(all(is.na(t5.sd)),
                                            NaN,
                                            sqrt(sum(t5.sd, na.rm = TRUE)/.N)),
                            max.t10.max = max(max.t10.max, na.rm = TRUE),
                            mean.t10.max = mean(mean.t10.max, na.rm = TRUE),
                            t10.mean = mean(t10.mean, na.rm = TRUE),
                            mean.t10.min = mean(mean.t10.min, na.rm = TRUE),
                            min.t10.min = min(min.t10.min, na.rm = TRUE),
                            t10.sd = fifelse(all(is.na(t10.sd)),
                                             NaN,
                                             sqrt(sum(t10.sd, na.rm = TRUE)/.N)),
                            max.t20.max = max(max.t20.max, na.rm = TRUE),
                            mean.t20.max = mean(mean.t20.max, na.rm = TRUE),
                            t20.mean = mean(t20.mean, na.rm = TRUE),
                            mean.t20.min = mean(mean.t20.min, na.rm = TRUE),
                            min.t20.min = min(min.t20.min, na.rm = TRUE),
                            t20.sd = fifelse(all(is.na(t20.sd)),
                                             NaN,
                                             sqrt(sum(t20.sd, na.rm = TRUE)/.N)),
                            max.t40.max = max(max.t40.max, na.rm = TRUE),
                            mean.t40.max = mean(mean.t40.max, na.rm = TRUE),
                            t40.mean = mean(t40.mean, na.rm = TRUE),
                            mean.t40.min = mean(mean.t40.min, na.rm = TRUE),
                            min.t40.min = min(min.t40.min, na.rm = TRUE),
                            t40.sd = fifelse(all(is.na(t40.sd)),
                                             NaN,
                                             sqrt(sum(t40.sd, na.rm = TRUE)/.N)),
                            max.vwc.max = max(max.vwc.max, na.rm = TRUE),
                            mean.vwc.max = mean(mean.vwc.max, na.rm = TRUE),
                            vwc.mean = mean(vwc.mean, na.rm = TRUE),
                            mean.vwc.min = mean(mean.vwc.min, na.rm = TRUE),
                            min.vwc.min = min(min.vwc.min, na.rm = TRUE),
                            vwc.sd = fifelse(all(is.na(vwc.sd)),
                                             NaN,
                                             sqrt(sum(vwc.sd, na.rm = TRUE)/.N)),
                            max.gwc.max = max(max.gwc.max, na.rm = TRUE),
                            mean.gwc.max = mean(mean.gwc.max, na.rm = TRUE),
                            gwc.mean = mean(gwc.mean, na.rm = TRUE),
                            mean.gwc.min = mean(mean.gwc.min, na.rm = TRUE),
                            min.gwc.min = min(min.gwc.min, na.rm = TRUE),
                            gwc.sd = fifelse(all(is.na(gwc.sd)),
                                             NaN,
                                             sqrt(sum(gwc.sd, na.rm = TRUE)/.N)),
                            wtd.mean = mean(wtd.mean, na.rm = TRUE),
                            wtd.sd = fifelse(all(is.na(wtd.mean)),
                                             NaN,
                                             sd(wtd.mean, na.rm = TRUE)),
                            wtd.n = sum(wtd.n),
                            max.rh.max = max(max.rh.max, na.rm = TRUE),
                            mean.rh.max = mean(mean.rh.max, na.rm = TRUE),
                            rh.mean = mean(rh.mean, na.rm = TRUE),
                            mean.rh.min = mean(mean.rh.min, na.rm = TRUE),
                            min.rh.min = min(min.rh.min, na.rm = TRUE),
                            rh.sd = fifelse(all(is.na(rh.sd)),
                                            NaN,
                                            sqrt(sum(rh.sd, na.rm = TRUE)/.N)),
                            precip.sum = sum(precip, na.rm = TRUE),
                            precip.cum = last(precip.cum),
                            subsidence.annual = last(subsidence.annual),
                            alt.annual = first(alt.annual),
                            alt.doy = first(alt.doy),
                            tp.annual = first(tp.annual),
                            biomass.annual = first(biomass.annual),
                            ndvi = max(ndvi, na.rm = TRUE),
                            ndvi.doy = ndvi.doy[which(first(ndvi == max(ndvi, na.rm = TRUE)))],
                            winter.snow.depth = max(winter.snow.depth, na.rm = TRUE)),
                          by = c('flux.year', 'season', 'block', 'fence',
                                 'plot', 'plot.id', 'treatment')]
# remove NaN, -Inf, and Inf introduced by calculations
flux.annual <- flux.annual[, lapply(.SD, function(x) replace(x, list = is.infinite(x), values = NA))]
flux.annual <- flux.annual[, lapply(.SD, function(x) replace(x, list = is.nan(x), values = NA))]

# merge with winter data
flux.annual <- merge(flux.annual,
                     env.winter,
                     by = c('flux.year', 'block', 'fence', 'plot', 'plot.id', 'treatment'))

# write.csv(flux.annual, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_annual.csv',
#           row.names = FALSE)

###########################################################################################

### Plot to Check Merge and Summaries Went Properly #######################################
### Daily
# Fluxes
ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = gpp.sum, color = 'GPP'), alpha = 0.5) +
  geom_line(aes(y = nee.sum, color = 'NEE'), alpha = 0.5) +
  geom_line(aes(y = -1*reco.sum, color = 'Reco'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_y_continuous(name = 'Flux (g C / day)') +
  scale_color_manual(values = c('green', 'blue', 'red')) +
  ggtitle('Daily Fluxes')

# Air temps
ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = tair.max, color = 'Max Air Temp'), alpha = 0.5) +
  geom_line(aes(y = tair.mean, color = 'Mean Air Temp'), alpha = 0.5) +
  geom_line(aes(y = tair.min, color = 'Min Air Temp'), alpha = 0.5) +
  geom_line(aes(y = tair.spread, color = 'Air Temp Spread'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('purple', 'red', 'black', 'blue')) +
  ggtitle('Air Temperature')

# GDD and FDD
ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = fdd, color = 'FDD'), alpha = 0.5) +
  geom_line(aes(y = gdd, color = 'GDD'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('blue', 'red')) +
  ggtitle('Growing Degree Days and Freezing Degree Days')

# Soil Temps
ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = t5.max, color = 'Max Air Temp'), alpha = 0.5) +
  geom_line(aes(y = t5.mean, color = 'Mean Air Temp'), alpha = 0.5) +
  geom_line(aes(y = t5.min, color = 'Min Air Temp'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue')) +
  ggtitle('5 cm Soil Temperature')

ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = t10.max, color = 'Max Air Temp'), alpha = 0.5) +
  geom_line(aes(y = t10.mean, color = 'Mean Air Temp'), alpha = 0.5) +
  geom_line(aes(y = t10.min, color = 'Min Air Temp'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue')) +
  ggtitle('10 cm Soil Temperature')

ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = t20.max, color = 'Max Air Temp'), alpha = 0.5) +
  geom_line(aes(y = t20.mean, color = 'Mean Air Temp'), alpha = 0.5) +
  geom_line(aes(y = t20.min, color = 'Min Air Temp'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue')) +
  ggtitle('20 cm Soil Temperature')

ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = t40.max, color = 'Max Air Temp'), alpha = 0.5) +
  geom_line(aes(y = t40.mean, color = 'Mean Air Temp'), alpha = 0.5) +
  geom_line(aes(y = t40.min, color = 'Min Air Temp'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue')) +
  ggtitle('40 cm Soil Temperature')

# Soil Moisture
ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = vwc.max, color = 'Max VWC'), alpha = 0.5) +
  geom_line(aes(y = vwc.mean, color = 'Mean VWC'), alpha = 0.5) +
  geom_line(aes(y = vwc.min, color = 'Min VWC'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue')) +
  ggtitle('VWC')

ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = vwc.sd, color = 'SD VWC'), alpha = 0.5) +
  facet_grid(fence~plot) +
  ggtitle('VWC SD')

ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = gwc.max, color = 'Max GWC'), alpha = 0.5) +
  geom_line(aes(y = gwc.mean, color = 'Mean GWC'), alpha = 0.5) +
  geom_line(aes(y = gwc.min, color = 'Min GWC'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue')) +
  ggtitle('GWC')

ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = gwc.sd, color = 'SD GWC'), alpha = 0.5) +
  facet_grid(fence~plot) +
  ggtitle('GWC SD')

ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = wtd, color = 'WTD'), alpha = 0.5) +
  facet_grid(fence~plot) +
  ggtitle('Water Table Depth')

# Precipitation
ggplot(flux.daily, aes(x = date)) +
  geom_step(aes(y = precip, color = 'Daily Precip'), alpha = 0.5) +
  geom_step(aes(y = precip.cum, color = 'Cumulative Precip'), alpha = 0.5) +
  scale_color_manual(values = c('blue', 'black')) +
  ggtitle('Precipitation')

# Relative Humidity
ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = rh.max, color = 'Max RH'), alpha = 0.5) +
  geom_line(aes(y = rh.mean, color = 'Mean RH'), alpha = 0.5) +
  geom_line(aes(y = rh.min, color = 'Min RH'), alpha = 0.5) +
  scale_color_manual(values = c('red', 'black', 'blue')) +
  ggtitle('Relative Humidity')

# Subsidence, Thaw Depth, TP, ALT
ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = td*-1, color = 'Thaw Depth'), alpha = 0.5) +
  geom_line(aes(y = tp.to.date*-1, color = 'Thaw Penetration'), alpha = 0.5) +
  scale_color_manual(values = c('black', 'red')) +
  facet_grid(fence~plot) +
  ggtitle('Thaw Depth')

# NDVI
ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = ndvi), alpha = 0.5) +
  facet_grid(fence~plot) +
  ggtitle('NDVI')

# Check lagged variables for a few plots
ggplot(flux.daily[year == 2010 & plot.id == '1_1' &
                    as_date(date) >= as_date('2010-06-01') &
                    as_date(date) < as_date('2010-10-01')],
       aes(x = date)) +
  geom_line(aes(y = vwc.mean, color = 'VWC'), alpha = 0.5) +
  geom_line(aes(y = vwc.mean.2d, color = 'VWC 2 Day'), alpha = 0.5) +
  geom_line(aes(y = vwc.mean.3d, color = 'VWC 3 Day'), alpha = 0.5) +
  geom_line(aes(y = vwc.mean.4d, color = 'VWC 4 Day'), alpha = 0.5) +
  geom_line(aes(y = vwc.mean.4d, color = 'VWC 5 Day'), alpha = 0.5) +
  geom_line(aes(y = vwc.mean.4d, color = 'VWC 6 Day'), alpha = 0.5) +
  geom_line(aes(y = vwc.mean.4d, color = 'VWC 7 Days'), alpha = 0.5) +
  scale_color_manual(values = c('black', 'gray15', 'gray30', 'gray45',
                                'gray60', 'gray75', 'gray90')) +
  facet_grid(fence~plot) +
  ggtitle('VWC')

ggplot(flux.daily[year == 2010 & plot.id == '1_1'],
       aes(x = date)) +
  geom_line(aes(y = gdd.2d, color = 'GDD 2 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.3d, color = 'GDD 3 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.4d, color = 'GDD 4 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.5d, color = 'GDD 5 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.6d, color = 'GDD 6 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.1w, color = 'GDD 7 Days'), alpha = 0.5) +
  scale_color_manual(values = c('black', 'gray15', 'gray30', 'gray45',
                                'gray60', 'gray75', 'gray90')) +
  facet_grid(fence~plot) +
  ggtitle('GDD')

ggplot(flux.daily[year == 2010 & plot.id == '4_6'],
       aes(x = date)) +
  geom_line(aes(y = gdd.2d, color = 'GDD 2 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.3d, color = 'GDD 3 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.4d, color = 'GDD 4 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.5d, color = 'GDD 5 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.6d, color = 'GDD 6 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.1w, color = 'GDD 7 Days'), alpha = 0.5) +
  scale_color_manual(values = c('black', 'gray15', 'gray30', 'gray45',
                                'gray60', 'gray75', 'gray90')) +
  facet_grid(fence~plot) +
  ggtitle('GDD')

ggplot(flux.daily[year == 2015 & plot.id == '1_1'],
       aes(x = date)) +
  geom_line(aes(y = gdd.2d, color = 'GDD 2 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.3d, color = 'GDD 3 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.4d, color = 'GDD 4 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.5d, color = 'GDD 5 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.6d, color = 'GDD 6 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.1w, color = 'GDD 7 Days'), alpha = 0.5) +
  scale_color_manual(values = c('black', 'gray15', 'gray30', 'gray45',
                                'gray60', 'gray75', 'gray90')) +
  facet_grid(fence~plot) +
  ggtitle('GDD')

ggplot(flux.daily[year == 2015 & plot.id == '4_6'],
       aes(x = date)) +
  geom_line(aes(y = gdd.2d, color = 'GDD 2 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.3d, color = 'GDD 3 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.4d, color = 'GDD 4 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.5d, color = 'GDD 5 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.6d, color = 'GDD 6 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.1w, color = 'GDD 7 Days'), alpha = 0.5) +
  scale_color_manual(values = c('black', 'gray15', 'gray30', 'gray45',
                                'gray60', 'gray75', 'gray90')) +
  facet_grid(fence~plot) +
  ggtitle('GDD')

### Weekly
# Fluxes
flux.weekly[, year.decimal := year + week/max(week), by = 'year']

ggplot(flux.weekly, aes(x = year.decimal)) +
  geom_line(aes(y = gpp.sum, color = 'GPP'), alpha = 0.5) +
  geom_line(aes(y = nee.sum, color = 'NEE'), alpha = 0.5) +
  geom_line(aes(y = -1*reco.sum, color = 'Reco'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_y_continuous(name = 'Flux (g C / week)') +
  scale_color_manual(values = c('green', 'blue', 'red')) +
  ggtitle('Weekly Fluxes')

# Air temps
ggplot(flux.weekly, aes(x = year.decimal)) +
  geom_line(aes(y = max.tair.max, color = 'Max Air Temp'), alpha = 0.5) +
  geom_line(aes(y = tair.mean, color = 'Mean Air Temp'), alpha = 0.5) +
  geom_line(aes(y = min.tair.min, color = 'Min Air Temp'), alpha = 0.5) +
  geom_line(aes(y = mean.tair.spread, color = 'Air Temp Spread'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('purple', 'red', 'black', 'blue')) +
  ggtitle('Air Temperature')

# GDD and FDD
ggplot(flux.weekly, aes(x = year.decimal)) +
  geom_line(aes(y = fdd, color = 'FDD'), alpha = 0.5) +
  geom_line(aes(y = gdd, color = 'GDD'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('blue', 'red')) +
  ggtitle('Weekly Growing Degree Days and Freezing Degree Days')

# Soil Temps
ggplot(flux.weekly, aes(x = year.decimal)) +
  geom_line(aes(y = max.t5.max, color = 'Max Air Temp'), alpha = 0.5) +
  geom_line(aes(y = t5.mean, color = 'Mean Air Temp'), alpha = 0.5) +
  geom_line(aes(y = min.t5.min, color = 'Min Air Temp'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue')) +
  ggtitle('Weekly 5 cm Soil Temperature')

ggplot(flux.weekly, aes(x = year.decimal)) +
  geom_line(aes(y = max.t10.max, color = 'Max Air Temp'), alpha = 0.5) +
  geom_line(aes(y = t10.mean, color = 'Mean Air Temp'), alpha = 0.5) +
  geom_line(aes(y = min.t10.min, color = 'Min Air Temp'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue')) +
  ggtitle('Weekly 10 cm Soil Temperature')

ggplot(flux.weekly, aes(x = year.decimal)) +
  geom_line(aes(y = max.t20.max, color = 'Max Air Temp'), alpha = 0.5) +
  geom_line(aes(y = t20.mean, color = 'Mean Air Temp'), alpha = 0.5) +
  geom_line(aes(y = min.t20.min, color = 'Min Air Temp'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue')) +
  ggtitle('Weekly 20 cm Soil Temperature')

ggplot(flux.weekly, aes(x = year.decimal)) +
  geom_line(aes(y = max.t40.max, color = 'Max Air Temp'), alpha = 0.5) +
  geom_line(aes(y = t40.mean, color = 'Mean Air Temp'), alpha = 0.5) +
  geom_line(aes(y = min.t40.min, color = 'Min Air Temp'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue')) +
  ggtitle('Weekly 40 cm Soil Temperature')

# Soil Moisture
ggplot(flux.weekly, aes(x = year.decimal)) +
  geom_line(aes(y = max.vwc.max, color = 'Max VWC'), alpha = 0.5) +
  geom_line(aes(y = vwc.mean, color = 'Mean VWC'), alpha = 0.5) +
  geom_line(aes(y = min.vwc.min, color = 'Min VWC'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue')) +
  ggtitle('Weekly VWC')

ggplot(flux.weekly, aes(x = year.decimal)) +
  geom_line(aes(y = vwc.sd, color = 'SD VWC'), alpha = 0.5) +
  facet_grid(fence~plot) +
  ggtitle('Weekly VWC SD')

ggplot(flux.weekly, aes(x = year.decimal)) +
  geom_line(aes(y = max.gwc.max, color = 'Max GWC'), alpha = 0.5) +
  geom_line(aes(y = gwc.mean, color = 'Mean GWC'), alpha = 0.5) +
  geom_line(aes(y = min.gwc.min, color = 'Min GWC'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue')) +
  ggtitle('Weekly GWC')

ggplot(flux.weekly, aes(x = year.decimal)) +
  geom_line(aes(y = gwc.sd, color = 'SD GWC'), alpha = 0.5) +
  facet_grid(fence~plot) +
  ggtitle('Weekly GWC SD')

ggplot(flux.weekly, aes(x = year.decimal)) +
  geom_line(aes(y = wtd.mean, color = 'WTD'), alpha = 0.5) +
  facet_grid(fence~plot) +
  ggtitle('Weekly Water Table Depth')

# Precipitation
ggplot(flux.weekly, aes(x = year.decimal)) +
  geom_step(aes(y = precip, color = 'Precip'), alpha = 0.5) +
  geom_step(aes(y = precip.cum, color = 'Cumulative\nPrecip'), alpha = 0.5) +
  scale_color_manual(values = c('blue', 'black')) +
  ggtitle('Weekly Precipitation')

ggplot(flux.weekly, aes(x = week, group = year)) +
  geom_step(aes(y = precip.cum, color = factor(year)), alpha = 0.5) +
  ggtitle('Weekly Precipitation')

# Relative Humidity
ggplot(flux.weekly, aes(x = year.decimal)) +
  geom_line(aes(y = max.rh.max, color = 'Max RH'), alpha = 0.5) +
  geom_line(aes(y = rh.mean, color = 'Mean RH'), alpha = 0.5) +
  geom_line(aes(y = min.rh.min, color = 'Min RH'), alpha = 0.5) +
  scale_color_manual(values = c('red', 'black', 'blue')) +
  ggtitle('Weekly Relative Humidity')

# Thaw Depth, TP
ggplot(flux.weekly, aes(x = year.decimal)) +
  geom_line(aes(y = td*-1, color = 'Thaw Depth'), alpha = 0.5) +
  geom_line(aes(y = tp.to.date*-1, color = 'Thaw Penetration'), alpha = 0.5) +
  scale_color_manual(values = c('black', 'red')) +
  facet_grid(fence~plot) +
  ggtitle('Weekly Thaw Depth')

# NDVI
ggplot(flux.weekly, aes(x = year.decimal)) +
  geom_line(aes(y = ndvi), alpha = 0.5) +
  facet_grid(fence~plot) +
  ggtitle('Weekly NDVI')

# Check lagged variables for a few plots
ggplot(flux.weekly[plot.id == '1_1'],
       aes(x = year.decimal)) +
  geom_line(aes(y = vwc.mean, color = 'VWC'), alpha = 0.5) +
  geom_line(aes(y = vwc.mean.2w, color = 'VWC 2 Week'), alpha = 0.5) +
  geom_line(aes(y = vwc.mean.3w, color = 'VWC 3 Week'), alpha = 0.5) +
  geom_line(aes(y = vwc.mean.1m, color = 'VWC 4 Week'), alpha = 0.5) +
  scale_color_manual(values = c('black', 'gray15', 'gray30', 'gray45')) +
  facet_grid(fence~plot) +
  ggtitle('Weekly VWC')

ggplot(flux.weekly[plot.id == '4_6'],
       aes(x = year.decimal)) +
  geom_line(aes(y = vwc.mean, color = 'VWC'), alpha = 0.5) +
  geom_line(aes(y = vwc.mean.2w, color = 'VWC 2 Week'), alpha = 0.5) +
  geom_line(aes(y = vwc.mean.3w, color = 'VWC 3 Week'), alpha = 0.5) +
  geom_line(aes(y = vwc.mean.1m, color = 'VWC 4 Week'), alpha = 0.5) +
  scale_color_manual(values = c('black', 'gray15', 'gray30', 'gray45')) +
  facet_grid(fence~plot) +
  ggtitle('Weekly VWC')

ggplot(flux.daily[year == 2010 & plot.id == '4_6'],
       aes(x = date)) +
  geom_line(aes(y = gdd.2d, color = 'GDD 2 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.3d, color = 'GDD 3 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.4d, color = 'GDD 4 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.5d, color = 'GDD 5 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.6d, color = 'GDD 6 Day'), alpha = 0.5) +
  geom_line(aes(y = gdd.1w, color = 'GDD 7 Days'), alpha = 0.5) +
  scale_color_manual(values = c('black', 'gray15', 'gray30', 'gray45',
                                'gray60', 'gray75', 'gray90')) +
  facet_grid(fence~plot) +
  ggtitle('GDD')

### Annual
ggplot(flux.annual, aes(x = flux.year)) +
  geom_hline(yintercept = 0) +
  geom_line(aes(y = gpp.sum, color = 'GPP'), alpha = 0.5) +
  geom_line(aes(y = nee.sum, color = 'NEE'), alpha = 0.5) +
  geom_line(aes(y = -1*reco.sum, color = 'Reco'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_y_continuous(name = 'Flux (g C / yr)') +
  scale_color_manual(values = c('green', 'blue', 'red')) +
  ggtitle('Annual Fluxes')

# Air Temps
ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = max.tair.max, color = 'Max Air Temp'), alpha = 0.5) +
  geom_line(aes(y = tair.mean, color = 'Mean Air Temp'), alpha = 0.5) +
  geom_line(aes(y = min.tair.min, color = 'Min Air Temp'), alpha = 0.5) +
  geom_line(aes(y = winter.min.tair.min, color = 'Prior Winter Min Air Temp'), alpha = 0.5) +
  geom_line(aes(y = mean.tair.spread, color = 'Air Temp Spread'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('purple', 'red', 'black', 'blue', 'turquoise')) +
  ggtitle('Annual Air Temperature')

# GDD and FDD
ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = fdd, color = 'FDD'), alpha = 0.5) +
  geom_line(aes(y = gdd, color = 'GDD'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('blue', 'red')) +
  ggtitle('Annual Growing Degree Days and Freezing Degree Days')

# Soil Temps
ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = max.t5.max, color = 'Max Air Temp'), alpha = 0.5) +
  geom_line(aes(y = t5.mean, color = 'Mean Air Temp'), alpha = 0.5) +
  geom_line(aes(y = min.t5.min, color = 'Min Air Temp'), alpha = 0.5) +
  geom_line(aes(y = winter.min.t5.min, color = 'Prior Winter Min Air Temp'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue', 'turquoise')) +
  ggtitle('Annual 5 cm Soil Temperature')

ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = max.t10.max, color = 'Max Air Temp'), alpha = 0.5) +
  geom_line(aes(y = t10.mean, color = 'Mean Air Temp'), alpha = 0.5) +
  geom_line(aes(y = min.t10.min, color = 'Min Air Temp'), alpha = 0.5) +
  geom_line(aes(y = winter.min.t10.min, color = 'Prior Winter Min Air Temp'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue', 'turquoise')) +
  ggtitle('Annual 10 cm Soil Temperature')

ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = max.t20.max, color = 'Max Air Temp'), alpha = 0.5) +
  geom_line(aes(y = t20.mean, color = 'Mean Air Temp'), alpha = 0.5) +
  geom_line(aes(y = min.t20.min, color = 'Min Air Temp'), alpha = 0.5) +
  geom_line(aes(y = min.t20.min, color = 'Min Air Temp'), alpha = 0.5) +
  geom_line(aes(y = winter.min.t20.min, color = 'Prior Winter Min Air Temp'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue', 'turquoise')) +
  ggtitle('Annual 20 cm Soil Temperature')

ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = max.t40.max, color = 'Max Air Temp'), alpha = 0.5) +
  geom_line(aes(y = t40.mean, color = 'Mean Air Temp'), alpha = 0.5) +
  geom_line(aes(y = min.t40.min, color = 'Min Air Temp'), alpha = 0.5) +
  geom_line(aes(y = winter.min.t40.min, color = 'Prior Winter Min Air Temp'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue', 'turquoise')) +
  ggtitle('Annual 40 cm Soil Temperature')

# Soil Moisture
ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = max.vwc.max, color = 'Max VWC'), alpha = 0.5) +
  geom_line(aes(y = vwc.mean, color = 'Mean VWC'), alpha = 0.5) +
  geom_line(aes(y = min.vwc.min, color = 'Min VWC'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue')) +
  ggtitle('Annual VWC')

ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = vwc.sd, color = 'SD VWC'), alpha = 0.5) +
  facet_grid(fence~plot) +
  ggtitle('Annual VWC SD')

ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = max.gwc.max, color = 'Max GWC'), alpha = 0.5) +
  geom_line(aes(y = gwc.mean, color = 'Mean GWC'), alpha = 0.5) +
  geom_line(aes(y = min.gwc.min, color = 'Min GWC'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue')) +
  ggtitle('Annual GWC')

ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = gwc.sd, color = 'SD GWC'), alpha = 0.5) +
  facet_grid(fence~plot) +
  ggtitle('Annual GWC SD')

ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = wtd.mean, color = 'WTD')) +
  geom_line(aes(y = wtd.sd, color = 'WTD SD')) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('blue', 'black')) +
  ggtitle('Annual Water Table Depth')

# Precipitation
ggplot(flux.annual, aes(x = flux.year)) +
  geom_col(aes(y = precip.sum/100, color = 'Precip', fill = 'Precip')) +
  scale_y_continuous(name = 'Precip (cm)') +
  scale_color_manual(values = c('blue'),
                     guide = FALSE) +
  scale_fill_manual(values = c('blue'),
                    guide = FALSE) +
  ggtitle('Annual Precipitation')

# Relative Humidity
ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = max.rh.max, color = 'Max RH'), alpha = 0.5) +
  geom_line(aes(y = rh.mean, color = 'Mean RH'), alpha = 0.5) +
  geom_line(aes(y = min.rh.min, color = 'Min RH'), alpha = 0.5) +
  scale_color_manual(values = c('red', 'black', 'blue')) +
  ggtitle('Annual Relative Humidity')

# Thaw Depth, TP
ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = alt.annual*-1, color = 'ALT'), alpha = 0.5) +
  geom_line(aes(y = tp.annual*-1, color = 'Thaw Penetration'), alpha = 0.5) +
  scale_color_manual(values = c('black', 'red')) +
  facet_grid(fence~plot) +
  ggtitle('Annual Thaw Depth')

# NDVI
ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = ndvi), alpha = 0.5) +
  facet_grid(fence~plot) +
  ggtitle('Annual Max NDVI')

# Snow Depth
ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = winter.snow.depth), alpha = 0.5) +
  facet_grid(fence~plot) +
  ggtitle('Annual Snow Depth')

###########################################################################################