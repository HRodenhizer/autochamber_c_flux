#############################################################################################################################
###                           Compile Flux Data and Environmental Variables for Modeling                                  ###
###                                                code by HGR 2/2020                                                     ###
#############################################################################################################################

# currently, half-hourly hobo data are missing for 2009-2011. I have gap-filled 
# with eddy data for that period, rather than try to re-process the raw data.

### To Do
# Where on earth is "season" coming from in flux.annual?
# substitute deep soil temps from nearby plots for ones without probes?

### Load Libraries ##########################################################################################################
library(lubridate)
library(berryFunctions)
library(data.table)
library(readxl)
library(sf)
library(tidyselect)
library(zoo)
library(viridis)
library(tidyverse)
#############################################################################################################################


### Half Hourly Data
### co2 Data ##############################################################################
### Load Fluxes
co2 <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/fluxes/Flux_data_halfhourly_modelled_2009_2021.csv")
co2 <- co2[!is.na(doy)]
co2 <- unique(co2)

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
    # print(i)
    
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
        select(year, fence, plot, doy.old = DOY, doy.new = DOY, hour.old = hour, hour.new = hour, CO2_r2 = rsquare, T_chamb = chambT.mean)
      
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
               select(year, fence, plot, doy.old, doy.new, hour.old, hour.new, CO2_r2 = rsquare, T_chamb = chambT.mean)
      )
      
    } else {
      
      print('Your file does not match any of the supported file types (csv, Rdata)')
      
    }
    
    data.dt <- rbind(data.dt, temp)
    
  }
  
  return(data.dt)
  
}

filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_slopes',
                        pattern = 'csv$|Rdata',
                        full.names = TRUE)
r2 <- load.r2(filenames)

### Join flux and r2 data
# use doy.old because both datasets had incorrect doy in some entries in 2018
# and correct doy variable could only be made in r2 since flux didn't have a timestamp
co2 <- merge(co2, r2, by = c('year', 'doy.old', 'hour.old', 'fence', 'plot'), all.x = TRUE)
# ggplot(co2, aes(x = doy, y = T_chamb)) +
#   geom_point() +
#   facet_grid(year ~ .) +
#   scale_y_continuous(limits = c(-50, 50))
# 
# rm(r2)

### Format data
# Create doy variable with correct (new) dates
co2[year >= 2012 & !is.na(doy.new), doy := doy.new]
co2[year >= 2012 & !is.na(hour.new), hour := hour.new]

### A bunch of data with incorrect hourmin info got filled during 2018 processing, I think
# I am removing the modeled fluxes that we have data for
co2[, duplicated.data := .N > 1, by = c('year', 'doy', 'hour', 'fence', 'plot')]
co2[, n := .N, by = c('year', 'doy', 'hour', 'fence', 'plot')]
setorder(co2, year, doy, hour, fence, plot)
co2 <- co2[!(year == 2018 & duplicated.data == TRUE & n == 2 & is.na(CO2_r2))]

# Filter out modeled data and empty rows
# co2 <- co2[filled == 1,]
# co2 <- co2[!is.na(Reco_g)]
co2[,.N, by = c('year')] # make sure there are about the equal numbers in all years except 2019, which should have fewer

# Date
co2[, date := parse_date_time(as_date(doy-1, origin = paste0(year, '-01-01')), orders = c('Y!-m!*-d!'))] # doy-1 because as_date is 0 indexed, while lubridate::yday() (used to create doy variable) is 1 indexed
co2[hour == round(hour), minute := 0]
co2[hour != round(hour), minute := 30]
co2[, ts := parse_date_time(paste(date, paste(floor(hour), minute, sep = ':')), orders = c('Y!-m!*-d! H!:M!'))]
co2[, month := month(ts)]
co2[, week := week(ts)]
co2[, day := mday(date)]
co2[, Flux_Date := paste(day, month, year, sep = '/')]

# treatment
co2[plot %in% c(2, 4), treatment := 'Control']
co2[plot %in% c(1, 3), treatment := 'Air Warming']
co2[plot %in% c(6, 8), treatment := 'Soil Warming']
co2[plot %in% c(5, 7), treatment := 'Air + Soil Warming']

# Plot ID
co2[, plot.id := paste(fence, plot, sep = '_')]
co2 <- co2[order(date, plot.id)]

# ggplot(co2, aes(x = doy, y = T_chamb)) +
#   geom_point() +
#   facet_grid(year ~ treatment) +
#   scale_y_continuous(limits = c(-50, 50))

# 2009-2011 chamber temps
filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/ITEX/warmxresp/2020_09_Dataset request/flux_data/co2/chamb_t_2009-2011/',
                        pattern = 'csv$',
                        full.names = TRUE)
chambt <- map_dfr(filenames,
                  ~ read.csv(.x))
chambt <- data.table(chambt)
chambt <- chambt[!is.na(Tchamb_fill), .(year, doy, hour, fence, plot, Tchamb_fill)]

# treatment
chambt[plot %in% c(2, 4), treatment := 'Control']
chambt[plot %in% c(1, 3), treatment := 'Air Warming']
chambt[plot %in% c(6, 8), treatment := 'Soil Warming']
chambt[plot %in% c(5, 7), treatment := 'Air + Soil Warming']

# Remove Plot information for early 2009 measurements (before plots had been established)
chambt <- chambt[year == 2009 & doy <= 152, plot := NA]

# summarize any duplicates
chambt <- chambt[, 
                 .(Tchamb_fill = mean(Tchamb_fill, na.rm = TRUE)), 
                 by = c('year', 'doy', 'hour', 'fence', 'plot', 'treatment')]

### Join flux and chamber temp data ###
# use doy and hour because the doy and hour variables were correct from the start for 2009-2011
co2 <- merge(co2, chambt, 
             by = c('year', 'doy', 'hour', 'fence', 'plot', 'treatment'), 
             all.x = TRUE)
co2[is.na(T_chamb) & !is.na(Tchamb_fill), T_chamb := Tchamb_fill]
# Remove ridiculous chamber temperatures
co2[T_chamb > 50, T_chamb := NA]

# Select columns
co2 <- co2[, .(ts, date, year, month, week, doy, hourmin = hour, fence, plot, 
               plot.id, treatment, filled, nee = NEE_g, reco = Reco_g, 
               gpp = GPP_g, r2 = CO2_r2, t.chamb = T_chamb)]
co2[,.N, by = year(date)] # make sure there are about the equal numbers in all years except 2019, which should have fewer

## add in hour variable to join with par later
co2[, hour := floor(hourmin)]

# order
co2 <- co2[order(date, plot.id, treatment, hourmin)]

# ggplot(chambt, aes(x = doy, y = Tchamb_fill)) +
#   geom_point() +
#   facet_grid(year ~ treatment)
# ggplot(co2, aes(x = date, y = t.chamb)) +
#   geom_point() +
#   facet_grid(treatment ~ .)
###########################################################################################

### Weather Data ##########################################################################
# ### start with 2 min avgs from early years to get half hourly
# weather.old <- fread('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/weather/2min/EML Met Data 2007-2009 2min avg.txt',
#                      sep = '\t',
#                      na.strings = c('N/A'),
#                      drop = c('#', 'PAR2, uE', 'Pressure, mbar',
#                               'Wind Speed, m/s', 'Gust Speed, m/s', 'Wind Direction, <f8>'),
#                      col.names = c('month', 'day', 'year', 'time',
#                                    'precip', 'PAR', 'Tair', 'RH'))
# weather.old[, ':=' (month = as.numeric(month),
#                     day = as.numeric(day),
#                     Tair = as.numeric(Tair))]
# weather.2008.2009 <- weather.old[year == 2008 & month >= 9 | year == 2009 & month < 10,]
# weather.2008.2009[, ':=' (hour = as.numeric(str_split(time, pattern = ':', simplify = TRUE)[,1]),
#                           min = as.numeric(str_split(time, pattern = ':', simplify = TRUE)[, 2]))]
# weather.2008.2009[, half.hour := fifelse(min < 30,
#                                          0,
#                                          0.5)]
# weather.2008.2009[, hourmin := hour + half.hour]
# weather.2008.2009[, date := paste(year,
#                                   str_pad(month, width = 2, side = 'left', pad = '0'),
#                                   str_pad(day, width = 2, side = 'left', pad = '0'),
#                                   sep = '-')]
# weather.2008.2009 <- weather.2008.2009[, .(DOY = yday(date),
#                                            precip = fifelse(any(!is.na(precip)),
#                                                             sum(precip, na.rm = TRUE),
#                                                             -999),
#                                            PAR = mean(PAR, na.rm = TRUE),
#                                            Tair = mean(Tair, na.rm = TRUE),
#                                            RH = mean(RH, na.rm = TRUE)),
#                                        by = .(date, year, month, day, hourmin)]
# weather.2008.2009[precip == -999, precip := NA]
# write.csv(weather.2008.2009,
#           '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/weather/HOBO_2008-10-01_to_2009-09-30_half_hourly.csv',
#           row.names = FALSE)

filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/weather',
                        pattern = '[csv$ | txt$]',
                        full.names = TRUE)
weather <- map_dfr(filenames,
                   ~ fread(.x,
                           header = TRUE,
                           na.strings = c('', 'NA')) %>%
                     select(year = matches('year', ignore.case = TRUE), 
                            DOY = matches('doy', ignore.case = TRUE),
                            hourmin = matches('hour'), 
                            Tair = matches('Tair', ignore.case = TRUE), 
                            par = PAR,
                            precip = matches('precip', ignore.case = TRUE), 
                            rh = RH))

weather <- data.table(weather)
weather[is.nan(precip), precip := NA]
weather[is.nan(rh), rh := NA]
weather[is.nan(par), par := NA]

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
weather[as_date(date) >= as_date('2010-10-01') & as_date(date) <= as_date('2011-09-30'), hourmin := hourmin - 0.5]
weather[, hour := floor(hourmin)]

# there may be a few duplicated timestamps due to data collection
# should be averaged or summed
weather[, duplicated := .N, by = c('date', 'hourmin')]
weather <- weather[,
                   .(Tair = mean(Tair, na.rm = TRUE),
                     par = mean(par, na.rm = TRUE),
                     precip = fifelse(all(is.na(precip)),
                                      NaN,
                                      sum(precip, na.rm = TRUE)),
                     rh = mean(rh, na.rm = TRUE)),
                   by = .(flux.year, season, date, hourmin)]
weather[is.nan(precip), precip := NA]
weather[is.nan(rh), rh := NA]
weather[is.nan(par), par := NA]
any(weather$duplicated > 1)

weather <- weather[order(date, hourmin)]

# Neaten
# weather data to use for environmental summary
weather.env <- weather[, .(flux.year, season, date, hourmin, Tair, par, precip, rh)]
weather.env <- weather.env[flux.year >= 2009][, .(tair.mean = mean(Tair, na.rm = TRUE),
                                                  precip = sum(precip, na.rm = TRUE),
                                                  par = mean(par, na.rm = TRUE)),
                                              by = c('flux.year', 'season')]
# weather data to use for flux data
weather.f <- weather[, .(flux.year, date, hourmin, Tair, par, precip, rh)]
# double check that the duplicate columns have been removed - this should be TRUE
nrow(unique(weather.f, by = c('date', 'hourmin'))) == nrow(weather.f)
# make sure there are no missing timestamps
times.frame <- expand_grid(date = parse_date_time(seq(ymd('2008-09-01'),
                                                      ymd('2021-09-30'),
                                                      by = 'days'),
                                                  orders = c('Y!-m!*-d!')),
                           hourmin = as.numeric(seq(0, 23.5, by = 0.5)))
weather.f <- merge(weather.f, times.frame, 
                   by = c('date', 'hourmin'),
                   all = TRUE)

# gap fill hourly data from 2009-2011 using eddy data
eddy <- fread('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/Ameriflux/AMF_US-EML_BASE_HH_3-5.csv',
              na.strings = c('-9999'))
eddy[, ts := parse_date_time(as.character(TIMESTAMP_START), orders = c('Y!m!d!H!M!'))]
eddy[, ':=' (date = parse_date_time(as_date(ts), orders = c('Y!-m!-d!')),
             hour = as.numeric(str_sub(as.character(TIMESTAMP_START), start = 9, end = 10)),
             min = as.numeric(str_sub(as.character(TIMESTAMP_START), start = 11, end = 12)))]
eddy[, hourmin := fifelse(min == 0,
                          hour,
                          hour + 0.5)]
eddy <- eddy[, .(date, hourmin, TA, PPFD_IN, RH)]

weather.f <- merge(weather.f, eddy, by = c('date', 'hourmin'), all = TRUE)
weather.f[, year := year(date)]
weather.f[, month := month(date)]
weather.f[, flux.year := fifelse(month >= 10,
                               year + 1,
                               year)]
weather.f[, ':=' (year = NULL,
                  month = NULL)]
tair.model <- lm(Tair ~ TA, data = weather.f)
summary(tair.model)
weather.f[, ':=' (filled.tair = factor(fifelse(is.na(Tair),
                                        1,
                                        0)),
                  Tair = fifelse(is.na(Tair),
                                 tair.model$coefficients[1] + tair.model$coefficients[2]*TA,
                                 Tair))]
# fill in individual missing values that had an NA in both columns
weather.f[, Tair := na.approx(Tair, maxgap = 1)]
weather.f[is.na(Tair), .N, by = c('flux.year')]

# fill all missing precip values during the measurement period with 0 
# (because the sum is already in the hourly value)
# this will have a half day saying it was measured before the container was
# placed in the middle of the day
weather.f[, precip.measured := fifelse(date >= date[first(which(!is.na(precip)))] & 
                                         date <= date[last(which(!is.na(precip)))],
                                       1,
                                       0),
          by = c('flux.year')]
weather.f[, precip.measured := fifelse(date == date[first(which(!is.na(precip)))] &
                                         is.na(precip) |
                                         date == date[last(which(!is.na(precip)))] &
                                         is.na(precip),
                                       0,
                                       precip.measured),
          by = c('flux.year')]
weather.f[, precip := fifelse(precip.measured == 1 & is.na(precip),
                              0,
                              precip)]

# fill PAR
weather.f[, par := na.approx(par, maxgap = 1)]

# fill RH
weather.f[, rh := na.approx(rh, maxgap = 1)]

# clean up columns
weather.f <- weather.f[, .(date, hourmin, flux.year, Tair, par, precip, rh, 
                           filled.tair)]

# ### Save output
# write.csv(weather.f, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/hobo_half_hourly_gap_filled.csv',
#           row.names = FALSE)

# # There are a few months without data at the beginning of 2009. These data 
# # are just missing.
# # check for missing timestamps
# measurement.times <- expand.grid(date = parse_date_time(seq(ymd('2008-10-01'),
#                                                             ymd('2021-09-30'),
#                                                             by = 'days'),
#                                                         orders = c('Y!-m!*-d!')),
#                                  hourmin = as.numeric(seq(0, 23.5, by = 0.5)))
# 
# test <- merge(weather.f, measurement.times, by = c('date', 'hourmin'), all = TRUE)
# 
# ggplot(weather.f[date >= as_date('2009-10-01') & date < as_date('2011-10-01')], aes(date, Tair, color = filled.tair)) +
#   geom_point()
###########################################################################################

### Soil Sensor Data ######################################################################
# filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/soil_sensors/CiPEHR',
#                         pattern = 'csv$|txt$',
#                         full.names = TRUE)
# soil.sensor <- map_dfr(filenames,
#                        ~ read.table(.x,
#                                     header = TRUE,
#                                     sep = ',',
#                                     fill = TRUE) %>%
#                          select(ts = 1, year = matches('[Y|y]ear'), fence = matches('[F|f]ence'),
#                                 plot = matches('[P|p]lot'), hourmin = matches('hour'),
#                                 T_five, T_ten, T_twenty, T_forty, VWC, GWC))
# 
# soil.sensor <- data.table(soil.sensor)
# 
# # Format time variables
# soil.sensor[, date := as_date(parse_date_time(ts, orders = c('Y!-m!*-d!', 'Y!-m!*-d! H!:M!:S!')))]
# soil.sensor[, year := year(date)]
# soil.sensor[, month := month(date)]
# soil.sensor[, week := week(date)]
# soil.sensor[, doy := yday(date)]
# soil.sensor[, day := mday(date)]
# soil.sensor[, date := parse_date_time(paste(year, month, day, sep = '-'), orders = c('Y!-m!*-d!'))]
# soil.sensor[, hour := floor(hourmin)]
# soil.sensor[, minute := hourmin%%1*60]
# soil.sensor[, ts := parse_date_time(paste(date, paste(hour, minute)), orders = c('Y!-m!*-d! H!:M!'))]
# 
# # Treatment
# soil.sensor[plot == 2 | plot == 4, treatment := 'Control']
# soil.sensor[plot == 1 | plot == 3, treatment := 'Air Warming']
# soil.sensor[plot == 6 | plot == 8, treatment := 'Soil Warming']
# soil.sensor[plot == 5 | plot == 7, treatment := 'Air + Soil Warming']
# 
# # Format Plot IDs
# soil.sensor[, plot.id := paste(fence, plot, sep = '_')]
# 
# # Gap fill missing vwc and soil temps in plots without sensors
# soil.sensor[, ':=' (T_twenty = fifelse(is.na(T_twenty),
#                                           mean(T_twenty, na.rm = TRUE),
#                                           T_twenty),
#                     T_forty  = fifelse(is.na(T_forty),
#                                        mean(T_forty, na.rm = TRUE),
#                                        T_forty),
#                     VWC  = fifelse(is.na(VWC),
#                                        mean(VWC, na.rm = TRUE),
#                                        VWC)),
#                     by = .(year, doy, hourmin, fence, treatment)]
# # remove NaN, -Inf, and Inf introduced by calculations
# soil.sensor <- soil.sensor[, lapply(.SD, function(x) replace(x, list = is.infinite(x), values = NA))]
# soil.sensor <- soil.sensor[, lapply(.SD, function(x) replace(x, list = is.nan(x), values = NA))]
# 
# # Neaten
# soil.sensor <- soil.sensor[, .(ts, date, year, month, week, doy, hour, hourmin,
#                                treatment, fence, plot, plot.id, t5 = T_five,
#                                t10 = T_ten, t20 = T_twenty, t40 = T_forty,
#                                vwc = VWC, gwc = GWC)]
# View(soil.sensor[is.na(t10), .N, by = .(year)])
# View(soil.sensor[is.na(t10), .N, by = .(year, plot.id)])
# 
# 
# ########  GAPFILL THE FULL DATASET, FIRST BY FILLING SMALL GAPS BY LINEAR
# ########  INTERPOLATION, THEN BY FILLING LARGER GAPS USING MEDIAN
# ########  PREDICTION FROM AN ENSEMBLE OF REGRESSIONS WITH OTHER SENSORS
# 
# 
# ##Fill small datagaps using linear interpolation between observations.
# soil.sensor[,
#             ':=' (t5 = na.approx(t5, maxgap = 4),
#                   t10 = na.approx(t10, maxgap = 4),
#                   t20 = na.approx(t20, maxgap = 4),
#                   t40 = na.approx(t40, maxgap = 4)),
#             by = .(fence, plot)]
# #
# ###Create a new DF ("meas") of all sensors and fill all gaps based on ensemble predictions
# meas <- dcast(melt(soil.sensor,
#                    id.vars = c('ts', 'plot.id'),
#                    measure.vars = c('t5', 't10', 't20', 't40'),
#                    variable.name = 'depth',
#                    value.name = 'tsoil'),
#               ts ~ depth + plot.id, value.var = 'tsoil')
# meas <- data.frame(meas)##Double check you have all sensor data, but no other columns (e.g. time, month)
# mods <- data.frame(meas)
# mods <- mods %>%
#   mutate(across(t5_1_1:t40_6_8, ~ as.numeric(NA)))
# mods[, 'ts'] <- meas[, 'ts']
# R2s <- data.frame(matrix(nrow = length(names(meas)) - 1, ncol = length(names(meas))))
# colnames(R2s) <- c('gap.filled.sensor', names(meas)[2:length(names(meas))])
# R2s[, 1] <- names(meas)[2:length(names(meas))]
# fits <- data.frame(names(meas)[2:length(names(meas))])
# fits$R2s <- NA
# fits$N.models <- NA
# #This loop makes a matrix of all univariate predictions with an R2 over 89.5
# for(j in 2:ncol(meas)) {
#   print(paste0('j = ', j))
# 
#   for(i in 2:ncol(meas)) {
#     b <- lm(meas[,j] ~ meas[,i])
# 
#     if (summary(b)$r.squared < 0.8) {
#       mods[,i] <- (b$coefficients[1] + (b$coefficients[2]*meas[,i]))
#       R2s[j, i] <- (summary(b)$r.squared)
#     }
# 
#   }
#   mods$med <- apply(select(mods, t5_1_1:t40_6_8), 1, median, na.rm = TRUE) #Take the median prediction of all models for all timestamps
#   fits[j-1, 2] <- (summary(lm(meas[,j] ~ mods$med))$r.squared) # calculate the fit of the predicted values
#   fits[j-1, 3] <- length(as.numeric(R2s[j,])[!is.na(as.numeric(R2s[j,]))])
#   print(summary(lm(meas[,j] ~ mods$med))) #Compare prediction of ensemble models to observed values
#   meas[,j] <- ifelse(is.na(meas[,j]), mods$med, meas[,j])#Replace NA's in the "measured" dataset with model predictions
# 
# }
# 
# # #Replace columns in "all" with gapfilled columns in "meas"
# dates <- dcast(melt(soil.sensor,
#                     id.vars = c('ts', 'plot.id'),
#                     measure.vars = c('t5', 't10', 't20', 't40'),
#                     variable.name = 'depth',
#                     value.name = 'tsoil'),
#                ts ~ depth + plot.id, value.var = 'tsoil')[, .(ts)]
# soil.sensor.ensemble <- melt(data.table(meas),
#                              measure.vars = c(colnames(meas)[2:ncol(meas)]),
#                              variable.name = 'id',
#                              value.name = 'tsoil')
# soil.sensor.ensemble[,
#                      c('depth', 'fence', 'plot') := tstrsplit(id, '_', fixed = TRUE)]
# soil.sensor.ensemble[, ':=' (fence = as.numeric(fence),
#                              plot = as.numeric(plot))]
# soil.sensor.ensemble[, id := NULL]
# soil.sensor.ensemble <- dcast(soil.sensor.ensemble,
#                               ts + fence + plot ~ depth,
#                               value.var = 'tsoil')
# soil.sensor.ensemble <- soil.sensor.ensemble[, .(ts, fence, plot, t5.filled = t5, t10.filled = t10,
#                                                  t20.filled = t20, t40.filled = t40)]
# soil.sensor.filled <- merge(soil.sensor, soil.sensor.ensemble,
#                      all = TRUE,
#                      by = c('ts', 'fence', 'plot'))
# 
# # write.csv(soil.sensor.filled,
# #           '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/soil_sensors/soil_sensor_ensemble_filled.csv',
# #           row.names = FALSE)
# 
# View(soil.sensor[is.na(t10), .N, by = .(year)])
# View(soil.sensor[is.na(t10), .N, by = .(year, plot.id)])
# View(soil.sensor.filled[is.na(t5.filled), .N, by = .(year)])
# View(soil.sensor.filled[is.na(t10.filled), .N, by = .(year)])
# View(soil.sensor.filled[is.na(t10.filled), .N, by = .(year, plot.id)])

soil.sensor <- fread('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/soil_sensors/soil_sensor_ensemble_filled.csv')

ggplot(soil.sensor[year == 2009], aes(x = ts)) +
  geom_point(aes(y = t10.filled, color = "gap filled"), alpha = 0.2) +
  geom_point(aes(y = t10, color = "pre gap fill"), alpha = 0.2) +
  facet_grid(fence ~ plot)

ggplot(soil.sensor[year == 2010], aes(x = ts)) +
  geom_point(aes(y = t10.filled, color = "gap filled"), alpha = 0.2) +
  geom_point(aes(y = t10, color = "pre gap fill"), alpha = 0.2) +
  facet_grid(fence ~ plot)

ggplot(soil.sensor[year == 2018], aes(x = ts)) +
  geom_point(aes(y = t10.filled, color = "gap filled"), alpha = 0.2) +
  geom_point(aes(y = t10, color = "pre gap fill"), alpha = 0.2) +
  facet_grid(fence ~ plot)

# ggplot(soil.sensor[year == 2021], aes(ts, t10)) +
#   geom_line() +
#   facet_grid(fence ~ plot)

### Need to gap fill the rest with eddy tower or 2ET
ec.ameriflux <- fread('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/soil_sensors/AMF_US-EML_BASE_HH_3-5.csv',
                      na.strings = c('-9999', 'NA'))
ec.2018 <- fread('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/soil_sensors/US-EML_HH_201804302330_201904302330.csv',
                 na.strings = c('-9999', 'NA'))

ec.soil.temp <- rbind(ec.ameriflux, ec.2018, use.names = TRUE, fill = TRUE)
ec.soil.temp[, ts := parse_date_time(TIMESTAMP_START, orders = c('Y!m!d!H!M!'))]
ec.soil.temp <- ec.soil.temp[year(ts) == 2018]
ec.soil.temp <- ec.soil.temp[, .(ts, TS_1_1_1, TS_1_2_1, TS_1_3_1, TS_1_4_1, TS_2_1_1, TS_2_2_1,
                 TS_2_3_1, TS_2_4_1)]
# ec.soil.temp[,which(unlist(lapply(ec.soil.temp, function(x)!all(is.na(x))))),with=F]

ggplot(ec.soil.temp, aes(x = ts)) +
  geom_line(aes(y = TS_1_1_1, color = "Probe 1"), alpha = 0.5) +
  geom_line(aes(y = TS_2_1_1, color = "Probe 2"), alpha = 0.5)

ggplot(ec.soil.temp, aes(x = ts)) +
  geom_line(aes(y = TS_1_2_1, color = "Probe 1"), alpha = 0.5) +
  geom_line(aes(y = TS_2_2_1, color = "Probe 2"), alpha = 0.5)

ggplot(ec.soil.temp, aes(x = ts)) +
  geom_line(aes(y = TS_1_3_1, color = "Probe 1"), alpha = 0.5) +
  geom_line(aes(y = TS_2_3_1, color = "Probe 2"), alpha = 0.5)

ggplot(ec.soil.temp, aes(x = ts)) +
  geom_line(aes(y = TS_1_4_1, color = "Probe 1"), alpha = 0.5) +
  geom_line(aes(y = TS_2_4_1, color = "Probe 2"), alpha = 0.5)

# gap fill with ensemble mean from ec soil temperature probes
soil.sensor.wide <- dcast(melt(soil.sensor, 
                               id.vars = c('ts', 'plot.id'), 
                               measure.vars = c('t5.filled', 't10.filled', 't20.filled', 't40.filled'),
                               variable.name = 'depth', 
                               value.name = 'tsoil'), 
                          ts ~ depth + plot.id, value.var = 'tsoil')


final.coefs <- data.table()
final.predictions <- data.table()
for (sensor.n in 2:ncol(soil.sensor.wide)) {
  
  coefs <- data.table()
  predictions <- data.table()
  
  for (ec.n in 2:ncol(ec.soil.temp)) {
    
    cols1 <- c(1, sensor.n)
    soil.sensor.subset <- soil.sensor.wide[, .SD , .SDcols = cols1]
    cols2 <- c(1, ec.n)
    ec.soil.temp.subset <- ec.soil.temp[, .SD, .SDcols = cols2]
    data <- merge(soil.sensor.subset,
                  ec.soil.temp.subset,
                  all = TRUE,
                  by = 'ts')
    data <- setnames(data, old = seq(1,3), new = c('ts', 'cip.soil', 'ec.soil'))
    
    if (!all(is.na(data$cip.soil)) & !all(is.na(data$ec.soil))) {
      model <- lm(cip.soil ~ ec.soil, data = data)
      model.coefs <- list(intercept = model$coefficients[1],
                          slope = model$coefficients[2],
                          r2 = summary(model)$r.squared)
      coefs <- rbind(coefs, model.coefs)
      
    }
  }  
  
  # save coefficients
  coefs <- coefs[r2 == max(r2)]
  coefs <- coefs[, sensor := colnames(soil.sensor.wide)[sensor.n]]
  final.coefs <- rbind(final.coefs, coefs)
  
  # estimate soil temperatures
  predictions[, ':=' (ts = data$ts,
                      sensor = rep(colnames(soil.sensor.wide)[sensor.n], nrow(data)),
                      prediction = coefs$intercept[1] + coefs$slope[1]*data$ec.soil)]
  final.predictions <- rbind(final.predictions, predictions)
  
}

final.predictions[,  c('depth', 'id') := tstrsplit(sensor, '.', fixed = TRUE)]
final.predictions[,  c('id', 'fence', 'plot') := tstrsplit(id, '_', fixed = TRUE)]
final.predictions[, ':=' (fence = as.numeric(fence),
                          plot = as.numeric(plot))]
final.predictions <- dcast(final.predictions,
              ts + fence + plot ~ depth,
              value.var = 'prediction')
final.predictions <- final.predictions[,
                                       ':=' (t5.pred = t5,
                                             t10.pred = t10,
                                             t20.pred = t20,
                                             t40.pred = t40)]
final.predictions <- final.predictions[,
                                       ':=' (t5 = NULL,
                                             t10 = NULL,
                                             t20 = NULL,
                                             t40 = NULL)]
soil.sensor <- merge(soil.sensor,
                     final.predictions,
                     all.x = TRUE,
                     by = c('ts', 'fence', 'plot'))

soil.sensor[, ':=' (t5.filled = fifelse(is.na(t5.filled),
                                        t5.pred,
                                        t5.filled),
                    t10.filled = fifelse(is.na(t10.filled),
                                        t10.pred,
                                        t10.filled),
                    t20.filled = fifelse(is.na(t20.filled),
                                        t20.pred,
                                        t20.filled),
                    t40.filled = fifelse(is.na(t40.filled),
                                        t40.pred,
                                        t40.filled))]

### This step filled about half of the remaining holes
View(soil.sensor[is.na(t10.filled), .N, by = .(year)])
View(soil.sensor[is.na(t10.filled), .N, by = .(year, plot.id)])

##Fill small datagaps using linear interpolation between observations.
soil.sensor[, 
            ':=' (t5.filled = na.approx(t5.filled, maxgap = 4),
                  t10.filled = na.approx(t10.filled, maxgap = 4),
                  t20.filled = na.approx(t20.filled, maxgap = 4),
                  t40.filled = na.approx(t40.filled, maxgap = 4)),
            by = .(fence, plot)]

### 
View(soil.sensor[is.na(t10.filled), .N, by = .(year)])
View(soil.sensor[is.na(t10.filled), .N, by = .(year, plot.id)])
View(soil.sensor[is.na(t10.filled) & year == 2018, .N, by = .(ts)])

soil.sensor[, date := parse_date_time(date, orders = c('Y!-m!-d!'))]

# Neaten
soil.sensor <- soil.sensor[, .(ts, date, year, month, week, doy, hour, hourmin,
                               treatment, fence, plot, plot.id, t5, t5.filled,
                               t10, t10.filled, t20, t20.filled, t40, t40.filled,
                               vwc, gwc)]
###########################################################################################

### Merge Half-Hourly Data ################################################################
### Merge with CO2 Data
### plot frame to join environmental data that doesn't have plot information with
plot.frame <- expand_grid(fence = seq(1, 6),
                          plot = seq(1, 8),
                          date = parse_date_time(seq(ymd('2008-09-01'),
                                                     ymd('2021-09-30'),
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
         flux.year = ifelse(month < 10,
                            year,
                            year + 1),
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
weather.f <- merge(weather.f, plot.frame, 
                   by = c('date', 'flux.year', 'hourmin'),
                   allow.cartesian = TRUE,
                   all = TRUE)
flux.hh <- merge(co2, weather.f,
              by = c('ts', 'year', 'month', 'week', 'doy', 'date', 'hour',
                     'hourmin', 'fence', 'plot', 'plot.id', 'treatment'),
              all = TRUE)
flux.hh[month %in% seq(5, 9) & is.na(precip),
     .N]/nrow(flux.hh[month %in% seq(5, 9)])
flux.hh[is.na(rh), .N]/nrow(flux.hh)
flux.hh[is.na(par), .N]/nrow(flux.hh)

### Soil sensors
flux.hh <- merge(flux.hh, 
              soil.sensor,
              by = c('ts', 'year', 'month', 'week', 'doy', 'date', 'hour',
                     'hourmin', 'fence', 'plot', 'plot.id', 'treatment'),
              all = TRUE)
flux.hh[is.na(t5), .N]/nrow(flux.hh)
flux.hh[is.na(t10), .N]/nrow(flux.hh)
flux.hh[is.na(t20), .N]/nrow(flux.hh)
flux.hh[is.na(t40), .N]/nrow(flux.hh)
flux.hh[is.na(vwc), .N]/nrow(flux.hh)
flux.hh[is.na(gwc), .N]/nrow(flux.hh)

flux.hh[, duplicated.data := .N > 1, by = .(ts, plot.id)]
View(flux.hh[duplicated.data == TRUE])
flux.hh[is.na(nee), .N, by = 'year']
###########################################################################################

### Gap Fill Chamber Temps ################################################################
### Clean up
# rm(alt.annual, biomass, chambt, co2, ndvi, plot.frame, soil.sensor, sub,
#    td, td.2009, weather, well.assignment, wtd, wtd.2009, snow.annual)
### Chamber Temps
# determine period when chambers are deployed
# this misses 2008 and gets a few wrong on the days that the chambers were deployed or removed
flux.hh[, deployed := fifelse(date >= date[first(which(!is.na(nee)))] & 
                             date <= date[last(which(!is.na(nee)))],
                           1,
                           0),
     by = c('year')]
# get 2008
flux.hh[year == 2008, deployed := 0]
# get the wrong values on the days that chambers were deployed or removed
flux.hh[, deployed := fifelse(date == date[first(which(!is.na(nee)))] &
                             is.na(precip) |
                             date == date[last(which(!is.na(nee)))] &
                             is.na(nee),
                           0,
                           deployed),
     by = c('year')]
flux.hh[is.na(deployed), .N, by = 'year']

# Remove really low chamber temps when air temp is much higher
flux.hh[(Tair-t.chamb) > 10, t.chamb := NA]
ggplot(flux.hh, aes(x = Tair, y = t.chamb, colour = year), alpha = 0.2) +
  geom_point() +
  facet_grid(.~treatment)

# model chambT using Tair on a plot by plot basis
model.t.chamb.lm <- function(df) {
  fit <- lm(t.chamb ~ Tair, data=df)
  return(list(t.chamb.intercept=coef(fit)[1], 
              t.chamb.slope=coef(fit)[2],
              t.chamb.r2 = summary(fit)$r.squared))
}

m.t.chamb <- flux.hh[, 
                  model.t.chamb.lm(.SD),
                  by=c('fence', 'plot')]
# m.chambT[plot %in% c(2, 4),
#          treatment := 'Control']
# m.chambT[plot %in% c(1, 3),
#          treatment := 'Air Warming']
# m.chambT[plot %in% c(6, 8),
#          treatment := 'Soil Warming']
# m.chambT[plot %in% c(5, 7),
#          treatment := 'Air + Soil Warming']
# m.chambT[,
#          mean(chambT.slope),
#          by = 'treatment']

# test out modeled chamber temps on all data
flux.hh <- merge(flux.hh, m.t.chamb, by = c('fence', 'plot'))
flux.hh[,
     t.chamb.m := t.chamb.intercept + t.chamb.slope*Tair]
# tchamb.m <- lm(t.chamb ~ Tair + treatment, data = flux.hh)
# summary(tchamb.m)
# 
# flux.hh[treatment == 'Control',
#      t.chamb.m := tchamb.m$coefficients[1] + Tair*tchamb.m$coefficients[2]]
# flux.hh[treatment == 'Air Warming',
#      t.chamb.m := tchamb.m$coefficients[1] + Tair*(tchamb.m$coefficients[2] + tchamb.m$coefficients[3])]
# flux.hh[treatment == 'Air + Soil Warming',
#      t.chamb.m := tchamb.m$coefficients[1] + Tair*(tchamb.m$coefficients[2] + tchamb.m$coefficients[4])]
# flux.hh[treatment == 'Soil Warming',
#      t.chamb.m := tchamb.m$coefficients[1] + Tair*(tchamb.m$coefficients[2] + tchamb.m$coefficients[5])]

# ggplot(flux.hh, aes(x = Tair, y = t.chamb.m, colour = year), alpha = 0.2) +
#   geom_point() +
#   facet_grid(.~treatment)

# create a t.chamb.filled column with hobo air temp when chambers aren't deployed and 
# gap filled chamber temps when they are deployed
flux.hh[!is.na(t.chamb), t.chamb.filled := t.chamb]
flux.hh[is.na(t.chamb) & deployed == 1, t.chamb.filled := t.chamb.m]
flux.hh[is.na(t.chamb) & deployed == 0, t.chamb.filled := Tair]
flux.hh[is.na(t.chamb.filled) & deployed == 1, .N, by = 'year']
flux.hh[is.na(t.chamb.filled), .N, by = 'year']
flux.hh[is.na(Tair), .N, by = 'year']
# ggplot(flux.hh, aes(x = date)) +
#   geom_point(aes(y = tair), color = 'red') +
#   geom_point(aes(y = Tair), color = 'black') +
#   facet_grid(fence ~ plot)

flux.hh[, ':=' (t.chamb.m = NULL)]
# ggplot(flux.hh, aes(x = tair, y = t.chamb, colour = year), alpha = 0.2) +
#   geom_point() +
#   facet_grid(.~treatment)
rm(deployed, m.t.chamb)


### Probably won't gap fill anything else
flux.hh[is.na(par), .N, by = 'year']
flux.hh[is.na(rh), .N, by = 'year'] # missing a lot
flux.hh[is.na(t5), .N, by = 'year']
flux.hh[is.na(t10), .N, by = 'year']
flux.hh[is.na(t20) & plot %in% c(1,2,5,6), .N, by = 'year']
flux.hh[is.na(t40) & plot %in% c(1,2,5,6), .N, by = 'year']
flux.hh[is.na(vwc) & plot %in% c(1,2,5,6), .N, by = 'year'] # missing a lot
flux.hh[is.na(gwc), .N, by = 'year'] # missing a lot

### Clean Up Environment and Memory
rm(chambt, co2, coefs, data, ec.2018, ec.ameriflux, ec.soil.temp, 
   ec.soil.temp.subset, eddy, final.coefs, final.predictions, model, 
   model.coefs, predictions, r2, soil.sensor, soil.sensor.subset,
   soil.sensor.wide, tair.model, times.frame, weather, cols1, cols2,
   ec.n, filenames, sensor.n)
gc()
###########################################################################################

### Create Summaries and Derived Variables ################################################
### Daily Summary
flux.hh[, ':=' (flux.year = fifelse(month >= 10,
                                    year + 1,
                                    year), # flux.year: GS + prior winter; Oct. 1, 2009 - Sept. 31, 2010 becomes 2010
                season = fifelse(month <= 4 | month >= 10,
                           0,
                           1),
                block = fifelse(fence <= 2,
                                'a',
                                fifelse(fence <= 4,
                                        'b',
                                        'c')))]
flux.hh <- flux.hh[, 
                   .(ts, date, year, flux.year, season, month, week, doy, hour, 
                     hourmin, block, fence, plot, plot.id, treatment, deployed, 
                     filled, nee, reco, gpp, r2, par, tair.hobo = Tair, t.chamb, 
                     t.chamb.filled, t5, t10, t20, t40, t5.filled, t10.filled, 
                     t20.filled, t40.filled, vwc, gwc, precip, rh)]
flux.hh <- flux.hh[order(ts, plot.id)]

# Check missing data
# 488448 is number of half hourly measurements in winter
flux.hh[is.na(nee), .N - 488448, by = .(flux.year)]
flux.hh[is.na(reco), .N - 488448, by = .(flux.year)]
flux.hh[is.na(gpp), .N - 488448, by = .(flux.year)]
flux.hh[is.na(par), .N, by = .(flux.year)]
flux.hh[is.na(tair.hobo), .N, by = .(flux.year)]
flux.hh[is.na(t5.filled), .N, by = .(flux.year)]
flux.hh[is.na(t10.filled), .N, by = .(flux.year)]
flux.hh[is.na(t20.filled), .N, by = .(flux.year)]
flux.hh[is.na(t40.filled), .N, by = .(flux.year)]
flux.hh[is.na(vwc), .N, by = .(flux.year)]
flux.hh[is.na(gwc), .N, by = .(flux.year)]
flux.hh[is.na(precip), .N, by = .(flux.year)]
flux.hh[is.na(rh), .N, by = .(flux.year)]


### Calculate daily values
flux.daily <- flux.hh[,
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
                        tair.max = max(t.chamb.filled, na.rm = TRUE),
                        tair.mean = mean(t.chamb.filled, na.rm = TRUE),
                        tair.min = min(t.chamb.filled, na.rm = TRUE),
                        tair.sd = fifelse(all(is.na(t.chamb.filled)),
                                          NaN,
                                          sd(t.chamb.filled, na.rm = TRUE)),
                        growing.days = mean(t.chamb.filled, na.rm = TRUE) - 5,
                        freezing.days = mean(t.chamb.filled, na.rm = TRUE)*-1,
                        par = fifelse(all(is.na(par)),
                                      NaN,
                                      sum(par, na.rm = TRUE)),
                        t5.min = min(t5, na.rm = TRUE),
                        t5.mean = mean(t5, na.rm = TRUE),
                        t5.max = max(t5, na.rm = TRUE),
                        t5.sd = fifelse(all(is.na(t5)),
                                        NaN,
                                        sd(t5, na.rm = TRUE)),
                        t5.filled.min = min(t5.filled, na.rm = TRUE),
                        t5.filled.mean = mean(t5.filled, na.rm = TRUE),
                        t5.filled.max = max(t5.filled, na.rm = TRUE),
                        t5.filled.sd = fifelse(all(is.na(t5.filled)),
                                               NaN,
                                               sd(t5.filled, na.rm = TRUE)),
                        t10.min = min(t10, na.rm = TRUE),
                        t10.mean = mean(t10, na.rm = TRUE),
                        t10.max = max(t10, na.rm = TRUE),
                        t10.sd = fifelse(all(is.na(t10)),
                                         NaN,
                                         sd(t10, na.rm = TRUE)),
                        t10.filled.min = min(t10.filled, na.rm = TRUE),
                        t10.filled.mean = mean(t10.filled, na.rm = TRUE),
                        t10.filled.max = max(t10.filled, na.rm = TRUE),
                        t10.filled.sd = fifelse(all(is.na(t10.filled)),
                                                NaN,
                                                sd(t10.filled, na.rm = TRUE)),
                        t20.min = min(t20, na.rm = TRUE),
                        t20.mean = mean(t20, na.rm = TRUE),
                        t20.max = max(t20, na.rm = TRUE),
                        t20.sd = fifelse(all(is.na(t20)),
                                         NaN,
                                         sd(t20, na.rm = TRUE)),
                        t20.filled.min = min(t20.filled, na.rm = TRUE),
                        t20.filled.mean = mean(t20.filled, na.rm = TRUE),
                        t20.filled.max = max(t20.filled, na.rm = TRUE),
                        t20.filled.sd = fifelse(all(is.na(t20.filled)),
                                                NaN,
                                                sd(t20.filled, na.rm = TRUE)),
                        t40.min = min(t40, na.rm = TRUE),
                        t40.mean = mean(t40, na.rm = TRUE),
                        t40.max = max(t40, na.rm = TRUE),
                        t40.sd = fifelse(all(is.na(t40)),
                                         NaN,
                                         sd(t40, na.rm = TRUE)),
                        t40.filled.min = min(t40.filled, na.rm = TRUE),
                        t40.filled.mean = mean(t40.filled, na.rm = TRUE),
                        t40.filled.max = max(t40.filled, na.rm = TRUE),
                        t40.filled.sd = fifelse(all(is.na(t40.filled)),
                                                NaN,
                                                sd(t40.filled, na.rm = TRUE)),
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
                        precip = fifelse(all(is.na(precip)),
                                         NaN,
                                         sum(precip, na.rm = TRUE)),
                        rh.max = max(rh, na.rm = TRUE),
                        rh.mean = mean(rh, na.rm = TRUE),
                        rh.min = min(rh, na.rm = TRUE),
                        rh.sd = fifelse(all(is.na(rh)),
                                        NaN,
                                        sd(rh, na.rm = TRUE))),
                      by = c('date', 'year', 'flux.year', 'season', 'month', 'week',
                             'doy', 'block', 'fence', 'plot', 'plot.id',
                             'treatment')]
# remove -Inf values
flux.daily <- flux.daily[, lapply(.SD, function(x) replace(x, list = is.infinite(x), values = NA))]
flux.daily <- flux.daily[, lapply(.SD, function(x) replace(x, list = is.nan(x), values = NA))]

# make sure that NAs introduced by no precip are 0 instead
flux.daily[is.na(precip),
           precip := 0]
# adjust growing and freezing day values to use limits
flux.daily[growing.days < 0,
           growing.days := 0]
flux.daily[freezing.days < 0,
           freezing.days := 0]
# calculate GDD and FDD from growing and freezing daily values
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

# # check that everything worked as expected
# Tair and derivates
ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = tair.mean, color = 'Tair Mean')) +
  geom_line(aes(y = tair.min, color = 'Tair Min')) +
  geom_line(aes(y = tair.max, color = 'Tair Max')) +
  geom_line(aes(y = growing.days, color = 'Growing Days')) +
  facet_grid(fence ~ plot)

ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = gdd, color = 'GDD')) +
  geom_line(aes(y = fdd, color = 'FDD')) +
  facet_grid(fence ~ plot)

# t5 and derivatives
ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = t5.mean, color = 't5 Mean'), alpha = 0.5) +
  geom_line(aes(y = t5.min, color = 't5 Min'), alpha = 0.5) +
  geom_line(aes(y = t5.max, color = 't5 Max'), alpha = 0.5) +
  facet_grid(fence ~ plot)

ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = t5.sd)) +
  facet_grid(fence ~ plot)

# t10 and derivatives
ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = t10.mean, color = 't10 Mean'), alpha = 0.5) +
  geom_line(aes(y = t10.min, color = 't10 Min'), alpha = 0.5) +
  geom_line(aes(y = t10.max, color = 't10 Max'), alpha = 0.5) +
  facet_grid(fence ~ plot)

ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = t10.sd)) +
  facet_grid(fence ~ plot)

# t20 and derivatives
ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = t20.mean, color = 't20 Mean'), alpha = 0.5) +
  geom_line(aes(y = t20.min, color = 't20 Min'), alpha = 0.5) +
  geom_line(aes(y = t20.max, color = 't20 Max'), alpha = 0.5) +
  facet_grid(fence ~ plot)

ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = t20.sd)) +
  facet_grid(fence ~ plot)

# t40 and derivatives
ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = t40.mean, color = 't40 Mean'), alpha = 0.5) +
  geom_line(aes(y = t40.min, color = 't40 Min'), alpha = 0.5) +
  geom_line(aes(y = t40.max, color = 't40 Max'), alpha = 0.5) +
  facet_grid(fence ~ plot)

ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = t40.sd)) +
  facet_grid(fence ~ plot)

# vwc and derivatives
ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = vwc.mean, color = 'VWC Mean'), alpha = 0.5) +
  geom_line(aes(y = vwc.min, color = 'VWC Min'), alpha = 0.5) +
  geom_line(aes(y = vwc.max, color = 'VWC Max'), alpha = 0.5) +
  facet_grid(fence ~ plot)

ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = vwc.sd)) +
  facet_grid(fence ~ plot)

# gwc and derivatives
ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = gwc.mean, color = 'GWC Mean'), alpha = 0.5) +
  geom_line(aes(y = gwc.min, color = 'GWC Min'), alpha = 0.5) +
  geom_line(aes(y = gwc.max, color = 'GWC Max'), alpha = 0.5) +
  facet_grid(fence ~ plot)

ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = gwc.sd)) +
  facet_grid(fence ~ plot)

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


### Calculate monthly values
flux.monthly <- flux.hh[,
                      .(nee.sum = fifelse(all(is.na(nee)),
                                          NaN,
                                          sum(nee, na.rm = TRUE)),
                        reco.sum = fifelse(all(is.na(reco)),
                                           NaN,
                                           sum(reco, na.rm = TRUE)),
                        gpp.sum = fifelse(all(is.na(gpp)),
                                          NaN,
                                          sum(gpp, na.rm = TRUE)),
                        tair.max = max(t.chamb.filled, na.rm = TRUE),
                        tair.mean = mean(t.chamb.filled, na.rm = TRUE),
                        tair.min = min(t.chamb.filled, na.rm = TRUE),
                        tair.sd = fifelse(all(is.na(t.chamb.filled)),
                                          NaN,
                                          sd(t.chamb.filled, na.rm = TRUE)),
                        par = fifelse(all(is.na(par)),
                                      NaN,
                                      sum(par, na.rm = TRUE)),
                        t5.min = min(t5, na.rm = TRUE),
                        t5.mean = mean(t5, na.rm = TRUE),
                        t5.max = max(t5, na.rm = TRUE),
                        t5.sd = fifelse(all(is.na(t5)),
                                        NaN,
                                        sd(t5, na.rm = TRUE)),
                        t5.filled.min = min(t5.filled, na.rm = TRUE),
                        t5.filled.mean = mean(t5.filled, na.rm = TRUE),
                        t5.filled.max = max(t5.filled, na.rm = TRUE),
                        t5.filled.sd = fifelse(all(is.na(t5.filled)),
                                               NaN,
                                               sd(t5.filled, na.rm = TRUE)),
                        t10.min = min(t10, na.rm = TRUE),
                        t10.mean = mean(t10, na.rm = TRUE),
                        t10.max = max(t10, na.rm = TRUE),
                        t10.sd = fifelse(all(is.na(t10)),
                                         NaN,
                                         sd(t10, na.rm = TRUE)),
                        t10.filled.min = min(t10.filled, na.rm = TRUE),
                        t10.filled.mean = mean(t10.filled, na.rm = TRUE),
                        t10.filled.max = max(t10.filled, na.rm = TRUE),
                        t10.filled.sd = fifelse(all(is.na(t10.filled)),
                                                NaN,
                                                sd(t10.filled, na.rm = TRUE)),
                        t20.min = min(t20, na.rm = TRUE),
                        t20.mean = mean(t20, na.rm = TRUE),
                        t20.max = max(t20, na.rm = TRUE),
                        t20.sd = fifelse(all(is.na(t20)),
                                         NaN,
                                         sd(t20, na.rm = TRUE)),
                        t20.filled.min = min(t20.filled, na.rm = TRUE),
                        t20.filled.mean = mean(t20.filled, na.rm = TRUE),
                        t20.filled.max = max(t20.filled, na.rm = TRUE),
                        t20.filled.sd = fifelse(all(is.na(t20.filled)),
                                                NaN,
                                                sd(t20.filled, na.rm = TRUE)),
                        t40.min = min(t40, na.rm = TRUE),
                        t40.mean = mean(t40, na.rm = TRUE),
                        t40.max = max(t40, na.rm = TRUE),
                        t40.sd = fifelse(all(is.na(t40)),
                                         NaN,
                                         sd(t40, na.rm = TRUE)),
                        t40.filled.min = min(t40.filled, na.rm = TRUE),
                        t40.filled.mean = mean(t40.filled, na.rm = TRUE),
                        t40.filled.max = max(t40.filled, na.rm = TRUE),
                        t40.filled.sd = fifelse(all(is.na(t40.filled)),
                                                NaN,
                                                sd(t40.filled, na.rm = TRUE)),
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
                        precip = fifelse(all(is.na(precip)),
                                         NaN,
                                         sum(precip, na.rm = TRUE)),
                        rh.max = max(rh, na.rm = TRUE),
                        rh.mean = mean(rh, na.rm = TRUE),
                        rh.min = min(rh, na.rm = TRUE),
                        rh.sd = fifelse(all(is.na(rh)),
                                        NaN,
                                        sd(rh, na.rm = TRUE))),
                      by = c('year', 'flux.year', 'season', 'month', 'block', 
                             'fence', 'plot', 'plot.id', 'treatment')]
# remove -Inf values
flux.monthly <- flux.monthly[, lapply(.SD, function(x) replace(x, list = is.infinite(x), values = NA))]
flux.monthly <- flux.monthly[, lapply(.SD, function(x) replace(x, list = is.nan(x), values = NA))]

# Add GDD and FDD
gdd.fdd.monthly <- flux.daily[
  , 
  .(year, flux.year, season, month, block, fence, plot, plot.id, treatment, 
    gdd, fdd, growing.days, freezing.days, precip.cum)
  ][
    ,
    .(monthly.gdd = sum(growing.days),
      monthly.fdd = sum(growing.days),
      gdd = max(gdd, na.rm = TRUE),
      fdd = max(fdd, na.rm = TRUE),
      precip.cum = max(precip.cum, na.rm = TRUE)),
    by = .(year, flux.year, season, month, block, fence, plot, plot.id, 
           treatment)
  ]

flux.monthly <- merge(flux.monthly, gdd.fdd.monthly,
                      by = c('year', 'flux.year', 'season', 'month', 'block', 
                             'fence', 'plot', 'plot.id', 'treatment'))
rm(gdd.fdd.monthly)

# lagged variables
flux.monthly[,
             ':=' (gdd.2m = frollsum(gdd, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                   gdd.3m = frollsum(gdd, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week 
                   fdd.2m = frollsum(fdd, n = 2, align = 'right', na.rm = TRUE), # previous week + current week 
                   fdd.3m = frollsum(fdd, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week 
                   precip.2m = frollsum(precip, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                   precip.3m = frollsum(precip, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                   vwc.mean.2m = frollmean(vwc.mean, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                   vwc.mean.3m = frollmean(vwc.mean, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                   vwc.max.2m = frollapply(vwc.max, n = 2, align = 'right', na.rm = TRUE, FUN = max), # previous week + current week
                   vwc.max.3m = frollapply(vwc.max, n = 3, align = 'right', na.rm = TRUE, FUN = max), # 2 previous weeks + current week
                   vwc.min.2m = frollapply(vwc.min, n = 2, align = 'right', na.rm = TRUE, FUN = min), # previous week + current week
                   vwc.min.3m = frollapply(vwc.min, n = 3, align = 'right', na.rm = TRUE, FUN = min), # 2 previous weeks + current week
                   gwc.mean.2m = frollmean(gwc.mean, n = 2, align = 'right', na.rm = TRUE), # previous week + current week
                   gwc.mean.3m = frollmean(gwc.mean, n = 3, align = 'right', na.rm = TRUE), # 2 previous weeks + current week
                   gwc.max.2m = frollapply(gwc.max, n = 2, align = 'right', na.rm = TRUE, FUN = max), # previous week + current week
                   gwc.max.3m = frollapply(gwc.max, n = 3, align = 'right', na.rm = TRUE, FUN = max), # 2 previous weeks + current week
                   gwc.min.2m = frollapply(gwc.min, n = 2, align = 'right', na.rm = TRUE, FUN = min), # previous week + current week
                   gwc.min.3m = frollapply(gwc.min, n = 3, align = 'right', na.rm = TRUE, FUN = min)), # 2 previous weeks + current week
             by = c('fence', 'plot')]

# remove NaN, -Inf, and Inf introduced by calculations
flux.monthly <- flux.monthly[, lapply(.SD, function(x) replace(x, list = is.infinite(x), values = NA))]
flux.monthly <- flux.monthly[, lapply(.SD, function(x) replace(x, list = is.nan(x), values = NA))]

# # check that everything worked as expected
# Tair and derivates
ggplot(flux.monthly, aes(x = year + month/12)) +
  geom_line(aes(y = tair.mean, color = 'Tair Mean')) +
  geom_line(aes(y = tair.min, color = 'Tair Min')) +
  geom_line(aes(y = tair.max, color = 'Tair Max')) +
  facet_grid(fence ~ plot)

ggplot(flux.monthly, aes(x = year + month/12)) +
  geom_line(aes(y = gdd, color = 'GDD')) +
  geom_line(aes(y = fdd, color = 'FDD')) +
  facet_grid(fence ~ plot)

# t5 and derivatives
ggplot(flux.monthly, aes(x = year + month/12)) +
  geom_line(aes(y = t5.mean, color = 't5 Mean'), alpha = 0.5) +
  geom_line(aes(y = t5.min, color = 't5 Min'), alpha = 0.5) +
  geom_line(aes(y = t5.max, color = 't5 Max'), alpha = 0.5) +
  facet_grid(fence ~ plot)

ggplot(flux.monthly, aes(x = year + month/12)) +
  geom_line(aes(y = t5.sd)) +
  facet_grid(fence ~ plot)

# t10 and derivatives
ggplot(flux.monthly, aes(x = year + month/12)) +
  geom_line(aes(y = t10.mean, color = 't10 Mean'), alpha = 0.5) +
  geom_line(aes(y = t10.min, color = 't10 Min'), alpha = 0.5) +
  geom_line(aes(y = t10.max, color = 't10 Max'), alpha = 0.5) +
  facet_grid(fence ~ plot)

ggplot(flux.monthly, aes(x = year + month/12)) +
  geom_line(aes(y = t10.sd)) +
  facet_grid(fence ~ plot)

# t20 and derivatives
ggplot(flux.monthly, aes(x = year + month/12)) +
  geom_line(aes(y = t20.mean, color = 't20 Mean'), alpha = 0.5) +
  geom_line(aes(y = t20.min, color = 't20 Min'), alpha = 0.5) +
  geom_line(aes(y = t20.max, color = 't20 Max'), alpha = 0.5) +
  facet_grid(fence ~ plot)

ggplot(flux.monthly, aes(x = year + month/12)) +
  geom_line(aes(y = t20.sd)) +
  facet_grid(fence ~ plot)

# t40 and derivatives
ggplot(flux.monthly, aes(x = year + month/12)) +
  geom_line(aes(y = t40.mean, color = 't40 Mean'), alpha = 0.5) +
  geom_line(aes(y = t40.min, color = 't40 Min'), alpha = 0.5) +
  geom_line(aes(y = t40.max, color = 't40 Max'), alpha = 0.5) +
  facet_grid(fence ~ plot)

ggplot(flux.monthly, aes(x = year + month/12)) +
  geom_line(aes(y = t40.sd)) +
  facet_grid(fence ~ plot)

# vwc and derivatives
ggplot(flux.monthly, aes(x = year + month/12)) +
  geom_line(aes(y = vwc.mean, color = 'VWC Mean'), alpha = 0.5) +
  geom_line(aes(y = vwc.min, color = 'VWC Min'), alpha = 0.5) +
  geom_line(aes(y = vwc.max, color = 'VWC Max'), alpha = 0.5) +
  facet_grid(fence ~ plot)

ggplot(flux.monthly, aes(x = year + month/12)) +
  geom_line(aes(y = vwc.sd)) +
  facet_grid(fence ~ plot)

# gwc and derivatives
ggplot(flux.monthly, aes(x = year + month/12)) +
  geom_line(aes(y = gwc.mean, color = 'GWC Mean'), alpha = 0.5) +
  geom_line(aes(y = gwc.min, color = 'GWC Min'), alpha = 0.5) +
  geom_line(aes(y = gwc.max, color = 'GWC Max'), alpha = 0.5) +
  facet_grid(fence ~ plot)

ggplot(flux.monthly, aes(x = year + month/12)) +
  geom_line(aes(y = gwc.sd)) +
  facet_grid(fence ~ plot)

### Calculate annual values
flux.seasonal <- flux.hh[,
                       .(nee.sum = fifelse(all(is.na(nee)),
                                           NaN,
                                           sum(nee, na.rm = TRUE)),
                         reco.sum = fifelse(all(is.na(reco)),
                                            NaN,
                                            sum(reco, na.rm = TRUE)),
                         gpp.sum = fifelse(all(is.na(gpp)),
                                           NaN,
                                           sum(gpp, na.rm = TRUE)),
                         tair.max = max(t.chamb.filled, na.rm = TRUE),
                         tair.mean = mean(t.chamb.filled, na.rm = TRUE),
                         tair.min = min(t.chamb.filled, na.rm = TRUE),
                         tair.sd = fifelse(all(is.na(t.chamb.filled)),
                                           NaN,
                                           sd(t.chamb.filled, na.rm = TRUE)),
                         par = fifelse(all(is.na(par)),
                                       NaN,
                                       sum(par, na.rm = TRUE)),
                         t5.min = min(t5, na.rm = TRUE),
                         t5.mean = mean(t5, na.rm = TRUE),
                         t5.max = max(t5, na.rm = TRUE),
                         t5.sd = fifelse(all(is.na(t5)),
                                         NaN,
                                         sd(t5, na.rm = TRUE)),
                         t5.filled.min = min(t5.filled, na.rm = TRUE),
                         t5.filled.mean = mean(t5.filled, na.rm = TRUE),
                         t5.filled.max = max(t5.filled, na.rm = TRUE),
                         t5.filled.sd = fifelse(all(is.na(t5.filled)),
                                                NaN,
                                                sd(t5.filled, na.rm = TRUE)),
                         t10.min = min(t10, na.rm = TRUE),
                         t10.mean = mean(t10, na.rm = TRUE),
                         t10.max = max(t10, na.rm = TRUE),
                         t10.sd = fifelse(all(is.na(t10)),
                                          NaN,
                                          sd(t10, na.rm = TRUE)),
                         t10.filled.min = min(t10.filled, na.rm = TRUE),
                         t10.filled.mean = mean(t10.filled, na.rm = TRUE),
                         t10.filled.max = max(t10.filled, na.rm = TRUE),
                         t10.filled.sd = fifelse(all(is.na(t10.filled)),
                                                 NaN,
                                                 sd(t10.filled, na.rm = TRUE)),
                         t20.min = min(t20, na.rm = TRUE),
                         t20.mean = mean(t20, na.rm = TRUE),
                         t20.max = max(t20, na.rm = TRUE),
                         t20.sd = fifelse(all(is.na(t20)),
                                          NaN,
                                          sd(t20, na.rm = TRUE)),
                         t20.filled.min = min(t20.filled, na.rm = TRUE),
                         t20.filled.mean = mean(t20.filled, na.rm = TRUE),
                         t20.filled.max = max(t20.filled, na.rm = TRUE),
                         t20.filled.sd = fifelse(all(is.na(t20.filled)),
                                                 NaN,
                                                 sd(t20.filled, na.rm = TRUE)),
                         t40.min = min(t40, na.rm = TRUE),
                         t40.mean = mean(t40, na.rm = TRUE),
                         t40.max = max(t40, na.rm = TRUE),
                         t40.sd = fifelse(all(is.na(t40)),
                                          NaN,
                                          sd(t40, na.rm = TRUE)),
                         t40.filled.min = min(t40.filled, na.rm = TRUE),
                         t40.filled.mean = mean(t40.filled, na.rm = TRUE),
                         t40.filled.max = max(t40.filled, na.rm = TRUE),
                         t40.filled.sd = fifelse(all(is.na(t40.filled)),
                                                 NaN,
                                                 sd(t40.filled, na.rm = TRUE)),
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
                         precip = fifelse(all(is.na(precip)),
                                          NaN,
                                          sum(precip, na.rm = TRUE)),
                         rh.max = max(rh, na.rm = TRUE),
                         rh.mean = mean(rh, na.rm = TRUE),
                         rh.min = min(rh, na.rm = TRUE),
                         rh.sd = fifelse(all(is.na(rh)),
                                         NaN,
                                         sd(rh, na.rm = TRUE))),
                       by = c('flux.year', 'season', 'block', 'fence', 'plot', 
                              'plot.id', 'treatment')]
# remove -Inf values
flux.seasonal <- flux.seasonal[, lapply(.SD, function(x) replace(x, list = is.infinite(x), values = NA))]
flux.seasonal <- flux.seasonal[, lapply(.SD, function(x) replace(x, list = is.nan(x), values = NA))]

# Add GDD and FDD
gdd.fdd.annual <- flux.daily[
  , 
  .(year, flux.year, season, month, block, fence, plot, plot.id, treatment, 
    gdd, fdd, growing.days, freezing.days)
][
  ,
  .(seasonal.gdd = sum(growing.days),
    seasonal.fdd = sum(freezing.days),
    gdd = max(gdd, na.rm = TRUE),
    fdd = max(fdd, na.rm = TRUE)),
  by = .(flux.year, block, fence, plot, plot.id, 
         treatment)
]

flux.seasonal <- merge(flux.seasonal, gdd.fdd.annual,
                     by = c('flux.year', 'block', 'fence', 'plot', 'plot.id', 
                            'treatment'))
rm(gdd.fdd.annual)


### Winter environmental conditions
env.winter <- flux.hh[season == 0,
                      .(winter.tair.min = min(tair.hobo, na.rm = TRUE),
                        winter.tair.mean = mean(tair.hobo, na.rm = TRUE),
                        winter.tair.sd = fifelse(all(is.na(tair.hobo)),
                                                 NaN,
                                                 sd(tair.hobo, na.rm = TRUE)),
                        winter.t5.min = min(t5, na.rm = TRUE),
                        winter.t5.mean = mean(t5, na.rm = TRUE),
                        winter.t5.sd = fifelse(all(is.na(t5)),
                                               NaN,
                                               sd(t5, na.rm = TRUE)),
                        winter.t10.min = min(t10, na.rm = TRUE),
                        winter.t10.mean = mean(t10, na.rm = TRUE),
                        winter.t10.sd = fifelse(all(is.na(t10)),
                                                NaN,
                                                sd(t10, na.rm = TRUE)),
                        winter.t20.min = min(t20, na.rm = TRUE),
                        winter.t20.mean = mean(t20, na.rm = TRUE),
                        winter.t20.sd = fifelse(all(is.na(t20)),
                                                NaN,
                                                sd(t20, na.rm = TRUE)),
                        winter.t40.min = min(t40, na.rm = TRUE),
                        winter.t40.mean = mean(t40, na.rm = TRUE),
                        winter.t40.sd = fifelse(all(is.na(t40)),
                                                NaN,
                                                sd(t40, na.rm = TRUE))),
                      by = c('flux.year', 'block', 'fence', 'plot',
                             'plot.id', 'treatment')]
# remove NaN, -Inf, and Inf introduced by calculations
env.winter <- env.winter[, lapply(.SD, function(x) replace(x, list = is.infinite(x), values = NA))]
env.winter <- env.winter[, lapply(.SD, function(x) replace(x, list = is.nan(x), values = NA))]

env.winter[is.na(winter.t20.mean),
           winter.t20.sd := NA]
env.winter[is.na(winter.t40.mean),
           winter.t40.sd := NA]

# Add GDD and FDD
fdd.winter <- flux.daily[
  season == 0, 
  .(flux.year, season, month, block, fence, plot, plot.id, treatment, 
    fdd)
][
  ,
  .(winter.fdd = max(fdd, na.rm = TRUE)),
  by = .(flux.year, block, fence, plot, plot.id, 
         treatment)
]

env.winter <- merge(env.winter, fdd.winter,
                      by = c('flux.year', 'block', 
                             'fence', 'plot', 'plot.id', 'treatment'))
rm(fdd.winter)


# merge with winter data
flux.seasonal <- merge(flux.seasonal,
                       env.winter,
                       by = c('flux.year', 'block', 'fence', 'plot', 'plot.id', 'treatment'))


### Calculate annual values
flux.annual <- flux.hh[season == 1,
                      .(nee.sum = fifelse(all(is.na(nee)),
                                          NaN,
                                          sum(nee, na.rm = TRUE)),
                        reco.sum = fifelse(all(is.na(reco)),
                                           NaN,
                                           sum(reco, na.rm = TRUE)),
                        gpp.sum = fifelse(all(is.na(gpp)),
                                          NaN,
                                          sum(gpp, na.rm = TRUE)),
                        tair.max = max(t.chamb.filled, na.rm = TRUE),
                        tair.mean = mean(t.chamb.filled, na.rm = TRUE),
                        tair.min = min(t.chamb.filled, na.rm = TRUE),
                        tair.sd = fifelse(all(is.na(t.chamb.filled)),
                                          NaN,
                                          sd(t.chamb.filled, na.rm = TRUE)),
                        par = fifelse(all(is.na(par)),
                                      NaN,
                                      sum(par, na.rm = TRUE)),
                        t5.min = min(t5, na.rm = TRUE),
                        t5.mean = mean(t5, na.rm = TRUE),
                        t5.max = max(t5, na.rm = TRUE),
                        t5.sd = fifelse(all(is.na(t5)),
                                        NaN,
                                        sd(t5, na.rm = TRUE)),
                        t5.filled.min = min(t5.filled, na.rm = TRUE),
                        t5.filled.mean = mean(t5.filled, na.rm = TRUE),
                        t5.filled.max = max(t5.filled, na.rm = TRUE),
                        t5.filled.sd = fifelse(all(is.na(t5.filled)),
                                               NaN,
                                               sd(t5.filled, na.rm = TRUE)),
                        t10.min = min(t10, na.rm = TRUE),
                        t10.mean = mean(t10, na.rm = TRUE),
                        t10.max = max(t10, na.rm = TRUE),
                        t10.sd = fifelse(all(is.na(t10)),
                                         NaN,
                                         sd(t10, na.rm = TRUE)),
                        t10.filled.min = min(t10.filled, na.rm = TRUE),
                        t10.filled.mean = mean(t10.filled, na.rm = TRUE),
                        t10.filled.max = max(t10.filled, na.rm = TRUE),
                        t10.filled.sd = fifelse(all(is.na(t10.filled)),
                                                NaN,
                                                sd(t10.filled, na.rm = TRUE)),
                        t20.min = min(t20, na.rm = TRUE),
                        t20.mean = mean(t20, na.rm = TRUE),
                        t20.max = max(t20, na.rm = TRUE),
                        t20.sd = fifelse(all(is.na(t20)),
                                         NaN,
                                         sd(t20, na.rm = TRUE)),
                        t20.filled.min = min(t20.filled, na.rm = TRUE),
                        t20.filled.mean = mean(t20.filled, na.rm = TRUE),
                        t20.filled.max = max(t20.filled, na.rm = TRUE),
                        t20.filled.sd = fifelse(all(is.na(t20.filled)),
                                                NaN,
                                                sd(t20.filled, na.rm = TRUE)),
                        t40.min = min(t40, na.rm = TRUE),
                        t40.mean = mean(t40, na.rm = TRUE),
                        t40.max = max(t40, na.rm = TRUE),
                        t40.sd = fifelse(all(is.na(t40)),
                                         NaN,
                                         sd(t40, na.rm = TRUE)),
                        t40.filled.min = min(t40.filled, na.rm = TRUE),
                        t40.filled.mean = mean(t40.filled, na.rm = TRUE),
                        t40.filled.max = max(t40.filled, na.rm = TRUE),
                        t40.filled.sd = fifelse(all(is.na(t40.filled)),
                                                NaN,
                                                sd(t40.filled, na.rm = TRUE)),
                        vwc.min = min(vwc, na.rm = TRUE),
                        vwc.mean = mean(vwc, na.rm = TRUE),
                        vwc.max = max(vwc, na.rm = TRUE),
                        vwc.sd = fifelse(all(is.na(vwc)),
                                         NaN,
                                         sd(vwc, na.rm = TRUE)),
                        gwc.min = min(gwc, na.rm = TRUE),
                        gwc.mean = mean(gwc, na.rm = TRUE),
                        gwc.max = max(gwc, na.rm = TRUE),
                        gwc.sd = fifelse(all(is.na(gwc)),
                                         NaN,
                                         sd(gwc, na.rm = TRUE)),
                        precip = fifelse(all(is.na(precip)),
                                         NaN,
                                         sum(precip, na.rm = TRUE)),
                        rh.min = min(rh, na.rm = TRUE),
                        rh.mean = mean(rh, na.rm = TRUE),
                        rh.max = max(rh, na.rm = TRUE),
                        rh.sd = fifelse(all(is.na(rh)),
                                        NaN,
                                        sd(rh, na.rm = TRUE))),
                      by = c('flux.year', 'block', 'fence', 'plot', 'plot.id',
                             'treatment')]
# remove -Inf values
flux.annual <- flux.annual[, lapply(.SD, function(x) replace(x, list = is.infinite(x), values = NA))]
flux.annual <- flux.annual[, lapply(.SD, function(x) replace(x, list = is.nan(x), values = NA))]

# Add GDD and FDD
gdd.fdd.annual <- flux.daily[
  , 
  .(year, flux.year, season, month, block, fence, plot, plot.id, treatment, 
    gdd, fdd, precip.cum)
][
  ,
  .(gdd = max(gdd, na.rm = TRUE),
    fdd = max(fdd, na.rm = TRUE)),
  by = .(flux.year, block, fence, plot, plot.id, 
         treatment)
]

flux.annual <- merge(flux.annual, gdd.fdd.annual,
                      by = c('flux.year', 'block', 'fence', 'plot', 'plot.id', 
                             'treatment'))
rm(gdd.fdd.annual)
# # check that everything worked as expected
# Tair and derivates
ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = tair.mean, color = 'Tair Mean')) +
  geom_line(aes(y = tair.min, color = 'Tair Min')) +
  geom_line(aes(y = tair.max, color = 'Tair Max')) +
  facet_grid(fence ~ plot)

ggplot(flux.annual[plot.id == '1_1'], aes(x = flux.year)) +
  geom_line(aes(y = gdd, color = 'GDD')) +
  geom_line(aes(y = fdd, color = 'FDD'))

# t5 and derivatives
ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = t5.mean, color = 't5 Mean'), alpha = 0.5) +
  geom_line(aes(y = t5.min, color = 't5 Min'), alpha = 0.5) +
  geom_line(aes(y = t5.max, color = 't5 Max'), alpha = 0.5) +
  facet_grid(fence ~ plot)

ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = t5.sd)) +
  facet_grid(fence ~ plot)

# t10 and derivatives
ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = t10.mean, color = 't10 Mean'), alpha = 0.5) +
  geom_line(aes(y = t10.min, color = 't10 Min'), alpha = 0.5) +
  geom_line(aes(y = t10.max, color = 't10 Max'), alpha = 0.5) +
  facet_grid(fence ~ plot)

ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = t10.sd)) +
  facet_grid(fence ~ plot)

# t20 and derivatives
ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = t20.mean, color = 't20 Mean'), alpha = 0.5) +
  geom_line(aes(y = t20.min, color = 't20 Min'), alpha = 0.5) +
  geom_line(aes(y = t20.max, color = 't20 Max'), alpha = 0.5) +
  facet_grid(fence ~ plot)

ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = t20.sd)) +
  facet_grid(fence ~ plot)

# t40 and derivatives
ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = t40.mean, color = 't40 Mean'), alpha = 0.5) +
  geom_line(aes(y = t40.min, color = 't40 Min'), alpha = 0.5) +
  geom_line(aes(y = t40.max, color = 't40 Max'), alpha = 0.5) +
  facet_grid(fence ~ plot)

ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = t40.sd)) +
  facet_grid(fence ~ plot)

# vwc and derivatives
ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = vwc.mean, color = 'VWC Mean'), alpha = 0.5) +
  geom_line(aes(y = vwc.min, color = 'VWC Min'), alpha = 0.5) +
  geom_line(aes(y = vwc.max, color = 'VWC Max'), alpha = 0.5) +
  facet_grid(fence ~ plot)

ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = vwc.sd)) +
  facet_grid(fence ~ plot)

# gwc and derivatives
ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = gwc.mean, color = 'GWC Mean'), alpha = 0.5) +
  geom_line(aes(y = gwc.min, color = 'GWC Min'), alpha = 0.5) +
  geom_line(aes(y = gwc.max, color = 'GWC Max'), alpha = 0.5) +
  facet_grid(fence ~ plot)

ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = gwc.sd)) +
  facet_grid(fence ~ plot)
###########################################################################################


### Sub-Weekly Data
### WTD Data ##############################################################################
### Load WTD
wtd <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/wtd/WTD_2021_compiled.csv")

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
well.assignment <- expand.grid(year = seq(2009, 2021),
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
well.assignment[year > 2013 & fence == 1 & plot == 3, well := 2.5]
well.assignment[year > 2013 & fence == 1 & plot == 5, well := 4.5]
well.assignment[year > 2013 & fence == 2 & plot == 3, well := 2.5]
well.assignment[year > 2013 & fence == 2 & plot == 5, well := 4.5]
well.assignment[year > 2013 & fence == 3 & plot == 2, well := 2.5]
well.assignment[year > 2013 & fence == 3 & plot == 7, well := 4.5]
well.assignment[year > 2013 & fence == 4 & plot == 3, well := 2.5]
well.assignment[year > 2013 & fence == 4 & plot == 8, well := 4.5]
well.assignment[year > 2013 & fence == 5 & plot == 4, well := 2.5]
well.assignment[year > 2013 & fence == 5 & plot == 6, well := 4.5]
well.assignment[year > 2013 & fence == 6 & plot == 2, well := 2.5]
well.assignment[year > 2013 & fence == 6 & plot == 7, well := 4.5]
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
wtd.f <- wtd.f[, .(flux.year = year, date, month, fence,  WTD_Date, treatment, plot.id, wtd = WTD)]
wtd.f <- wtd.f[order(date, plot.id)]

# for environmental data
wtd.env <- wtd[, ':=' (treatment = fifelse(well %in% c(1, 2, 2.5, 1.17, 2.17, 3.17, 4.17),
                                      'Control',
                                      'Soil Warming'),
                       flux.year = year)]
wtd.env <- wtd.env[, .(wtd.mean = round(mean(WTD, na.rm = TRUE), 2)),
                   by = .(flux.year, treatment)]

### Create Summaries and Derived Variables 
wtd.monthly <- wtd.f[
  !(plot.id %in% paste(seq(1, 6), 'NA', sep = '_')),
][,
  .(wtd.mean = mean(wtd, na.rm = TRUE),
    wtd.sd = sd(wtd, na.rm = TRUE),
    wtd.n = .N),
  by = .(flux.year, month, fence, plot.id, treatment)
]

wtd.annual <- wtd.f[
  !(plot.id %in% paste(seq(1, 6), 'NA', sep = '_')),
][,
  .(wtd.mean = mean(wtd, na.rm = TRUE),
    wtd.sd = sd(wtd, na.rm = TRUE),
    wtd.n = .N),
  by = .(flux.year, fence, plot.id, treatment)
]

# check output
ggplot(wtd.monthly[!(plot.id %in% c('1_NA', '2_NA', '3_NA', '4_NA', '5_NA', '6_NA'))], 
       aes(x = flux.year + month/12)) +
  geom_line(aes(y = wtd.mean, color = 'WTD Mean')) +
  geom_line(aes(y = wtd.sd, color = 'WTD SD')) +
  geom_line(aes(y = wtd.n, color = 'WTD N')) +
  facet_wrap(~ plot.id, ncol = 8)

ggplot(wtd.annual[!(plot.id %in% c('1_NA', '2_NA', '3_NA', '4_NA', '5_NA', '6_NA'))], 
       aes(x = flux.year)) +
  geom_line(aes(y = wtd.mean, color = 'WTD Mean')) +
  geom_line(aes(y = wtd.sd, color = 'WTD SD')) +
  geom_line(aes(y = wtd.n, color = 'WTD N')) +
  facet_wrap(~ plot.id, ncol = 8)
###########################################################################################

### Load Thaw Depth Data ##################################################################
td <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/td/Thaw_depth_2009-2021.csv")
td[, V1 := NULL]

# Remove rows without plot data or thaw depth data
td <- td[!is.na(plot)]
td <- td[!is.na(td)]
# Remove DryPEHR Data
td <- td[!is.na(as.numeric(plot))]

# Date
td[, date := parse_date_time(date, orders = c('Y!-m!*-d!', 'm!-d!-y!'))]
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
td.f <- td[,.(date, flux.year, month, fence, TD_Date, treatment, plot.id, td)]

### Create Summaries and Derived Variables
td.monthly <- td.f[,
                     .(td = max(td, na.rm = TRUE)),
                     by = .(flux.year, month, fence, plot.id, treatment)
]

td.annual <- td.f[,
                    .(td = max(td, na.rm = TRUE)),
                    by = .(flux.year, fence, plot.id, treatment)
]

ggplot(td.monthly, 
       aes(x = flux.year + month/12)) +
  geom_line(aes(y = td, color = 'TD Mean')) +
  facet_wrap(~ plot.id, ncol = 8)

ggplot(td.annual, 
       aes(x = flux.year)) +
  geom_line(aes(y = td, color = 'TD Mean')) +
  facet_wrap(~ plot.id, ncol = 8)
###########################################################################################

### NDVI ##################################################################################
ndvi <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/ndvi/EML_AK_CiPEHR_NDVI_2009-2021EL.csv",
              stringsAsFactors = FALSE)
ndvi <- ndvi[plot.type == 'flux' & !is.na(as.numeric(plot))]
ndvi[
  , 
  ':=' (date = parse_date_time(date, orders = c('m!*/d!/Y!', 'm!*/d!/y!')),
        ndvi.date = mdy(date),
        plot = as.numeric(plot),
        ndvi = NDVI_relative)
][,
  ':=' (month = month(date),
        plot.id = paste(fence, plot, sep = '_'))
  ][,
    flux.year := fifelse(month >= 10,
                        year + 1,
                        year)
  ]

ndvi <- ndvi[, .(date, ndvi.date, flux.year, month, fence, plot, plot.id, ndvi)]

### Create Summaries and Derived Variables
ndvi.monthly <- ndvi[
  ,
  .SD[ndvi == max(ndvi, na.rm = TRUE)],
  by = .(flux.year, month, fence, plot, plot.id)
][
  ,
  date := NULL
  ][
    ,
    .(ndvi = first(ndvi),
      ndvi.date = first(ndvi.date)),
    by = .(flux.year, month, fence, plot, plot.id)
  ]

ndvi.annual <- ndvi[
  ,
  .SD[ndvi == max(ndvi, na.rm = TRUE)],
  by = .(flux.year, fence, plot, plot.id)
][
  ,
  ':=' (date = NULL,
        month = NULL)
][
  ,
  .(ndvi = first(ndvi),
   ndvi.date = first(ndvi.date)),
  by = .(flux.year, fence, plot, plot.id)
]

ggplot(ndvi.monthly, aes(x = flux.year + month/12)) +
  geom_line(aes(y = ndvi)) +
  facet_grid(fence ~ plot)

ggplot(ndvi.annual, aes(x = flux.year)) +
  geom_line(aes(y = ndvi)) +
  facet_grid(fence ~ plot)
###########################################################################################

### Merge Sub-Weekly Datasets #############################################################
### Half-hourly (Rolling joins)
# Water Table Depth
# set the key to allow proper rolling join
setkey(wtd.f, fence, treatment, plot.id, flux.year, month, date)
setkey(flux.hh, fence, treatment, plot.id, flux.year, month, date)

# Rolling join of water table depth and flux
# This joins by matching treatment and plot id and finding the closest date match between
# wtd and flux. In cases where there are two thaw depths equally distant in time
# from the flux measurement, it will return both, resulting in a longer data table than 
# the flux input. 
flux.hh <- wtd.f[flux.hh, roll = 'nearest']

# Remove duplicate wtd measurements for one flux measurement by selecting the earlier thaw
# depth
setkey(flux.hh, ts, date, flux.year, year, season, month, week, doy, hour, 
       hourmin, block, fence, plot, plot.id, treatment)
flux.hh <- flux.hh[, 
             first(.SD), 
             by = .(ts, date, flux.year, year, season, month, week, doy, hour, 
                    hourmin, block, fence, plot, plot.id, treatment)]

# If WTD date is more than 2 weeks from date, remove wtd
flux.hh[abs(interval(date, WTD_Date)/ddays(1)) > 14,
        ':=' (wtd = NA,
              WTD_Date = NA)]
flux.hh[month %in% seq(5, 9) & is.na(wtd), .N]/nrow(flux.hh[month %in% seq(5, 9)])
flux.hh[month %in% seq(5, 9) & is.na(wtd), .N, by = .(year)]

# Thaw Depth
# set the key to allow proper rolling join
setkey(td.f, flux.year, month, fence, treatment, plot.id, date)
setkey(flux.hh, flux.year, month, fence, treatment, plot.id, date)

# Rolling join of thaw depth and flux
# This joins by matching treatment and plot id and finding the closest date match between
# thaw depth and flux. In cases where there are two thaw depths equally distant in time
# from the flux measurement, it will return both, resulting in a longer data table than 
# the flux input. 
flux.hh <- td.f[flux.hh, roll = 'nearest']

# Remove duplicate thaw depths for one flux measurement by selecting the earlier thaw
# depth
setkey(flux.hh, ts, date, flux.year, year, season, month, week, doy, hour, 
       hourmin, block, fence, plot, plot.id, treatment)
flux.hh <- flux.hh[, 
                   first(.SD),
                   by = .(ts, date, flux.year, year, season, month, week, doy, hour, 
                          hourmin, block, fence, plot, plot.id, treatment)]

# If TD date is more than 2 weeks from date, remove td
flux.hh[abs(interval(date, TD_Date)/ddays(1)) > 14,
     ':=' (td = NA,
           TD_Date = NA)]
flux.hh[month %in% seq(5, 9) & is.na(td), .N]/nrow(flux.hh[month %in% seq(5, 9)])

### NDVI
# set the key to allow proper rolling join
setkey(ndvi, flux.year, month, fence, plot, plot.id, date)
setkey(flux.hh, flux.year, month, fence, plot, plot.id, date)

# Rolling join of thaw depth and flux
# This joins by matching year, fence, and plot and finding the closest date match between
# NDVI and flux. In cases where there are two NDVIs equally distant in time
# from the flux measurement, it will return both, resulting in a longer data table than 
# the flux input. 
flux.hh <- ndvi[flux.hh, roll = 'nearest']

# Remove duplicate NDVI values for one flux measurement by selecting the earlier thaw
# depth
setkey(flux.hh, ts, date, flux.year, year, season, month, week, doy, hour, 
       hourmin, block, fence, plot, plot.id, treatment)
flux.hh <- flux.hh[, first(.SD),
             by = .(ts, date, flux.year, year, season, month, week, doy, hour, 
                    hourmin, block, fence, plot, plot.id, treatment)]

# If NDVI date is more than 2 weeks from date, remove ndvi
flux.hh[abs(interval(date, ndvi.date)/ddays(1)) > 14,
     ndvi := NA]
flux.hh[month %in% seq(5, 9) & is.na(ndvi), .N]/nrow(flux.hh[month %in% seq(5, 9)])
flux.hh[month %in% seq(5, 9) & is.na(ndvi), .N, by = 'year']


### Daily (Rolling Joins)
# Water Table Depth
# set the key to allow proper rolling join
setkey(wtd.f, fence, treatment, plot.id, flux.year, month, date)
setkey(flux.daily, fence, treatment, plot.id, flux.year, month, date)

# Rolling join of water table depth and flux
# This joins by matching treatment and plot id and finding the closest date match between
# wtd and flux. In cases where there are two thaw depths equally distant in time
# from the flux measurement, it will return both, resulting in a longer data table than 
# the flux input. 
flux.daily <- wtd.f[flux.daily, roll = 'nearest']

# Remove duplicate wtd measurements for one flux measurement by selecting the earlier thaw
# depth
setkey(flux.daily, date, flux.year, year, season, month, week, doy,
       block, fence, plot, plot.id, treatment)
flux.daily <- flux.daily[, 
                   first(.SD), 
                   by = .(date, flux.year, year, season, month, week, doy,
                          block, fence, plot, plot.id, treatment)]

# If WTD date is more than 2 weeks from date, remove wtd
flux.daily[abs(interval(date, WTD_Date)/ddays(1)) > 14,
        ':=' (wtd = NA,
              WTD_Date = NA)]
flux.daily[month %in% seq(5, 9) & is.na(wtd), .N]/nrow(flux.daily[month %in% seq(5, 9)])
flux.daily[month %in% seq(5, 9) & is.na(wtd), .N, by = .(year)]

# Thaw Depth
# set the key to allow proper rolling join
setkey(td.f, flux.year, month, fence, treatment, plot.id, date)
setkey(flux.daily, flux.year, month, fence, treatment, plot.id, date)

# Rolling join of thaw depth and flux
# This joins by matching treatment and plot id and finding the closest date match between
# thaw depth and flux. In cases where there are two thaw depths equally distant in time
# from the flux measurement, it will return both, resulting in a longer data table than 
# the flux input. 
flux.daily <- td.f[flux.daily, roll = 'nearest']

# Remove duplicate thaw depths for one flux measurement by selecting the earlier thaw
# depth
setkey(flux.daily, date, flux.year, year, season, month, week, doy,
       block, fence, plot, plot.id, treatment)
flux.daily <- flux.daily[, 
                   first(.SD),
                   by = .(date, flux.year, year, season, month, week, doy,
                          block, fence, plot, plot.id, treatment)]

# If TD date is more than 2 weeks from date, remove td
flux.daily[abs(interval(date, TD_Date)/ddays(1)) > 14,
        ':=' (td = NA,
              TD_Date = NA)]
flux.daily[month %in% seq(5, 9) & is.na(td), .N]/nrow(flux.daily[month %in% seq(5, 9)])

### NDVI
# set the key to allow proper rolling join
setkey(ndvi, flux.year, month, fence, plot, plot.id, date)
setkey(flux.daily, flux.year, month, fence, plot, plot.id, date)

# Rolling join of thaw depth and flux
# This joins by matching year, fence, and plot and finding the closest date match between
# NDVI and flux. In cases where there are two NDVIs equally distant in time
# from the flux measurement, it will return both, resulting in a longer data table than 
# the flux input. 
flux.daily <- ndvi[flux.daily, roll = 'nearest']

# Remove duplicate NDVI values for one flux measurement by selecting the earlier thaw
# depth
setkey(flux.daily, date, flux.year, year, season, month, week, doy,
       block, fence, plot, plot.id, treatment)
flux.daily <- flux.daily[, first(.SD),
                   by = .(date, flux.year, year, season, month, week, doy,
                          block, fence, plot, plot.id, treatment)]

# If NDVI date is more than 2 weeks from date, remove ndvi
flux.daily[abs(interval(date, ndvi.date)/ddays(1)) > 14,
        ndvi := NA]
flux.daily[month %in% seq(5, 9) & is.na(ndvi), .N]/nrow(flux.daily[month %in% seq(5, 9)])
flux.daily[month %in% seq(5, 9) & is.na(ndvi), .N, by = 'year']


### Monthly
flux.monthly <- merge(flux.monthly, wtd.monthly, 
                      by = c('flux.year', 'month', 'fence', 'plot.id', 'treatment'),
                      all = TRUE)
flux.monthly <- merge(flux.monthly, td.monthly, 
                     by = c('flux.year', 'month', 'fence', 'plot.id', 'treatment'),
                     all = TRUE)
flux.monthly <- merge(flux.monthly, ndvi.monthly,
                    by = c('flux.year', 'month', 'fence', 'plot', 'plot.id'),
                    all = TRUE)

# Seasonal
flux.seasonal <- merge(flux.seasonal, 
                       wtd.annual[, season := 1, ], 
                     by = c('flux.year', 'season', 'fence', 'plot.id', 'treatment'),
                     all = TRUE)
flux.seasonal <- merge(flux.seasonal, 
                       ndvi.annual[, season := 1, ],
                     by = c('flux.year', 'season', 'fence', 'plot', 'plot.id'),
                     all = TRUE)

# Annual
wtd.annual[, season := NULL]
flux.annual <- merge(flux.annual, wtd.annual, 
                    by = c('flux.year', 'fence', 'plot.id', 'treatment'),
                    all = TRUE)
ndvi.annual[, season := NULL]
flux.annual <- merge(flux.annual, ndvi.annual,
                     by = c('flux.year', 'fence', 'plot', 'plot.id'),
                     all = TRUE)
###########################################################################################


### Annual Data
### ALT ###################################################################################
alt.annual <- td[, .(alt = max(td, na.rm = TRUE),
                alt.date = TD_Date[which(td == max(td, na.rm = TRUE))]),
            by = .(fence, plot.id, treatment, year)]
alt.annual <- alt.annual[, .(alt = first(alt),
                   alt.date = first(alt.date)),
               by = .(fence, plot.id, treatment, year)]
alt.annual <- alt.annual[, .(flux.year = year, fence, plot.id, treatment, alt, alt.date)]

alt.env <- alt.annual[, .(alt = round(mean(alt, na.rm = TRUE), 2),
                          alt.doy = round(mean(yday(alt.date), na.rm = TRUE))),
                   by = .(flux.year, treatment)]
############################################################################################

### Load Subsidence Data ##################################################################
sub <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/ALT_Subsidence_Corrected_2009_2020.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE)
sub <- sub[exp == 'CiPEHR']
sub[, ':='(exp = NULL,
           plot = as.numeric(plot),
           subsidence = subsidence * 100)]
sub.annual <- sub[, .(flux.year = year, fence, plot, treatment, subsidence)]
sub.2021 <- sub.annual[
  flux.year == 2009,
][, 
  ':=' (flux.year = 2021,
        subsidence = NA)
  ]
sub.annual <- rbind(sub.annual, sub.2021)

# interpolate 2021 subsidence
model.subsidence.lm <- function(df) {
  fit <- lm(subsidence ~ flux.year, data=df)
  return(list(subsidence.intercept=coef(fit)[1], 
              subsidence.slope=coef(fit)[2],
              subsidence.r2 = summary(fit)$r.squared))
}

m.subsidence <- sub.annual[, 
                           model.subsidence.lm(.SD),
                           by=c('fence', 'plot')]

ggplot(sub.annual, 
       aes(x = flux.year, y = subsidence)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_grid(fence ~ plot)

sub.annual <- merge(sub.annual, m.subsidence, by = c('fence', 'plot'))
sub.annual[, subsidence := fifelse(flux.year == 2021,
                                   subsidence.intercept + subsidence.slope*flux.year,
                                   subsidence)]
ggplot(sub.annual, 
       aes(x = flux.year, y = subsidence)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_grid(fence ~ plot)

sub.annual[, ':=' (subsidence.intercept = NULL,
                   subsidence.slope = NULL,
                   subsidence.r2 = NULL)]
sub.annual[is.na(subsidence), .N, by = 'flux.year']

### using LiDAR data doesn't produce believable results...
### leaving 2021 subsidence empty
# sub.2021 <- fread('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/elevation/plot_elev_2021_lidar.csv')
# sub.2021 <- sub.2021[!is.na(as.numeric(plot))]
# sub.2021[, plot := as.numeric(plot)]
# sub.2021 <- merge(sub.2021, sub[year == 2017], by = c('fence', 'plot', 'treatment'))
# sub.2021[, subsidence := subsidence + sub.17.21*100]
# sub.2021 <- sub.2021[, .(year = year.x, fence, plot, treatment, subsidence)]
# sub <- rbind(sub, sub.2021)
# 
# ggplot(sub, aes(x = year, y = subsidence)) +
#   geom_point() +
#   facet_grid(fence ~ plot)
###########################################################################################

### Load Biomass Data ##################################################################
# format 2021 data
biomass.2021 <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/biomass/EML_2021_Biomass_Corrected.csv',
                         stringsAsFactors = FALSE) %>%
  filter(!is.na(as.numeric(plot))) %>%
  mutate(WW = NA,
         SW = NA,
         block = case_when(fence <= 2 ~ 'A',
                           fence <= 4 ~ 'B',
                           fence <= 6 ~ 'C')) %>%
  select(year, block, fence, plot, WW, SW, species, avghits, biomass = Biomass)

# read in all data
biomass <- read.csv("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/biomass/CiPEHR/EML_AK_CiPEHR_BiomassBySpecies_2009-2013__20150320_VGS.csv",
                    stringsAsFactors = FALSE) %>%
  rbind.data.frame(read.csv("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/biomass/CiPEHR/CiPEHR_biomass_2017.csv",
                            stringsAsFactors = FALSE)) %>%
  rbind.data.frame(biomass.2021) %>%
  mutate(plot = as.numeric(plot)) %>%
  group_by(year, fence, plot) %>%
  summarise(biomass = sum(biomass, na.rm = TRUE)) # sum of all species
biomass <- as.data.table(biomass)
biomass.frame <- expand_grid(fence = seq(1, 6),
                             plot = seq(1, 8),
                             year = seq(2009, 2021)) %>%
  as.data.table()
biomass <- merge(biomass, biomass.frame, all = TRUE)
# biomass.model.subset <- biomass[year >= 2013 & year <= 2017]
# biomass.models <- lapply()
biomass[,
        biomass := fifelse(is.na(biomass) & year >= 2013,
                           zoo::na.approx(biomass),
                           biomass),
        by = c('fence', 'plot')]
ggplot(biomass, aes(x = year)) +
  geom_line(aes(y = biomass), alpha = 0.5) +
  facet_grid(fence~plot) +
  ggtitle('Annual Biomass')

biomass <- biomass[, .(flux.year = year, fence, plot, biomass)]
###########################################################################################

### Load Snow Data ########################################################################
snow <- fread('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/snow_depth/plot_snow_depth_2009_2021.csv')

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

snow.annual <- snow[, .(flux.year, block, fence, plot, treatment, snow.depth)]

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

snow.free.2021 <- read_excel('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/Computer_Backups/Healy cabin computer backup/2021/CiPEHR_DryPEHR/SnowFree_2021/Date Plots Snow Free_2021.xlsx',
                             sheet = 1) %>%
  slice(-1) %>%
  select(plot = `Plot Number`, flux = Flux) %>%
  mutate(flux = as_date(as.numeric(flux), origin = '1899-12-30')) %>% # excel uses 1900-01-01, but I think there is a difference in indexing that is causing the 2 day offset?
  separate(plot, into = c('fence', 'plot'), sep = '_',  convert = TRUE) %>%
  mutate(flux.year = 2021,
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
  rbind.data.frame(snow.free.2020) %>%
  rbind.data.frame(snow.free.2021)
snow.free <- as.data.table(snow.free)
snow.free <- snow.free[, .(doy.snow.free = round(mean(doy.snow.free, na.rm = TRUE))),
                       by = c('flux.year', 'treatment')]
rm(snow.free.2009.2016, snow.free.2017, snow.free.2018, snow.free.2019, 
   snow.free.2020, snow.free.2021)

snow.env <- merge(snow.env, snow.free, 
                  by = c('flux.year', 'treatment'),
                  all = TRUE)
###########################################################################################

### Merge Annual Data #####################################################################
### Half-Hourly
# ALT
flux.hh <- merge(flux.hh,
                 alt.annual,
                 by = c('flux.year', 'fence', 'plot.id', 'treatment'),
                 all = TRUE)

flux.hh[month %in% seq(5, 9) & is.na(alt) & year >= 2009, .N]/nrow(flux.hh[month %in% seq(5, 9) & year >= 2009])
flux.hh[is.na(alt) & flux.year >= 2009, .N]/nrow(flux.hh[year >= 2009])

# Subsidence
flux.hh <- merge(flux.hh,
                 sub.annual,
                 by = c('flux.year', 'fence', 'plot', 'treatment'),
                 all = TRUE)
flux.hh[month %in% seq(5, 9) & is.na(subsidence), .N]/nrow(flux.hh[month %in% seq(5, 9)])

flux.hh <- flux.hh[order(ts, plot.id)]
flux.hh[, tp := alt - subsidence]
flux.hh[, tp.to.date := td - subsidence]
flux.hh[is.na(subsidence) & flux.year >= 2009, .N, by = 'flux.year']
flux.hh[is.na(tp) & flux.year >= 2009, .N, by = 'flux.year']

# Biomass
flux.hh <- merge(flux.hh,
                 biomass,
                 by = c('flux.year', 'fence', 'plot'),
                 all = TRUE)
flux.hh[month %in% seq(5, 9) & is.na(biomass), .N]/nrow(flux.hh[month %in% seq(5, 9)])

# Snow
flux.hh <- merge(flux.hh,
                 snow.annual,
                 by = c('flux.year','block', 'fence', 'plot', 'treatment'),
                 all = TRUE)
flux.hh[month %in% seq(5, 9) & is.na(snow.depth), .N]/nrow(flux.hh[month %in% seq(5, 9)])


### Daily
# ALT
flux.daily <- merge(flux.daily,
                    alt.annual,
                    by = c('flux.year', 'fence', 'plot.id', 'treatment'),
                    all = TRUE)

flux.daily[month %in% seq(5, 9) & is.na(alt) & year >= 2009, .N]/nrow(flux.daily[month %in% seq(5, 9) & year >= 2009])
flux.daily[is.na(alt) & flux.year >= 2009, .N]/nrow(flux.daily[year >= 2009])

# Subsidence
flux.daily <- merge(flux.daily,
                    sub.annual,
                    by = c('flux.year', 'fence', 'plot', 'treatment'),
                    all = TRUE)
flux.daily[month %in% seq(5, 9) & is.na(subsidence), .N]/nrow(flux.daily[month %in% seq(5, 9)])

flux.daily <- merge(flux.daily, m.subsidence, by = c('fence', 'plot'))
flux.daily[, subsidence := fifelse(flux.year == 2021,
                                   subsidence.intercept + subsidence.slope*year,
                                   subsidence)]
flux.daily[, ':=' (subsidence.intercept = NULL,
                   subsidence.slope = NULL,
                   subsidence.r2 = NULL)]
flux.daily[is.na(subsidence), .N, by = 'flux.year']

flux.daily[, tp := alt - subsidence]
flux.daily[, tp.to.date := td - subsidence]
flux.daily[is.na(subsidence) & flux.year >= 2009, .N, by = 'flux.year']
flux.daily[is.na(tp) & flux.year >= 2009, .N, by = 'flux.year']

# Biomass
flux.daily <- merge(flux.daily,
                    biomass,
                    by = c('flux.year', 'fence', 'plot'),
                    all = TRUE)
flux.daily[month %in% seq(5, 9) & is.na(biomass), .N]/nrow(flux.daily[month %in% seq(5, 9)])

# Snow
flux.daily <- merge(flux.daily,
                    snow.annual,
                    by = c('flux.year','block', 'fence', 'plot', 'treatment'),
                    all = TRUE)
flux.daily[month %in% seq(5, 9) & is.na(snow.depth), .N]/nrow(flux.daily[month %in% seq(5, 9)])


### Monthly
# ALT
flux.monthly <- merge(flux.monthly,
                      alt.annual,
                      by = c('flux.year', 'fence', 'plot.id', 'treatment'),
                      all = TRUE)

flux.monthly[month %in% seq(5, 9) & is.na(alt) & year >= 2009, .N]/nrow(flux.monthly[month %in% seq(5, 9) & year >= 2009])
flux.monthly[is.na(alt) & flux.year >= 2009, .N]/nrow(flux.monthly[year >= 2009])

# Subsidence
flux.monthly <- merge(flux.monthly,
                      sub.annual,
                      by = c('flux.year', 'fence', 'plot', 'treatment'),
                      all = TRUE)
flux.monthly[month %in% seq(5, 9) & is.na(subsidence), .N]/nrow(flux.monthly[month %in% seq(5, 9)])

flux.monthly <- merge(flux.monthly, m.subsidence, by = c('fence', 'plot'))
flux.monthly[, subsidence := fifelse(flux.year == 2021,
                                     subsidence.intercept + subsidence.slope*year,
                                     subsidence)]
flux.monthly[, ':=' (subsidence.intercept = NULL,
                     subsidence.slope = NULL,
                     subsidence.r2 = NULL)]
flux.monthly[is.na(subsidence), .N, by = 'flux.year']

flux.monthly[, tp := alt - subsidence]
flux.monthly[, tp.to.date := td - subsidence]
flux.monthly[is.na(subsidence) & flux.year >= 2009, .N, by = 'flux.year']
flux.monthly[is.na(tp) & flux.year >= 2009, .N, by = 'flux.year']

# Biomass
flux.monthly <- merge(flux.monthly,
                      biomass,
                      by = c('flux.year', 'fence', 'plot'),
                      all = TRUE)
flux.monthly[month %in% seq(5, 9) & is.na(biomass), .N]/nrow(flux.monthly[month %in% seq(5, 9)])

# Snow
flux.monthly <- merge(flux.monthly,
                      snow.annual,
                      by = c('flux.year', 'block', 'fence', 'plot', 'treatment'),
                      all = TRUE)
flux.monthly[month %in% seq(5, 9) & is.na(snow.depth), .N]/nrow(flux.monthly[month %in% seq(5, 9)])

### Seasonal
flux.seasonal <- merge(flux.seasonal, 
                     alt.annual[, season := 1,],
                     by = c('flux.year', 'season', 'fence', 'plot.id', 'treatment'),
                     all = TRUE)
flux.seasonal <- merge(flux.seasonal, 
                     sub.annual[, season := 1,],
                     by = c('flux.year', 'season', 'fence', 'plot', 'treatment'),
                     all = TRUE)
flux.seasonal[, tp := alt - subsidence]
flux.seasonal <- merge(flux.seasonal, 
                     biomass[, season := 1,],
                     by = c('flux.year', 'season', 'fence', 'plot'),
                     all = TRUE)
flux.seasonal <- merge(flux.seasonal, 
                     snow.annual[, season := 1,],
                     by = c('flux.year', 'season', 'block', 'fence', 'plot', 'treatment'),
                     all = TRUE)

### Annual
alt.annual[, season := NULL]
flux.annual <- merge(flux.annual, 
                     alt.annual,
                     by = c('flux.year', 'fence', 'plot.id', 'treatment'),
                     all = TRUE)
sub.annual[, season := NULL]
flux.annual <- merge(flux.annual, 
                     sub.annual,
                     by = c('flux.year', 'fence', 'plot', 'treatment'),
                     all = TRUE)
flux.annual[, tp := alt - subsidence]
biomass[, season := NULL]
flux.annual <- merge(flux.annual, 
                     biomass,
                     by = c('flux.year', 'fence', 'plot'),
                     all = TRUE)
snow.annual[, season := NULL]
flux.annual <- merge(flux.annual, 
                     snow.annual,
                     by = c('flux.year', 'block', 'fence', 'plot', 'treatment'),
                     all = TRUE)

# merge with winter data
flux.annual <- merge(flux.annual,
                     env.winter,
                     by = c('flux.year', 'block', 'fence', 'plot', 'plot.id', 'treatment'),
                     all = TRUE)

### Clean up environment and memory
rm(alt.annual, biomass, biomass.2021, biomass.frame, ndvi, ndvi.annual, 
   ndvi.monthly, snow, snow.annual, snow.free, sub, td, td.annual,
   td.monthly, well.assignment, wtd, wtd.2009, wtd.annual, wtd.monthly)
gc()
###########################################################################################

### Merge Environmental Data Only #########################################################
# Join all data separated by treatment
env.treat <- merge(snow.env, alt.env,
                   by = c('flux.year', 'treatment'),
                   all = TRUE)
env.treat <- merge(env.treat, wtd.env,
                   by = c('flux.year', 'treatment'),
                   all = TRUE)
###########################################################################################

### Final Formatting ######################################################################
### Select variables/variable order
# Half-Hourly
# save as flux_hh rather than flux_all

# Daily
flux.daily.final <- flux.daily[
  ,
  .(date,	year,	flux.year,	season,	month,	week,	doy,	
    block,	fence,	plot,	plot.id,	treatment,	deployed,
    nee.sum,	reco.sum,	gpp.sum,
    tair.min,	tair.mean,	tair.max,	tair.sd,	tair.spread,
    growing.days,	freezing.days,	gdd,	fdd,
    rh.min,	rh.mean,	rh.max,	rh.sd,	precip,	precip.cum, par,
    t5.min,	t5.mean,	t5.max,	t5.sd,
    t5.filled.min, t5.filled.mean, t5.filled.max,	t5.filled.sd,
    t10.min,	t10.mean,	t10.max,	t10.sd,
    t10.filled.min, t10.filled.mean, t10.filled.max,	t10.filled.sd,
    t20.min,	t20.mean, t20.max,	t20.sd,
    t20.filled.min, t20.filled.mean, t20.filled.max,	t20.filled.sd,
    t40.min,	t40.mean,	t40.max,	t40.sd,
    t40.filled.min, t40.filled.mean, t40.filled.max,	t40.filled.sd,
    vwc.min,	vwc.mean,	vwc.max,	vwc.sd,	gwc.min,	gwc.mean,	gwc.max,	gwc.sd,
    wtd, wtd.date = WTD_Date,	subsidence,	td, td.date = TD_Date, alt,	alt.date,	tp.to.date, tp,
    biomass,	ndvi,	ndvi.date,	spring.snow.depth = snow.depth,
    gdd.2d,	gdd.3d,	gdd.4d,	gdd.5d,	gdd.6d,	gdd.1w,	gdd.2w,
    fdd.2d,	fdd.3d,	fdd.4d,	fdd.5d,	fdd.6d,	fdd.1w,	fdd.2w,
    precip.2d,	precip.3d,	precip.4d,	precip.5d,	precip.6d,	precip.1w,	precip.2w,
    vwc.min.2d,	vwc.min.3d,	vwc.min.4d,	vwc.min.5d,	vwc.min.6d,	vwc.min.1w,	vwc.min.2w,
    vwc.mean.2d,	vwc.mean.3d,	vwc.mean.4d,	vwc.mean.5d,	vwc.mean.6d,	vwc.mean.1w,	vwc.mean.2w,
    vwc.max.2d,	vwc.max.3d,	vwc.max.4d,	vwc.max.5d,	vwc.max.6d,	vwc.max.1w,	vwc.max.2w,
    gwc.mean.2d,	gwc.mean.3d,	gwc.mean.4d,	gwc.mean.5d,	gwc.mean.6d,	gwc.mean.1w,	gwc.mean.2w,
    gwc.max.2d,	gwc.max.3d,	gwc.max.4d,	gwc.max.5d,	gwc.max.6d,	gwc.max.1w,	gwc.max.2w,
    gwc.min.2d,	gwc.min.3d,	gwc.min.4d,	gwc.min.5d,	gwc.min.6d,	gwc.min.1w,	gwc.min.2w)
]

# Monthly
flux.monthly.final <- flux.monthly[
  ,
  .(flux.year,	year,	season,	month,	block,	fence,	plot,	plot.id,	treatment,	
    nee.sum,	reco.sum,	gpp.sum,
    tair.min,	tair.mean,	tair.max,	tair.sd,	monthly.gdd,	monthly.fdd,	gdd,	fdd,
    rh.min,	rh.mean,	rh.max,	rh.sd,	precip,	precip.cum, par,
    t5.min,	t5.mean,	t5.max,	t5.sd,
    t5.filled.min, t5.filled.mean, t5.filled.max,	t5.filled.sd,
    t10.min,	t10.mean,	t10.max,	t10.sd,
    t10.filled.min, t10.filled.mean, t10.filled.max,	t10.filled.sd,
    t20.min,	t20.mean, t20.max,	t20.sd,
    t20.filled.min, t20.filled.mean, t20.filled.max,	t20.filled.sd,
    t40.min,	t40.mean,	t40.max,	t40.sd,
    t40.filled.min, t40.filled.mean, t40.filled.max,	t40.filled.sd,
    vwc.min,	vwc.mean,	vwc.max,	vwc.sd,	gwc.min,	gwc.mean,	gwc.max,	gwc.sd,
    wtd.mean,	wtd.sd,	wtd.n,	subsidence,	td, alt,	alt.date,	tp.to.date,	tp,
    biomass,	ndvi,	ndvi.date,	spring.snow.depth = snow.depth,
    gdd.2m,	gdd.3m,	fdd.2m,	fdd.3m,	precip.2m,	precip.3m,
    vwc.min.2m,	vwc.min.3m, vwc.mean.2m,	vwc.mean.3m,	vwc.max.2m,	vwc.max.3m,
    gwc.min.2m,	gwc.min.3m, gwc.mean.2m,	gwc.mean.3m,	gwc.max.2m,	gwc.max.3m)
]
    
# Seasonal
flux.seasonal.final <- flux.seasonal[
  ,
  .(flux.year,	block,	fence,	plot,	plot.id,	treatment,	season,
    nee.sum,	reco.sum,	gpp.sum,
    tair.min,	tair.mean, tair.max,	tair.sd,	gdd,	fdd, seasonal.gdd, seasonal.fdd,
    rh.min,	rh.mean,	rh.max,	rh.sd,	precip,	par,
    t5.min,	t5.mean,	t5.max,	t5.sd,
    t5.filled.min, t5.filled.mean, t5.filled.max,	t5.filled.sd,
    t10.min,	t10.mean,	t10.max,	t10.sd,
    t10.filled.min, t10.filled.mean, t10.filled.max,	t10.filled.sd,
    t20.min,	t20.mean, t20.max,	t20.sd,
    t20.filled.min, t20.filled.mean, t20.filled.max,	t20.filled.sd,
    t40.min,	t40.mean,	t40.max,	t40.sd,
    t40.filled.min, t40.filled.mean, t40.filled.max,	t40.filled.sd,
    vwc.min,	vwc.mean,	vwc.max,	vwc.sd,	gwc.min,	gwc.mean,	gwc.max,	gwc.sd,
    wtd.mean,	wtd.sd,	wtd.n,
    subsidence,	alt,	alt.date,	tp,
    biomass,	ndvi,	ndvi.date,
    spring.snow.depth = snow.depth,	winter.fdd,	winter.tair.min,	winter.tair.mean,	winter.tair.sd,
    winter.t5.min,	winter.t5.mean,	winter.t5.sd,	winter.t10.min,	winter.t10.mean,
    winter.t10.sd,	winter.t20.min,	winter.t20.mean,	winter.t20.sd,
    winter.t40.min,	winter.t40.mean,	winter.t40.sd)
][order(flux.year, fence, plot, season)]

# Annual
flux.annual.final <- flux.annual[
  , 
  .(flux.year,	block,	fence,	plot,	plot.id,	treatment,	
    nee.sum,	reco.sum,	gpp.sum, 
    tair.min, tair.mean, tair.max,	tair.sd, gdd,	fdd,	
    rh.min, rh.mean,	rh.max,	rh.sd, precip,	par,
    t5.min, t5.mean, t5.max,	t5.sd,	
    t5.filled.min, t5.filled.mean,	t5.filled.max, t5.filled.sd, 
    t10.min,	t10.mean, t10.max,	t10.sd,	
    t10.filled.min, t10.filled.mean,	t10.filled.max, t10.filled.sd,	
    t20.min, t20.mean,	t20.max,	t20.sd,	
    t20.filled.min, t20.filled.mean,	t20.filled.max, t20.filled.sd,	
    t40.min, t40.mean, t40.max,	t40.sd,	
    t40.filled.min, t40.filled.mean,	t40.filled.max, t40.filled.sd, 
    vwc.min,	vwc.mean, vwc.max,	vwc.sd,	gwc.min,	gwc.mean,	gwc.max,	gwc.sd,	
    wtd.mean, wtd.sd,	wtd.n,  alt,	alt.date,	subsidence,	tp,	
    biomass,	ndvi,	ndvi.date, 
    spring.snow.depth = snow.depth,	
    winter.tair.min, winter.tair.mean,	winter.tair.sd, winter.fdd,	
    winter.t5.min,	winter.t5.mean,	winter.t5.sd,	
    winter.t10.min,	winter.t10.mean, winter.t10.sd,	
    winter.t20.min,	winter.t20.mean,	winter.t20.sd,	
    winter.t40.min, winter.t40.mean,	winter.t40.sd
  )]
###########################################################################################

### Data Export ###########################################################################
### Half-hourly
saveRDS(flux.hh,
        '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_hh.RData')

### Daily
write.csv(flux.daily.final, 
          '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_daily.csv',
          row.names = FALSE)
# GS only
flux.daily.gs <- flux.daily[season == 1 & year >= 2009]
write.csv(flux.daily.gs, 
          '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_daily_gs.csv',
          row.names = FALSE)

### Monthly
write.csv(flux.monthly.final, 
          '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_monthly.csv',
          row.names = FALSE)
# GS only
flux.monthly.gs <- flux.monthly.final[season == 1 & year >= 2009]
write.csv(flux.monthly.gs, 
          '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_monthly_gs.csv',
          row.names = FALSE)

### Seasonal
write.csv(flux.seasonal.final, 
          '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_seasonal.csv',
          row.names = FALSE)

### Annual
write.csv(flux.annual.final, 
          '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_annual.csv',
          row.names = FALSE)

# Winter Meteorological/Environmental Data
write.csv(env.winter, 
          '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/env_winter.csv',
          row.names = FALSE)

# treatment level environmental data
write.csv(env.treat,
          '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/env_annual_treatment.csv',
          row.names = FALSE)
###########################################################################################

### Merge WTD, precip, subsidence, td for soil hydrology investigation ####################
### Join TD to WTD
# set the key to allow proper rolling join
wtd.f <- wtd.f[!(str_detect(plot.id, 'NA'))]
wtd.f <- wtd.f[ , WTD_Date := NULL]
setkey(wtd.f, fence, treatment, plot.id, flux.year, month, date)
setkey(td.f, fence, treatment, plot.id, flux.year, month, date)

# Rolling join of water table depth and flux
# This joins by matching treatment and plot id and finding the closest date match between
# wtd and td. In cases where there are two thaw depths equally distant in time
# from the wtd measurement, it will return both, resulting in a longer data table than 
# the wtd input. 
hydrology <- td.f[wtd.f, roll = 'nearest']


# Calculate rolling precipitation sum
# Try both 1 week and 2 week
precip.f <- weather.f[, .(date, hour, precip)]
precip.f <- precip.f[date >= as_date('2009-01-01')]
precip.f <- precip.f[, .(precip = sum(precip, na.rm = TRUE)), by = 'date']
precip.f[, ':=' (precip.1w = frollsum(precip, n = 7, align = 'right', na.rm = TRUE),
                 precip.2w = frollsum(precip, n = 14, align = 'right', na.rm = TRUE),
                 precip = NULL)]

# Join precip to hydrology
hydrology <- merge(hydrology, precip.f, by = 'date',
                   all.x = TRUE, all.y = FALSE)

# Join subsidence to hydrology
hydrology[, plot := as.numeric(str_sub(plot.id, start = 3))]
hydrology <- merge(hydrology, sub.annual, by = c('flux.year', 'fence', 'plot', 'treatment'),
                   all.x = TRUE)

# # save data
# write.csv(hydrology,
#           '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/hydrology.csv',
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
  # geom_line(aes(y = tair.spread, color = 'Air Temp Spread'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c(#'purple', 
                                'red', 'black', 'blue')) +
  ggtitle('Air Temperature')

ggplot(flux.daily[tair.mean >= 0], aes(x = date)) +
  geom_line(aes(y = tair.mean)) +
  facet_grid(fence~plot) +
  ggtitle('Mean Air Temperature')

# GDD and FDD
ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = fdd, color = 'FDD'), alpha = 0.5) +
  geom_line(aes(y = gdd, color = 'GDD'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('blue', 'red')) +
  ggtitle('Growing Degree Days and Freezing Degree Days')

# Soil Temps
ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = t5.max, color = 'Max Soil Temp'), alpha = 0.5) +
  geom_line(aes(y = t5.mean, color = 'Mean Soil Temp'), alpha = 0.5) +
  geom_line(aes(y = t5.min, color = 'Min Soil Temp'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue')) +
  ggtitle('5 cm Soil Temperature')

ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = t10.max, color = 'Max Soil Temp'), alpha = 0.5) +
  geom_line(aes(y = t10.mean, color = 'Mean Soil Temp'), alpha = 0.5) +
  geom_line(aes(y = t10.min, color = 'Min Soil Temp'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue')) +
  ggtitle('10 cm Soil Temperature')

ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = t20.max, color = 'Max Soil Temp'), alpha = 0.5) +
  geom_line(aes(y = t20.mean, color = 'Mean Soil Temp'), alpha = 0.5) +
  geom_line(aes(y = t20.min, color = 'Min Soil Temp'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue')) +
  ggtitle('20 cm Soil Temperature')

ggplot(flux.daily, aes(x = date)) +
  geom_line(aes(y = t40.max, color = 'Max Soil Temp'), alpha = 0.5) +
  geom_line(aes(y = t40.mean, color = 'Mean Soil Temp'), alpha = 0.5) +
  geom_line(aes(y = t40.min, color = 'Min Soil Temp'), alpha = 0.5) +
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
  geom_line(aes(y = wtd)) +
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
  geom_line(aes(y = tair.max, color = 'Max Air Temp'), alpha = 0.5) +
  geom_line(aes(y = tair.mean, color = 'Mean Air Temp'), alpha = 0.5) +
  geom_line(aes(y = tair.min, color = 'Min Air Temp'), alpha = 0.5) +
  geom_line(aes(y = winter.tair.min, color = 'Prior Winter Min Air Temp'), alpha = 0.5) +
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
  geom_line(aes(y = t5.max, color = 'Max Soil Temp'), alpha = 0.5) +
  geom_line(aes(y = t5.mean, color = 'Mean Soil Temp'), alpha = 0.5) +
  geom_line(aes(y = t5.min, color = 'Min Soil Temp'), alpha = 0.5) +
  geom_line(aes(y = winter.t5.min, color = 'Prior Winter Min Soil Temp'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue', 'turquoise')) +
  ggtitle('Annual 5 cm Soil Temperature')

ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = t10.max, color = 'Max Soil Temp'), alpha = 0.5) +
  geom_line(aes(y = t10.mean, color = 'Mean Soil Temp'), alpha = 0.5) +
  geom_line(aes(y = t10.min, color = 'Min Soil Temp'), alpha = 0.5) +
  geom_line(aes(y = winter.t10.min, color = 'Prior Winter Min Soil Temp'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue', 'turquoise')) +
  ggtitle('Annual 10 cm Soil Temperature')

ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = t20.max, color = 'Max Soil Temp'), alpha = 0.5) +
  geom_line(aes(y = t20.mean, color = 'Mean Soil Temp'), alpha = 0.5) +
  geom_line(aes(y = t20.min, color = 'Min Soil Temp'), alpha = 0.5) +
  geom_line(aes(y = winter.t20.min, color = 'Prior Winter Min Soil Temp'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue', 'turquoise')) +
  ggtitle('Annual 20 cm Soil Temperature')

ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = t40.max, color = 'Max Soil Temp'), alpha = 0.5) +
  geom_line(aes(y = t40.mean, color = 'Mean Soil Temp'), alpha = 0.5) +
  geom_line(aes(y = t40.min, color = 'Min Soil Temp'), alpha = 0.5) +
  geom_line(aes(y = winter.t40.min, color = 'Prior Winter Min Soil Temp'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue', 'turquoise')) +
  ggtitle('Annual 40 cm Soil Temperature')

# Soil Moisture
ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = vwc.max, color = 'Max VWC'), alpha = 0.5) +
  geom_line(aes(y = vwc.mean, color = 'Mean VWC'), alpha = 0.5) +
  geom_line(aes(y = vwc.min, color = 'Min VWC'), alpha = 0.5) +
  facet_grid(fence~plot) +
  scale_color_manual(values = c('red', 'black', 'blue')) +
  ggtitle('Annual VWC')

ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = vwc.sd, color = 'SD VWC'), alpha = 0.5) +
  facet_grid(fence~plot) +
  ggtitle('Annual VWC SD')

ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = gwc.max, color = 'Max GWC'), alpha = 0.5) +
  geom_line(aes(y = gwc.mean, color = 'Mean GWC'), alpha = 0.5) +
  geom_line(aes(y = gwc.min, color = 'Min GWC'), alpha = 0.5) +
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
  geom_col(aes(y = precip/100, color = 'Precip', fill = 'Precip')) +
  scale_y_continuous(name = 'Precip (cm)') +
  scale_color_manual(values = c('blue'),
                     guide = FALSE) +
  scale_fill_manual(values = c('blue'),
                    guide = FALSE) +
  ggtitle('Annual Precipitation')

# Relative Humidity
ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = rh.max, color = 'Max RH'), alpha = 0.5) +
  geom_line(aes(y = rh.mean, color = 'Mean RH'), alpha = 0.5) +
  geom_line(aes(y = rh.min, color = 'Min RH'), alpha = 0.5) +
  scale_color_manual(values = c('red', 'black', 'blue')) +
  ggtitle('Annual Relative Humidity')

# Thaw Depth, TP
ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = alt*-1, color = 'ALT'), alpha = 0.5) +
  geom_line(aes(y = tp*-1, color = 'Thaw Penetration'), alpha = 0.5) +
  scale_color_manual(values = c('black', 'red')) +
  facet_grid(fence~plot) +
  ggtitle('Annual Thaw Depth')

# NDVI
ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = ndvi), alpha = 0.5) +
  facet_grid(fence~plot) +
  ggtitle('Annual Max NDVI')

# Biomass
ggplot(flux.annual, aes(x = flux.year)) +
  geom_line(aes(y = biomass), alpha = 0.5) +
  facet_grid(fence~plot) +
  ggtitle('Annual Biomass')

# Snow Depth
ggplot(flux.annual.final, aes(x = flux.year)) +
  geom_line(aes(y = spring.snow.depth), alpha = 0.5) +
  facet_grid(fence~plot) +
  ggtitle('Annual Snow Depth')
###########################################################################################

### Compare with old files to make sure things didn't get messed up in conversion #########
old.flux.annual <- fread('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/archive/input_data/2nd_round/flux_annual.csv')

flux.compare <- merge(flux.annual[, .(flux.year, fence, plot, plot.id, nee.sum, reco.sum, gpp.sum)],
                      old.flux.annual[, .(flux.year, fence, plot, plot.id, nee.sum, reco.sum, gpp.sum)],
                      by = c('flux.year', 'fence', 'plot', 'plot.id'),
                      all = TRUE)

ggplot(flux.compare, aes(x = reco.sum.x, y = reco.sum.y, color = flux.year)) +
  geom_point()

tair.compare <- merge(flux.annual[, .(flux.year, fence, plot, plot.id, tair.min, tair.mean, tair.max, tair.sd)],
                      old.flux.annual[, .(flux.year, fence, plot, plot.id, tair.min, tair.mean, max.tair.max, tair.sd)],
                      by = c('flux.year', 'fence', 'plot', 'plot.id'),
                      all = TRUE)

ggplot(tair.compare, aes(x = tair.mean.x, y = tair.mean.y, color = flux.year)) +
  geom_point()

rh.compare <- merge(flux.annual[, .(flux.year, fence, plot, plot.id, rh.min, rh.mean, rh.max, rh.sd)],
                    old.flux.annual[, .(flux.year, fence, plot, plot.id, min.rh.min, rh.mean, max.rh.max, rh.sd)],
                    by = c('flux.year', 'fence', 'plot', 'plot.id'),
                    all = TRUE)

ggplot(rh.compare, aes(x = rh.max, y = max.rh.max, color = flux.year)) +
  geom_point()

t5.compare <- merge(flux.annual[, .(flux.year, fence, plot, plot.id, t5.min, t5.mean, t5.max, t5.sd)],
                    old.flux.annual[, .(flux.year, fence, plot, plot.id, min.t5.min, t5.mean, max.t5.max, t5.sd)],
                    by = c('flux.year', 'fence', 'plot', 'plot.id'),
                    all = TRUE)

ggplot(t5.compare, aes(x = t5.max, y = max.t5.max, color = flux.year)) +
  geom_point()

vwc.compare <- merge(flux.annual[, .(flux.year, fence, plot, plot.id, vwc.min, vwc.mean, vwc.max, vwc.sd)],
                    old.flux.annual[, .(flux.year, fence, plot, plot.id, min.vwc.min, vwc.mean, max.vwc.max, vwc.sd)],
                    by = c('flux.year', 'fence', 'plot', 'plot.id'),
                    all = TRUE)

ggplot(vwc.compare, aes(x = vwc.min, y = min.vwc.min, color = flux.year)) +
  geom_point()
ggplot(vwc.compare, aes(x = vwc.sd.x, y = vwc.sd.y, color = flux.year)) +
  geom_point()

spring.snow.depth.compare <- merge(flux.annual.final[, .(flux.year, fence, plot, plot.id, spring.snow.depth)],
                     old.flux.annual[, .(flux.year, fence, plot, plot.id, winter.snow.depth)],
                     by = c('flux.year', 'fence', 'plot', 'plot.id'),
                     all = TRUE)

ggplot(spring.snow.depth.compare, aes(x = spring.snow.depth, y = winter.snow.depth, color = flux.year)) +
  geom_point()

old.flux.daily <- fread('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/archive/input_data/2nd_round/flux_daily.csv')
old.flux.daily[, date := parse_date_time(date, orders = c('Y!-m!-d!'))]
flux.compare <- merge(flux.daily[, .(flux.year, date, fence, plot, plot.id, nee.sum, reco.sum, gpp.sum)],
                      old.flux.daily[, .(flux.year, date, fence, plot, plot.id, nee.sum, reco.sum, gpp.sum)],
                      by = c('flux.year', 'date', 'fence', 'plot', 'plot.id'),
                      all = TRUE)

ggplot(flux.compare, aes(x = gpp.sum.x, y = gpp.sum.y, color = flux.year)) +
  geom_point()
###########################################################################################