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
        select(year, fence, plot, doy.old = DOY, doy.new = DOY, hour.old = hour, hour.new = hour, co2_r2 = rsquare, T_chamb = chambT.mean)
      
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
               select(year, fence, plot, doy.old, doy.new, hour.old, hour.new, co2_r2 = rsquare, T_chamb = chambT.mean)
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
co2[, date := parse_date_time(as.Date(doy-1, origin = paste0(year, '-01-01')), orders = c('Y!-m!*-d!'))] # doy-1 because as.Date is 0 indexed, while lubridate::yday() (used to create doy variable) is 1 indexed
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
co2[, Plot_ID := paste(fence, plot, sep = '_')]
co2 <- co2[order(date, Plot_ID)]

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
co2 <- co2[, .(ts, date, year, month, week, doy, hourmin = hour, fence, plot, Plot_ID, treatment, filled, nee = NEE_g, reco = Reco_g, gpp = GPP_g, co2_r2, t.chamb = T_chamb)]
co2[,.N, by = year(date)] # make sure there are about the equal numbers in all years except 2019, which should have fewer

## add in hour variable to join with par later
co2[, hour := floor(hourmin)]

# order
co2 <- co2[order(date, Plot_ID, treatment, hourmin)]
###########################################################################################

### Weather Data ##########################################################################
filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/weather/',
                        pattern = 'csv$',
                        full.names = TRUE)
weather <- map_dfr(filenames,
                   ~ read.csv(.x,
                              header = TRUE) %>%
                     select(year = matches('[Y|y]ear'), DOY, CO2_hr = matches('hour'), Tair, PAR))

weather <- data.table(weather)

# Format time variables
weather[, date := parse_date_time(as.Date(DOY-1, origin = paste(year, '-01-01', sep = '')), orders = c('Y!-m!*-d!'))] # doy-1 because as.Date is 0 indexed, while lubridate::yday() (used to create doy variable) is 1 indexed
weather[, year := year(date)]
weather[, month := month(date)]
weather[, day := mday(date)]
weather[, hour := floor(CO2_hr)]
weather <- weather[order(date, hour)]

# Neaten
par <- weather[, .(date, hour, Tair, PAR)]
par <- par[, .(PAR = mean(PAR, na.rm = TRUE), Tair = mean(Tair, na.rm = TRUE)), by = .(date, hour)]
tair <- weather[CO2_hr >= 9 & CO2_hr <= 15, .(date, CO2_hr, Tair)]
tair <- tair[, lapply(.SD, mean, na.rm = TRUE), by = .(date)]
tair <- tair[, .(date, Tair)]
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
soil.sensor[, Plot_ID := paste(fence, plot, sep = '_')]

# Neaten
soil.sensor <- soil.sensor[, .(date, fence, hourmin, treatment, Plot_ID, t5 = T_five, t10 = T_ten,
                               t20 = T_twenty, t40 = T_forty, vwc = VWC, gwc = GWC)]
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
wtd[, WTD_Date := paste(day, month, year, sep = '/')]

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
wtd[, Plot_ID := paste(fence, plot, sep = '_')]

# Neaten
wtd <- wtd[, .(date, fence,  WTD_Date, treatment, Plot_ID, wtd = WTD)]
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
td[, TD_Date := paste(day, month, year, sep = '/')]

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
td[, Plot_ID := paste(fence, plot, sep = '_')]
td <- td[order(date, Plot_ID)]
td <- td[,.(date, fence, TD_Date, treatment, Plot_ID, Thaw_Depth = td)]
###########################################################################################

### Merge Datasets ########################################################################
### PAR
flux <- merge(co2, par, by = c('date', 'hour'), all.x = TRUE)

# Filter out daytime measurements to keep only dark respiration
flux <- flux[PAR <= 10 | month(date) %in% c(1, 2, 3, 4, 10, 11, 12)]

# Create Replicate Column
flux[, rep := sequence(.N), by = c('Flux_Date', 'Treatment', 'Plot_ID')]
flux[, Treatment_if_other := paste('REPMEAS_', rep, ':', co2_hr, sep = '')]

### co2 + CH4
flux <- merge(flux, methane, all = TRUE, by = c('date', 'year', 'doy', 'fence', 'Flux_Date', 'Treatment', 'Treatment_if_other', 'Plot_ID'))
flux <- flux[!is.na(co2) & is.na(CH4), C_Loss := 'co2']
flux <- flux[is.na(co2) & !is.na(CH4), C_Loss := 'CH4']
flux <- flux[!is.na(co2) & !is.na(CH4), C_Loss := 'co2 & CH4']

### Tair for methane
flux <- merge(flux, tair, by = c('date'))
flux[, Tair := Tair.x]
flux[is.na(Tair) & C_Loss == 'CH4', Tair := Tair.y]
flux[is.na(Tair), .N]

### Soil sensors
flux <- merge(flux,
              soil.sensor,
              by = c('date', 'fence', 'co2_hr', 'Treatment', 'Plot_ID'),
              all.x = TRUE,
              all.y = FALSE)
flux <- merge(flux,
              soil.avg,
              by = c('date', 'fence', 'Treatment', 'Plot_ID'),
              all.x = TRUE,
              all.y = FALSE)
flux[is.na(Soil_Temp.x), .N]
flux[is.na(Soil_Moist.x), .N]
flux[is.na(Soil_Temp.y), .N]
flux[is.na(Soil_Moist.y), .N]
# Fill in missing half hour measurements with daily average
flux[, Soil_Temp := Soil_Temp.x]
flux[is.na(Soil_Temp.x), Soil_Temp := Soil_Temp.y]
flux[, Soil_Moist := Soil_Moist.x]
flux[is.na(Soil_Moist.x), Soil_Moist := Soil_Moist.y]
flux[is.na(Soil_Temp), .N]
flux[is.na(Soil_Moist), .N]

### Water Table Depth
# set the key to allow proper rolling join
setkey(wtd, fence, Treatment, Plot_ID, date)
setkey(flux, fence, Treatment, Plot_ID, date)

# Rolling join of water table depth and flux
# This joins by matching treatment and plot id and finding the closest date match between
# wtd and flux. In cases where there are two thaw depths equally distant in time
# from the flux measurement, it will return both, resulting in a longer data table than 
# the flux input. 
flux <- wtd[flux, roll = 'nearest']

# Remove duplicate wtd measurements for one flux measurement by selecting the earlier thaw
# depth
setkey(flux, date, Flux_Date, Treatment, Treatment_if_other, Plot_ID, C_Loss, co2_hr,
       co2, co2_r2, CH4, T_chamb)
flux <- flux[, first(.SD), by = .(date, Flux_Date, Treatment, Treatment_if_other,
                                  Plot_ID, C_Loss, co2_hr, co2, co2_r2, CH4, T_chamb)]

flux[is.na(Water_Table_Depth), .N]

### Thaw Depth
# set the key to allow proper rolling join
setkey(td, fence, Treatment, Plot_ID, date)
setkey(flux, fence, Treatment, Plot_ID, date)

# Rolling join of thaw depth and flux
# This joins by matching treatment and plot id and finding the closest date match between
# thaw depth and flux. In cases where there are two thaw depths equally distant in time
# from the flux measurement, it will return both, resulting in a longer data table than 
# the flux input. 
flux <- td[flux, roll = 'nearest']

# Remove duplicate thaw depths for one flux measurement by selecting the earlier thaw
# depth
setkey(flux, date, Flux_Date, Treatment, Treatment_if_other, Plot_ID, C_Loss, co2_hr,
       co2, co2_r2, CH4, T_chamb, WTD_Date, Water_Table_Depth)
flux <- flux[, first(.SD),
             by = .(date, Flux_Date, Treatment,
                    Treatment_if_other, Plot_ID, C_Loss, co2_hr,co2, co2_r2, CH4,
                    T_chamb, WTD_Date, Water_Table_Depth)]
flux[is.na(Thaw_Depth), .N]
flux <- flux[order(date, Plot_ID)]

# Plot to make sure things look right
# Chamber temps vs. air temps
ggplot(flux, aes(x = Tair, y = T_chamb, colour = year), alpha = 0.2) +
  geom_point() +
  facet_grid(.~Treatment)

# co2
ggplot(flux, aes(x = doy, y = co2, colour = year), alpha = 0.2) +
  geom_point()

ggplot(flux, aes(x = Soil_Temp, y = co2, colour = year), alpha = 0.2) +
  geom_point()

ggplot(flux, aes(x = T_chamb, y = co2, colour = year), alpha = 0.2) +
  geom_point()

ggplot(flux, aes(x = Soil_Moist, y = co2, colour = year), alpha = 0.2) +
  geom_point() +
  facet_grid(.~Treatment)

# CH4
ggplot(flux, aes(x = doy, y = CH4, colour = year), alpha = 0.2) +
  geom_point()

ggplot(flux, aes(x = Soil_Temp, y = CH4, colour = year), alpha = 0.2) +
  geom_point()

ggplot(flux, aes(x = Tair, y = CH4, colour = year), alpha = 0.2) +
  geom_point()

ggplot(flux, aes(x = Soil_Moist, y = CH4, colour = year), alpha = 0.2) +
  geom_point() +
  facet_grid(.~Treatment)

# one erroneously high value
flux <- flux[co2 <= 0.5 | is.na(co2)]
###########################################################################################

### Gap Fill Chamber Temps ################################################################
# Remove really low chamber temps when air temp is much higher
flux[(Tair-T_chamb) > 10, T_chamb := NA]
ggplot(flux, aes(x = Tair, y = T_chamb, colour = year), alpha = 0.2) +
  geom_point() +
  facet_grid(.~Treatment)

tchamb.m <- lm(T_chamb ~ Tair + Treatment, data = flux)
summary(tchamb.m)

flux[Treatment == 'Control', T_chamb_m := tchamb.m$coefficients[1] + Tair*tchamb.m$coefficients[2]]
flux[Treatment == 'Air Warming', T_chamb_m := tchamb.m$coefficients[1] + Tair*(tchamb.m$coefficients[2] + tchamb.m$coefficients[3])]
flux[Treatment == 'Air + Soil Warming', T_chamb_m := tchamb.m$coefficients[1] + Tair*(tchamb.m$coefficients[2] + tchamb.m$coefficients[4])]
flux[Treatment == 'Soil Warming', T_chamb_m := tchamb.m$coefficients[1] + Tair*(tchamb.m$coefficients[2] + tchamb.m$coefficients[5])]

ggplot(flux, aes(x = Tair, y = T_chamb_m, colour = year), alpha = 0.2) +
  geom_point() +
  facet_grid(.~Treatment)

flux[is.na(T_chamb), T_chamb := ifelse(C_Loss == 'co2' & !(month(date) %in% c(1, 2, 3, 4, 10, 11, 12)),
                                       T_chamb_m,
                                       NA)]
flux[, Air_Temp := ifelse(!is.na(T_chamb),
                          T_chamb,
                          Tair)]

ggplot(flux, aes(x = Tair, y = T_chamb, colour = year), alpha = 0.2) +
  geom_point() +
  facet_grid(.~Treatment)
###########################################################################################

### Final Clean-up
flux[, Flux_Year := '']
flux[, Flux_Julian_Day := '']
flux[, Season := '']
flux[, Flux_ID := '']
flux.final <- flux[, .(date, Flux_Date, Flux_Year, Flux_Julian_Day, Treatment, Treatment_if_other,
                       Plot_ID, C_Loss, Season, Flux_ID, co2, co2_r2, CH4, CH4_r2, PAR,
                       Air_Temp, Soil_Temp, Soil_Moist, Water_Table_Depth, Thaw_Depth)]
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