################################################################################
###                         Gap Fill 2019 Fluxes                             ###
###                          Code by HGR 11/2021                             ###
################################################################################

### To Do
# Model Reco in 2019 plots that don't have measurements
# Merge modeled 2019 plots with measured plots
# Try using data from flux_filled files to facilitate merge with 2019 measured data?
# This will be annoying because column names don't quite line up between years


### Load Libraries #############################################################
library(data.table)
library(lubridate)
library(tidyverse)
library(viridis)
library(gbm)
################################################################################

### Load Data ##################################################################
# will need to load in parameters (and half hourly data) for all years
# a function to load and return a file
loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# data.2014 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/data/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2014/data_Processing/NEE_PAR_coefs_cip_dryp_predicted_2014.Rdata')
# data.2015 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/data/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2015/data_Processing/NEE_PAR_coefs_cip_dryp_predicted_missing_weird_filled_2014_2015.Rdata')
# data.2016 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/data/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2016/data_Processing/NEE_PAR_coefs_cip_dryp_predicted_missing_filled_2015_2016.Rdata')
# data.2017 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/data/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2017/data_Processing/NEE_PAR_coefs_cip_dryp_predicted_refit_2017.Rdata')
# data.2018 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/data/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2018/data_processing/NEE_PAR_coefs_cip_dryp_predicted_refit_2018.Rdata')
# data.2019 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/data/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2019/data_processing/NEE_PAR_coefs_cip_dryp_predicted_refit_2019.Rdata')
# data.2020 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/data/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2020/data_processing/NEE_PAR_coefs_cip_dryp_predicted_refit_2020.Rdata')
param.2016 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2016/Processed/Filled_Fluxes_2015params_2016_DUALdowel.Rdata')
param.2017 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2017/Data_Processing/Filled_Fluxes_2017_DUALdowel_refit.Rdata')
param.2018 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2018/Data_processing/Filled_Fluxes_2018_DUALdowel_20181031.Rdata')
param.2019 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2019/Data_processing/Filled_Fluxes_2019_DUALdowel.Rdata')
param.2020 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2020/Data_processing/Filled_Fluxes_2020_DUALdowel.Rdata')

param.2016.2020 <- rbind(param.2016[, .(date, year, month, DOY, half.hour, 
                                        fence, plot, treatment,
                                        tair = Tair,
                                        nee = NEE.g.halfhour,
                                        reco = Reco.g.halfhour,
                                        gpp = GPP.g.halfhour, 
                                        PAR, a, GPmax, R)],
                         param.2017[, .(date, year, month, DOY = DOY.x, half.hour, 
                                        fence, plot, treatment,
                                        tair = Tair,
                                        nee = NEE.g.halfhour,
                                        reco = Reco.g.halfhour,
                                        gpp = GPP.g.halfhour, 
                                        PAR, a, GPmax, R)])

# data.2019[,
#           half.hour := fifelse(half.hour == 0,
#                                23.5,
#                                half.hour - 0.5)]
# data.2020[, timestamp1 := parse_date_time(paste(date, timestamp1), orders = c('Y!-m!-d! H!:M!:S!'))]
# 
# data <- rbind(data.2014, data.2015, data.2016, data.2017, data.2018, data.2019, data.2020,
#               use.names = TRUE, fill = TRUE)
# data[is.na(timestamp), timestamp := timestamp1]
# data[, year := year(timestamp)]
# data <- data[!is.na(flux.umol)]
# # data <- data[plot %in% seq(1, 8)]
# data[plot %in% c(2, 4), treatment := 'Control']
# data[plot %in% c(1, 3), treatment := 'Air Warming']
# data[plot %in% c(6, 8), treatment := 'Soil Warming']
# data[plot %in% c(5, 7), treatment := 'Air + Soil Warming']
# data[plot == 9, treatment := 'Drying']
# data[plot == 10, treatment := 'Warming']
# data[plot == 11, treatment := 'Drying + Warming']

# load 2019 hobo data
hobo.2019 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/Weather data (HOBO)/2019/Processed/HOBO_2018-10-01_to_2019-09-30_half_hourly.Rdata')
hobo.2020 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/Weather data (HOBO)/2020/Processed/HOBO_2019-10-01_to_2020-09-30_half_hourly.Rdata')
hobo.2019 <- rbind(hobo.2019, hobo.2020)[ts >= as_date('2019-01-01') & ts < as_date('2020-01-01'), .(year, doy = DOY, hourmin = half.hour, Tair.hobo = Tair, PAR.hobo = PAR)]

data <- loadRData('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_all.RData')
data[, month := month(ts)]

# try modeling with GBM
flux.seasonal <- fread('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_annual.csv')
nee.seasonal.gbm <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/nee_seasonal_gbm.rds')
reco.seasonal.gbm <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/reco_seasonal_gbm.rds')
gpp.seasonal.gbm <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/gpp_seasonal_gbm.rds')
flux.monthly <- fread('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_monthly.csv')
nee.monthly.gbm <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/nee_monthly_gbm.rds')
reco.monthly.gbm <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/reco_monthly_gbm.rds')
gpp.monthly.gbm <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/gpp_monthly_gbm.rds')
################################################################################

### Create Frame for 2019 Plots Without Data ###################################
# check for unusual temps
data[t.chamb.filled > 25, .N]
data[t.chamb.filled > 40, .N]
data[t.chamb > 25, .N]
data[t.chamb > 40, .N]
data[tair > 20, .N]
data[tair > 30, .N]

# prepare 2019 data for gap filling
frame.2019 <- expand_grid(year = 2019,
                          fence = seq(1, 6),
                          plot = seq(1, 11),
                          doy = seq(min(data[year == 2019 & !is.na(nee)]$doy), 
                                    max(data[year == 2019 & !is.na(nee)]$doy)),
                          hourmin = seq(0, 23.5, by = 0.5))
frame.2019 <- data.table(frame.2019)
frame.2019[, date := as_date('2018-12-31') + days(doy)]
frame.2019[,
           ts := parse_date_time(paste(date, 
                                       paste(str_pad(as.character(floor(hourmin)), 
                                                     side = "left", 
                                                     pad = '0',
                                                     width = 2), 
                                             str_pad(as.character(hourmin%%1*60),
                                                     side = 'left',
                                                     pad = '0',
                                                     width = 2), 
                                             sep = ':')),
                                 orders = c('Y!-m!-d! H!:M!'))]
frame.2019[,
           month := month(ts)]
# add air temperature
frame.2019 <- merge(frame.2019, hobo.2019, 
                    by = c('year', 'doy', 'hourmin'),
                    all.x = TRUE)
# add parameters
# param.2019 <- unique(data[year == 2019 & !is.na(a), .(fence, plot, year, month,
#                                                       a, GPmax, R)])
param.2019[, month := month(date)]
param.2019.unique <- unique(param.2019[!is.na(a), 
                                       .(fence, plot, year, month, a, GPmax, R)])
frame.2019 <- merge(frame.2019, param.2019.unique, by = c('fence', 'plot', 'year', 'month'),
                    all.x = TRUE)

data[, date := as_date(date)]
filled <- merge(data, 
                frame.2019, 
                by = c('fence', 'plot','ts', 'date', 'year', 'month', 
                       'doy', 'hourmin'),
                all = TRUE)
filled[, plot.id := paste(fence, plot, sep = '_')]
filled[is.na(tair),
       tair := Tair.hobo]
filled[is.na(par),
       par := PAR.hobo]
filled[is.na(tair), .N]
filled[is.na(par), .N]
filled[is.na(t.chamb.filled), .N]

# model chambT using Tair on a plot by plot basis
model.chambT.lm <- function(df) {
  fit <- lm(t.chamb ~ tair, data=df)
  return(list(chambT.intercept=coef(fit)[1], 
              chambT.slope=coef(fit)[2],
              chambT.r2 = summary(fit)$r.squared))
}

m.chambT <- data[, 
                 model.chambT.lm(.SD),
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
filled <- merge(filled, m.chambT, by = c('fence', 'plot'))
filled[,
     chambT.filled := chambT.intercept + chambT.slope*Tair]
filled[is.na(chambT.filled), .N]
ggplot(filled, aes(x = chambT, y = chambT.filled, color = factor(year))) +
  geom_point() +
  facet_grid(fence ~ plot)
filled[is.na(chambT),
       chambT := chambT.filled]
filled[is.na(chambT), .N]
filled[is.nan(chambT), .N]
################################################################################

### Model parameters with chamber temp #########################################
data.monthly <- filled[, 
                       lapply(.SD, mean, na.rm = TRUE), 
                       by = .(year, month, fence, plot),
                       .SDcols = c('a', 'GPmax', 'R', 
                                   'chambT')]
data.monthly <- data.monthly[, 
                             lapply(.SD, 
                                    function(x) replace(x, list = is.nan(x), values = NA))]

data.monthly[a > 0.15, a := NA]
data.monthly[GPmax > 30 | GPmax < -100, GPmax := NA]
data.monthly[R < -100, R := NA]
data.monthly[chambT > 20, chambT := NA]

# plot a
ggplot(data.monthly, aes(x = chambT, y = a, color = factor(year))) +
  geom_point() +
  scale_x_continuous(limits = c(0, 20)) +
  scale_color_viridis(discrete = TRUE)

# plot GPPmax
ggplot(data.monthly, aes(x = chambT, y = GPmax, color = factor(year))) +
  geom_point() +
  scale_x_continuous(limits = c(0, 20)) +
  scale_color_viridis(discrete = TRUE)

# plot R
ggplot(data.monthly, aes(x = chambT, y = R, color = factor(year))) +
  geom_point() +
  scale_x_continuous(limits = c(0, 20)) +
  scale_color_viridis(discrete = TRUE)

### Model parameters
model.parameter.lm <- function(df, parameter) {
  fit <- lm(get(parameter) ~ chambT + I(chambT^2), data=df)
  output <- list(coef(fit)[1],
                 coef(fit)[2],
                 coef(fit)[3],
                 summary(fit)$r.squared)
  names(output) <- c(paste0(parameter, '.param.1'),
                     paste0(parameter, '.param.2'),
                     paste0(parameter, '.param.3'),
                     paste0(parameter, '.r2'))
  return(output)
}

# # model light response parameters using the relationship with air temp
# m.a <- lm(a ~ chambT + I(chambT^2), data = data.monthly[!(is.na(chambT) & is.na(a))])
# summary(m.a)
m.a <- data.monthly[!(is.na(chambT) & is.na(a)),
                    model.parameter.lm(.SD, 'a'),
                    by=c('fence', 'plot')]
m.a

# m.gpmax <- lm(GPmax ~ chambT + I(chambT^2), data = data.monthly[is.na(chambT)==F & is.na(GPmax)==F])
# summary(m.gpmax)
m.gpmax <- data.monthly[!(is.na(chambT) & is.na(GPmax)),
                    model.parameter.lm(.SD, 'GPmax'),
                    by=c('fence', 'plot')]
m.gpmax

# m.r <- lm(R ~ chambT + I(chambT^2), data = data.monthly[is.na(chambT)==F & is.na(R)==F])
# summary(m.r)
m.r <- data.monthly[!(is.na(chambT) & is.na(R)),
                    model.parameter.lm(.SD, 'R'),
                    by=c('fence', 'plot')]
m.r

# model parameters based on environmental conditions to compare with actual parameters
data.monthly <- merge(data.monthly,
                      m.a,
                      by = c('fence', 'plot'),
                      all.x = TRUE)
data.monthly <- merge(data.monthly,
                      m.gpmax,
                      by = c('fence', 'plot'),
                      all.x = TRUE)
data.monthly <- merge(data.monthly,
                      m.r,
                      by = c('fence', 'plot'),
                      all.x = TRUE)

# data.monthly[is.na(chambT)==F & is.na(a)==F,
#              a.filled := coef(m.a)[1] + (coef(m.a)[2] * chambT) + (coef(m.a)[3] * chambT^2)]
# 
# data.monthly[is.na(chambT)==F & is.na(GPmax)==F,
#              gpmax.filled := coef(m.gpmax)[1] + (coef(m.gpmax)[2] * chambT) + (coef(m.gpmax)[3] * chambT^2)]
# 
# data.monthly[is.na(chambT)==F & is.na(R)==F,
#              r.filled := coef(m.r)[1] + (coef(m.r)[2] * chambT) + (coef(m.r)[3] * chambT^2)]
data.monthly[,
             a.filled := a.param.1 + (a.param.2 * chambT) + (a.param.3 * chambT^2)]

data.monthly[,
             gpmax.filled := GPmax.param.1 + (GPmax.param.2 * chambT) + (GPmax.param.3 * chambT^2)]

data.monthly[,
             r.filled := R.param.1 + (R.param.2 * chambT) + (R.param.3 * chambT^2)]
# missing 1 value in 2016 is okay, because we are only filling 2019 data
data.monthly[is.na(a.filled), .N]
data.monthly[is.na(gpmax.filled), .N]
data.monthly[is.na(r.filled), .N]

# plot parameters modeled with environmental variables with original model parameters
# a
ggplot(data.monthly, aes(x=chambT, y = a)) +
  geom_point(aes(color = "Parameters")) +
  geom_point(aes(y=a.filled, colour="Modeled Parameters")) +
  scale_color_manual(values = c("red", "black"))

# GPmax
ggplot(data.monthly, aes(x=chambT, y = GPmax)) +
  geom_point(aes(color = "Parameters")) +
  geom_point(aes(y=gpmax.filled, colour="Modeled Parameters")) +
  scale_color_manual(values = c("red", "black"))

# R
ggplot(data.monthly, aes(x=chambT, y = R)) +
  geom_point(aes(color = "Parameters")) +
  geom_point(aes(y=r.filled, colour="Modeled Parameters")) +
  scale_color_manual(values = c("red", "black"))

# Is this step causing the unreasonable NEE values?
# Adjust modeled parameters with unreasonable values
# figure out median parameter values at low temperatures where modeled parameters are bad
a.low <- min(data.monthly[a > 0]$a, na.rm = TRUE)
gpmax.low <- min(data.monthly[GPmax > 0]$GPmax, na.rm = TRUE)
r.low <- max(data.monthly[R < 0]$R, na.rm = TRUE)

data.monthly[a.filled < a.low,
               a.filled := a.low]
data.monthly[gpmax.filled < gpmax.low,
               gpmax.filled := gpmax.low]
data.monthly[r.filled > r.low,
               r.filled := r.low]

# plot parameters modeled with environmental variables with original model parameters
# a
ggplot(data.monthly, aes(x=chambT, y = a)) +
  geom_point(aes(color = "Parameters")) +
  geom_point(aes(y=a.filled, colour="Modeled Parameters")) +
  scale_color_manual(values = c("red", "black"))

# GPmax
ggplot(data.monthly, aes(x=chambT, y = GPmax)) +
  geom_point(aes(color = "Parameters")) +
  geom_point(aes(y=gpmax.filled, colour="Modeled Parameters")) +
  scale_color_manual(values = c("red", "black"))

# R
ggplot(data.monthly, aes(x=chambT, y = R)) +
  geom_point(aes(color = "Parameters")) +
  geom_point(aes(y=r.filled, colour="Modeled Parameters")) +
  scale_color_manual(values = c("red", "black"))
################################################################################

### Model missing parameters using chamber temp ################################
# clean up the data
filled[a > 0.15, a := NA]
filled[GPmax > 30 | GPmax < -100, GPmax := NA]
filled[R < -100, R := NA]

# prep gap filled parameters to join with data
filled.param <- data.monthly[, 
                             .(fence, plot, year, month, a, GPmax, R,
                               chambT.monthly = chambT,
                               a.param.1, a.param.2, a.param.3, a.r2,
                               GPmax.param.1, GPmax.param.2, GPmax.param.3, GPmax.r2,
                               R.param.1, R.param.2, R.param.3, R.r2,
                               a.filled, gpmax.filled, r.filled)]

# join gap filled parameters with data
filled <- merge(filled, filled.param,
                by = c('fence', 'plot', 'year', 'month', 'a', 'GPmax', 'R'),
                all.x = TRUE)
filled[year == 2019 & is.na(a.filled), .N]
filled[year == 2019 & is.na(gpmax.filled), .N]
filled[year == 2019 & is.na(r.filled), .N]

filled[,
       ':=' (filled.a = fifelse(is.na(a),
                               1,
                               0),
             filled.gpmax = fifelse(is.na(GPmax),
                                    1,
                                    0),
             filled.r = fifelse(is.na(R),
                                1,
                                0))]
filled[year == 2019, .N, by = 'filled.a']
filled[year == 2019, .N, by = 'filled.gpmax']
filled[year == 2019, .N, by = 'filled.r']

# add gap filled values to parameter columns
filled[,
       ':=' (a = fifelse(filled.a == 1,
                         a.filled,
                         a),
             GPmax = fifelse(filled.gpmax == 1,
                             gpmax.filled,
                             GPmax),
             R = fifelse(filled.r == 1,
                         r.filled,
                         R))]
filled[year == 2019 & is.na(a), .N]
filled[year == 2019 & is.na(GPmax), .N]
filled[year == 2019 & is.na(R), .N]

# # plot the parameters
# # a
# ggplot(filled.2019, aes(x=chambT, y = a, color = factor(filled.param))) +
#   geom_point() +
#   scale_color_manual(values = c('black', 'red')) +
#   facet_grid(fence ~ plot)
# 
# # GPmax
# ggplot(filled.2019, aes(x=chambT, y = a, color = factor(filled.param))) +
#   geom_point() +
#   scale_color_manual(values = c('black', 'red')) +
#   facet_grid(fence ~ plot)
# 
# # R
# ggplot(filled.2019, aes(x=chambT, y = a, color = factor(filled.param))) +
#   geom_point() +
#   scale_color_manual(values = c('black', 'red')) +
#   facet_grid(fence ~ plot)
################################################################################

### Model NEE Using Gap Filled Parameters ######################################
filled[, 
            NEE.pred := (a*PAR*GPmax)/((a*PAR) + GPmax) + R]
filled[year == 2019 & is.na(NEE.pred), .N]
filled[NEE.pred < -400, .N]


# # need to fix ridiculous nee values!
# 
# 
# filled[NEE.pred < -400,
#             ':=' (a = a.filled,
#                   GPmax = gpmax.filled,
#                   R = r.filled)]
# filled[NEE.pred < -400,
#             ':=' (NEE.pred = (a*PAR*GPmax)/((a*PAR) + GPmax) + R,
#                   filled.param = 1)]

# plot predicted vs. measured NEE
ggplot(filled[year == 2019 & !is.na(flux.umol)], 
       aes(x = flux.umol, y = NEE.pred, color = treatment)) +
  geom_point()
# these show both measured and modeled for points that have data
ggplot(filled[year == 2019], aes(x = PAR)) +
  geom_point(aes(y = flux.umol, color = 'Measured')) +
  geom_point(aes(y = NEE.pred, color = 'Modeled')) +
  scale_color_manual(breaks = c('Measured', 'Modeled'),
                     values = c('black', 'red'))
ggplot(filled[year == 2019], aes(x = timestamp)) +
  geom_point(aes(y = flux.umol, color = 'Measured')) +
  geom_point(aes(y = NEE.pred, color = 'Modeled')) +
  scale_color_manual(breaks = c('Measured', 'Modeled'),
                     values = c('black', 'red'))
# these show only measured for points that have data to highlight the measurements that will actually be gap filled
ggplot(filled[year == 2019], aes(x = PAR)) +
  geom_point(aes(y = flux.umol, color = 'Measured')) +
  geom_point(data = filled[year == 2019 & is.na(flux.umol)], 
             aes(y = NEE.pred, color = 'Modeled')) +
  scale_color_manual(breaks = c('Measured', 'Modeled'),
                     values = c('black', 'red'))
ggplot(filled[year >= 2018], aes(x = PAR)) +
  geom_point(aes(y = flux.umol, color = 'Measured')) +
  geom_point(data = filled[year == 2019 & is.na(flux.umol)], 
             aes(y = NEE.pred, color = 'Modeled')) +
  scale_color_manual(breaks = c('Measured', 'Modeled'),
                     values = c('black', 'red')) +
  facet_grid(fence ~ plot)
ggplot(filled[year >= 2018], aes(x = PAR)) +
  geom_point(aes(y = flux.umol, color = year)) +
  geom_point(data = filled[year == 2019 & is.na(flux.umol)], 
             aes(y = NEE.pred), color = 'red') +
  facet_grid(fence ~ plot)
ggplot(filled[year == 2019], aes(x = timestamp)) +
  geom_point(aes(y = flux.umol, color = 'Measured')) +
  geom_point(data = filled[year == 2019 & is.na(flux.umol)], 
             aes(y = NEE.pred, color = 'Modeled')) +
  scale_color_manual(breaks = c('Measured', 'Modeled'),
                     values = c('black', 'red'))
################################################################################

### Create 2019 Files ##########################################################
flux.2019.original <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2019/Data_processing/Filled_Fluxes_2019_DUALdowel.Rdata')
plots.2019 <- unique(data[year == 2019, .(fence, plot)])[, plot.id := paste(fence, plot, sep = '_')]$plot.id
filled.2019 <- filled[year == 2019 & !(plot.id %in% plots.2019),
                      .(date, year, month, DOY, half.hour, fence, plot, treatment, 
                        NEE.pred, chambT, Tair, PAR, wind_sp, wind_di, gust_sp, patm,
                        a, GPmax, R, chambT.monthly, 
                        a.param.1, a.param.2, a.param.3, a.r2,
                        GPmax.param.1, GPmax.param.2, GPmax.param.3, GPmax.r2,
                        R.param.1, R.param.2, R.param.3, R.r2,
                        filled.a, filled.gpmax, filled.r)]
filled.2019.daily <- filled.2019[,
                                 .(flux.umol = sum(flux.umol),
                                   ),
                                 by = c('date', 'year', 'month', 'DOY', 
                                        'fence', 'plot', 'treatment')]
################################################################################

### Try modeling (with GBM) ####################################################
### try monthly
# model data
flux.monthly.filled.2019 <- flux.monthly
flux.monthly.filled.2019[flux.year == 2019 & is.na(nee.sum), .N]
flux.monthly.filled.2019[flux.year == 2019 & is.na(reco.sum), .N]
flux.monthly.filled.2019[flux.year == 2019 & is.na(gpp.sum), .N]

flux.monthly.filled.2019[,
                          filled.gbm := fifelse(flux.year == 2019 & month %in% seq(5,9) & is.na(nee.sum),
                                                1,
                                                0)]

# NEE
flux.monthly.filled.2019 <- flux.monthly.filled.2019[flux.year == 2019 & month %in% seq(5,9) & is.na(nee.sum),
                                                       ':=' (nee.sum = predict(nee.monthly.gbm,
                                                                               newdata = .SD,
                                                                               n.trees = nee.monthly.gbm$n.trees),
                                                             filled.gbm = 1)]
flux.monthly.filled.2019[flux.year == 2019 & month %in% seq(5,9) & is.na(nee.sum),
                         .N]
# Reco
flux.monthly.filled.2019 <- flux.monthly.filled.2019[flux.year == 2019 & month %in% seq(5,9) & is.na(reco.sum),
                                                       ':=' (reco.sum = predict(reco.monthly.gbm,
                                                                                newdata = .SD,
                                                                                n.trees = reco.monthly.gbm$n.trees),
                                                             filled.gbm = 1)]
flux.monthly.filled.2019[flux.year == 2019 & month %in% seq(5,9) & is.na(reco.sum),
                         .N]
# GPP
flux.monthly.filled.2019 <- flux.monthly.filled.2019[flux.year == 2019 & month %in% seq(5,9) & is.na(gpp.sum),
                                                       ':=' (gpp.sum = predict(gpp.monthly.gbm,
                                                                               newdata = .SD,
                                                                               n.trees = gpp.monthly.gbm$n.trees),
                                                             filled.gbm = 1)]
flux.monthly.filled.2019[flux.year == 2019 & month %in% seq(5,9) & is.na(gpp.sum),
                         .N]

flux.monthly.filled.2019[flux.year == 2019 & month %in% seq(5,9) & is.na(flux.year),
                         .N]
flux.monthly.filled.2019[flux.year == 2019 & month %in% seq(5,9) & is.na(filled.gbm),
                         .N]


# Plot output
ggplot(flux.monthly.filled.2019[month %in% seq(5,9)],
       aes(x = flux.year, y = nee.sum, color = factor(filled.gbm))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(name = 'Gap Filled w/\nGBM Prediction',
                     values = c('black', 'red')) +
  facet_grid(month~treatment) +
  theme_bw()
ggplot(flux.monthly.filled.2019[month %in% seq(5,9)],
       aes(x = flux.year, y = reco.sum, color = factor(filled.gbm))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(name = 'Gap Filled w/\nGBM Prediction',
                     values = c('black', 'red')) +
  facet_grid(month~treatment) +
  theme_bw()
ggplot(flux.monthly.filled.2019[month %in% seq(5,9)],
       aes(x = flux.year, y = gpp.sum, color = factor(filled.gbm))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(name = 'Gap Filled w/\nGBM Prediction',
                     values = c('black', 'red')) +
  facet_grid(month~treatment) +
  theme_bw()

# # Save output
# write.csv(flux.monthly.filled.2019,
#           '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_monthly_filled_2019.csv',
#           row.names = FALSE)

flux.seasonal.filled.2019.from.monthly <- flux.monthly.filled.2019[
  flux.year == 2019 & filled.gbm == 1,
  .(nee.sum.monthly = sum(nee.sum, na.rm = TRUE),
    gpp.sum.monthly = sum(gpp.sum, na.rm = TRUE),
    reco.sum.monthly = sum(reco.sum, na.rm = TRUE)),
  by = c('fence', 'plot', 'flux.year')]

### seasonal
# rename the columns that need it
flux.seasonal[, ':=' (tp.annual = tp,
                      alt.annual = alt)]

# model data
flux.seasonal.filled.2019 <- flux.seasonal
flux.seasonal.filled.2019[flux.year == 2019 & is.na(nee.sum), .N]
flux.seasonal.filled.2019[flux.year == 2019 & is.na(reco.sum), .N]
flux.seasonal.filled.2019[flux.year == 2019 & is.na(gpp.sum), .N]

flux.seasonal.filled.2019[,
                          filled.gbm := fifelse(flux.year == 2019 & is.na(nee.sum),
                                                1,
                                                0)]

# NEE
flux.seasonal.filled.2019 <- flux.seasonal.filled.2019[flux.year == 2019 & is.na(nee.sum),
                                                       ':=' (nee.sum = predict(nee.seasonal.gbm,
                                                                               newdata = .SD,
                                                                               n.trees = nee.seasonal.gbm$n.trees),
                                                             filled.gbm = 1)]
# Reco
flux.seasonal.filled.2019 <- flux.seasonal.filled.2019[flux.year == 2019 & is.na(reco.sum),
                                                       ':=' (reco.sum = predict(reco.seasonal.gbm,
                                                                                newdata = .SD,
                                                                                n.trees = reco.seasonal.gbm$n.trees),
                                                             filled.gbm = 1)]
# GPP
flux.seasonal.filled.2019 <- flux.seasonal.filled.2019[flux.year == 2019 & is.na(gpp.sum),
                                                       ':=' (gpp.sum = predict(gpp.seasonal.gbm,
                                                                               newdata = .SD,
                                                                               n.trees = gpp.seasonal.gbm$n.trees),
                                                             filled.gbm = 1)]
# Add in sums from monthly models
flux.seasonal.filled.2019 <- merge(flux.seasonal.filled.2019,
                                   flux.seasonal.filled.2019.from.monthly,
                                   by = c('flux.year', 'fence', 'plot'),
                                   all = TRUE)

# plot to compare models
ggplot(flux.seasonal.filled.2019[filled.gbm == 1],
       aes(x = nee.sum, y = nee.sum.monthly)) +
  geom_point()

# Plot output
ggplot(flux.seasonal.filled.2019,
       aes(x = flux.year)) +
  geom_point(aes(y = nee.sum, color = factor(filled.gbm)), alpha = 0.5) +
  geom_point(aes(y = nee.sum.monthly),
             color = 'blue',
             alpha = 0.2) +
  scale_color_manual(name = 'Gap Filled w/\nGBM Prediction',
                     values = c('black', 'red')) +
  facet_wrap(~treatment) +
  theme_bw()
ggplot(flux.seasonal.filled.2019,
       aes(x = flux.year, y = reco.sum, color = factor(filled.gbm))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(name = 'Gap Filled w/\nGBM Prediction',
                     values = c('black', 'red')) +
  facet_wrap(~treatment) +
  theme_bw()
ggplot(flux.seasonal.filled.2019,
       aes(x = flux.year, y = gpp.sum, color = factor(filled.gbm))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(name = 'Gap Filled w/\nGBM Prediction',
                     values = c('black', 'red')) +
  facet_wrap(~treatment) +
  theme_bw()

### Monthly predictions lead to a wider and more realistic distribution of 
### annual flux sums, so I will use the monthly predictions
flux.seasonal.filled.2019[filled.gbm == 1, .N]
flux.seasonal.filled.2019[!is.na(nee.sum.monthly), .N]
flux.seasonal.filled.2019[filled.gbm == 1,
                          ':=' (nee.sum = nee.sum.monthly,
                                reco.sum = reco.sum.monthly,
                                gpp.sum = gpp.sum.monthly)]
flux.seasonal.filled.2019[,
                          ':=' (nee.sum.monthly = NULL,
                                reco.sum.monthly = NULL,
                                gpp.sum.monthly = NULL)]

# # Save output
# write.csv(flux.seasonal.filled.2019,
#           '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_annual_filled_2019.csv',
#           row.names = FALSE)
################################################################################