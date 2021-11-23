################################################################################
###                         Gap Fill 2019 Fluxes                             ###
###                          Code by HGR 11/2021                             ###
################################################################################

### To Do
# Try to use several years of data to get plot level models of parameters,
# because the current method doesn't give very different results for the
# different plots, whether lacking vegetation or not.

### Load Libraries #############################################################
library(data.table)
library(lubridate)
library(tidyverse)
library(viridis)
################################################################################

### Load Data ##################################################################
# will need to load in parameters (and half hourly data) for all years
# a function to load and return a file
loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}

data.2014 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/data/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2014/data_Processing/NEE_PAR_coefs_cip_dryp_predicted_2014.Rdata')
data.2015 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/data/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2015/data_Processing/NEE_PAR_coefs_cip_dryp_predicted_missing_weird_filled_2014_2015.Rdata')
data.2016 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/data/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2016/data_Processing/NEE_PAR_coefs_cip_dryp_predicted_missing_filled_2015_2016.Rdata')
data.2017 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/data/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2017/data_Processing/NEE_PAR_coefs_cip_dryp_predicted_refit_2017.Rdata')
data.2018 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/data/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2018/data_processing/NEE_PAR_coefs_cip_dryp_predicted_refit_2018.Rdata')
data.2019 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/data/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2019/data_processing/NEE_PAR_coefs_cip_dryp_predicted_refit_2019.Rdata')
data.2020 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/data/CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2020/data_processing/NEE_PAR_coefs_cip_dryp_predicted_refit_2020.Rdata')

data.2019[,
          half.hour := fifelse(half.hour == 0,
                               23.5,
                               half.hour - 0.5)]
data.2020[, timestamp1 := parse_date_time(paste(date, timestamp1), orders = c('Y!-m!-d! H!:M!:S!'))]

data <- rbind(data.2014, data.2015, data.2016, data.2017, data.2018, data.2019, data.2020,
              use.names = TRUE, fill = TRUE)
data[is.na(timestamp), timestamp := timestamp1]
data[, year := year(timestamp)]
data <- data[!is.na(flux.umol)]

# load 2019 hobo data
hobo.2019 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/Weather data (HOBO)/2019/Processed/HOBO_2018-10-01_to_2019-09-30_half_hourly.Rdata')
hobo.2020 <- loadRData('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/Weather data (HOBO)/2020/Processed/HOBO_2019-10-01_to_2020-09-30_half_hourly.Rdata')
hobo.2019 <- rbind(hobo.2019, hobo.2020)[ts >= as_date('2019-01-01') & ts < as_date('2020-01-01'), .(year, DOY, half.hour, Tair.hobo = Tair, PAR.hobo = PAR)]
################################################################################

### Gap fill chamber temp ######################################################
# check for unusual temps
data[chambT > 25, .N]
data[chambT > 35, .N]
data[chambT > 35, chambT := NA]
data[Tair > 20, .N]
data[Tair > 30, .N]

# prepare 2019 data for gap filling
frame.2019 <- expand_grid(year = 2019,
                          fence = seq(1, 6),
                          plot = seq(1, 11),
                          DOY = seq(min(data.2019$DOY), max(data.2019$DOY)),
                          half.hour = seq(0, 23.5, by = 0.5))
frame.2019 <- data.frame(frame.2019)

filled.2019 <- merge(data[timestamp >= as_date('2019-01-01') & timestamp < as_date('2020 - 01-01')], 
                     frame.2019, 
                     by = c('fence', 'plot', 'year', 'DOY', 'half.hour'),
                     all = TRUE)
filled.2019 <- merge(filled.2019, hobo.2019, by = c('year', 'DOY', 'half.hour'),
                     all.x = TRUE)
filled.2019[, date := as_date('2018-12-31') + days(DOY)]
filled.2019[is.na(timestamp),
            timestamp := parse_date_time(paste(date, 
                                          paste(str_pad(as.character(floor(half.hour)), 
                                                        side = "left", 
                                                        pad = '0',
                                                        width = 2), 
                                                str_pad(as.character(half.hour%%1*60),
                                                        side = 'left',
                                                        pad = '0',
                                                        width = 2), 
                                                sep = ':')),
                                    orders = c('Y!-m!-d! H!:M!'))]
filled.2019[,
            month := month(timestamp)]
filled.2019[is.na(Tair),
            Tair := Tair.hobo]
filled.2019[is.na(PAR),
            PAR := PAR.hobo]
filled.2019[is.na(Tair), .N]
filled.2019[is.na(PAR), .N]

# model chambT using Tair on a plot by plot basis
model.chambT.lm <- function(df) {
  fit <- lm(chambT ~ Tair, data=df)
  return(list(chambT.intercept=coef(fit)[1], 
              chambT.slope=coef(fit)[2],
              chambT.r2 = summary(fit)$r.squared))
}

m.chambT <- data[, 
                 model.chambT.lm(.SD),
                 by=c('fence', 'plot')]
m.chambT

# test out modeled chamber temps on all data
data <- merge(data, m.chambT, by = c('fence', 'plot'))
data[,
     chambT.filled := chambT.intercept + chambT.slope*Tair]
data[is.na(chambT.filled), .N]
ggplot(data, aes(x = chambT, y = chambT.filled, color = factor(year))) +
  geom_point() +
  facet_grid(fence ~ plot)
ggplot(data[fence == 4 & plot == 6], aes(x = chambT, y = chambT.filled, color = factor(year))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm')

# model all chamber temperatures to check model performance
filled.2019 <- merge(filled.2019, m.chambT, by = c('fence', 'plot'))
filled.2019[is.na(chambT),
            chambT := chambT.intercept + chambT.slope*Tair]
filled.2019[is.na(chambT), .N]
################################################################################

### Model parameters with chamber temp #########################################
data.monthly <- data[, 
                     lapply(.SD, mean, na.rm = TRUE), 
                     by = .(year, month, fence, plot),
                     .SDcols = c('a', 'GPmax', 'R', 
                                 'chambT', 'Tair')]

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
# model.parameter.lm <- function(df, parameter) {
#   fit <- lm(get(parameter) ~ chambT + I(chambT^2), data=df)
#   output <- list(coef(fit)[1], 
#                  coef(fit)[2],
#                  coef(fit)[3],
#                  summary(fit)$r.squared)
#   names(output) <- c(paste0(parameter, '.param.1'), 
#                      paste0(parameter, '.param.2'),
#                      paste0(parameter, '.param.3'),
#                      paste0(parameter, '.r2'))
#   return(output)
# }

# model light response parameters using the relationship with air temp
m.a <- lm(a ~ chambT + I(chambT^2), data = data.monthly[!(is.na(chambT) & is.na(a))])
summary(m.a)
# m.a <- data.monthly[!(is.na(chambT) & is.na(a)), 
#                     model.parameter.lm(.SD, 'a'),
#                     by=c('fence', 'plot')]
# m.a

m.gpmax <- lm(GPmax ~ chambT + I(chambT^2), data = data.monthly[is.na(chambT)==F & is.na(GPmax)==F])
summary(m.gpmax)
# m.gpmax <- data.monthly[!(is.na(chambT) & is.na(GPmax)), 
#                     model.parameter.lm(.SD, 'GPmax'),
#                     by=c('fence', 'plot')]
# m.gpmax

m.r <- lm(R ~ chambT + I(chambT^2), data = data.monthly[is.na(chambT)==F & is.na(R)==F])
summary(m.r)
# m.r <- data.monthly[!(is.na(chambT) & is.na(R)), 
#                     model.parameter.lm(.SD, 'R'),
#                     by=c('fence', 'plot')]
# m.r

# # model parameters based on environmental conditions to compare with actual parameters
# data.monthly <- merge(data.monthly,
#                       m.a,
#                       by = c('fence', 'plot'),
#                       all.x = TRUE)
# data.monthly <- merge(data.monthly,
#                       m.gpmax,
#                       by = c('fence', 'plot'),
#                       all.x = TRUE)
# data.monthly <- merge(data.monthly,
#                       m.r,
#                       by = c('fence', 'plot'),
#                       all.x = TRUE)

data.monthly[is.na(chambT)==F & is.na(a)==F,
                 a.filled := coef(m.a)[1] + (coef(m.a)[2] * chambT) + (coef(m.a)[3] * chambT^2)]

data.monthly[is.na(chambT)==F & is.na(GPmax)==F,
                 gpmax.filled := coef(m.gpmax)[1] + (coef(m.gpmax)[2] * chambT) + (coef(m.gpmax)[3] * chambT^2)]

data.monthly[is.na(chambT)==F & is.na(R)==F,
                 r.filled := coef(m.r)[1] + (coef(m.r)[2] * chambT) + (coef(m.r)[3] * chambT^2)]
# data.monthly[is.na(chambT)==F & is.na(a)==F,
#                  a.filled := a.param.1 + (a.param.2 * chambT) + (a.param.3 * chambT^2)]
# 
# data.monthly[is.na(chambT)==F & is.na(GPmax)==F,
#                  gpmax.filled := GPmax.param.1 + (GPmax.param.2 * chambT) + (GPmax.param.3 * chambT^2)]
# 
# data.monthly[is.na(chambT)==F & is.na(R)==F,
#                  r.filled := R.param.1 + (R.param.2 * chambT) + (R.param.3 * chambT^2)]

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
filled.2019[a > 0.15, a := NA]
filled.2019[GPmax > 30 | GPmax < -100, GPmax := NA]
filled.2019[R < -100, R := NA]

# gap fill parameters
filled.2019[is.na(chambT), .N]
filled.2019.monthly <- filled.2019[, 
                                   lapply(.SD, mean, na.rm = TRUE), 
                                   by = .(year, month, fence, plot),
                                   .SDcols = c('a', 'GPmax', 'R', 
                                               'chambT')]
filled.2019.monthly <- filled.2019.monthly[, 
                                           lapply(.SD, 
                                                  function(x) replace(x, list = is.nan(x), values = NA))]

# filled.2019.monthly <- merge(filled.2019.monthly,
#                              m.a,
#                              by = c('fence', 'plot'),
#                              all.x = TRUE)
# filled.2019.monthly <- merge(filled.2019.monthly,
#                              m.gpmax,
#                              by = c('fence', 'plot'),
#                              all.x = TRUE)
# filled.2019.monthly <- merge(filled.2019.monthly,
#                              m.r,
#                              by = c('fence', 'plot'),
#                              all.x = TRUE)

filled.2019.monthly[!is.na(chambT),
             a.filled := coef(m.a)[1] + (coef(m.a)[2] * chambT) + (coef(m.a)[3] * chambT^2)]
filled.2019.monthly[!is.na(chambT),
             gpmax.filled := coef(m.gpmax)[1] + (coef(m.gpmax)[2] * chambT) + (coef(m.gpmax)[3] * chambT^2)]
filled.2019.monthly[!is.na(chambT),
             r.filled := coef(m.r)[1] + (coef(m.r)[2] * chambT) + (coef(m.r)[3] * chambT^2)]
# filled.2019.monthly[!is.na(chambT),
#                     a.filled := a.param.1 + (a.param.2 * chambT) + (a.param.3 * chambT^2)]
# filled.2019.monthly[!is.na(chambT),
#                     gpmax.filled := GPmax.param.1 + (GPmax.param.2 * chambT) + (GPmax.param.3 * chambT^2)]
# filled.2019.monthly[!is.na(chambT),
#                     r.filled := R.param.1 + (R.param.2 * chambT) + (R.param.3 * chambT^2)]
filled.2019.monthly[is.na(a.filled), .N]
filled.2019.monthly[is.na(gpmax.filled), .N]
filled.2019.monthly[is.na(r.filled), .N]

# plot the parameters
# a
ggplot(filled.2019.monthly, aes(x=chambT, y = a)) +
  geom_point(aes(color = "Parameters")) +
  geom_point(aes(y=a.filled, colour="Modeled Parameters")) +
  scale_color_manual(values = c("red", "black"))

# GPmax
ggplot(filled.2019.monthly, aes(x=chambT, y = GPmax)) +
  geom_point(aes(color = "Parameters")) +
  geom_point(aes(y=gpmax.filled, colour="Modeled Parameters")) +
  scale_color_manual(values = c("red", "black"))

# R
ggplot(filled.2019.monthly, aes(x=chambT, y = R)) +
  geom_point(aes(color = "Parameters")) +
  geom_point(aes(y=r.filled, colour="Modeled Parameters")) +
  scale_color_manual(values = c("red", "black"))

# Is this step causing the unreasonable NEE values?
# Adjust modeled parameters with unreasonable values
# figure out median parameter values at low temperatures where modeled parameters are bad
a.low <- min(filled.2019.monthly[a > 0]$a, na.rm = TRUE)
gpmax.low <- min(filled.2019.monthly[GPmax > 0]$GPmax, na.rm = TRUE)
r.low <- max(filled.2019.monthly[R < 0]$R, na.rm = TRUE)

filled.2019.monthly[a.filled < a.low,
                    a.filled := a.low]
filled.2019.monthly[gpmax.filled < gpmax.low,
                    gpmax.filled := gpmax.low]
filled.2019.monthly[r.filled > r.low,
                    r.filled := r.low]

# plot the parameters
# a
ggplot(filled.2019.monthly, aes(x=chambT, y = a)) +
  geom_point(aes(color = "Parameters")) +
  geom_point(aes(y=a.filled, colour="Modeled Parameters")) +
  scale_color_manual(values = c("red", "black"))

# GPmax
ggplot(filled.2019.monthly, aes(x=chambT, y = GPmax)) +
  geom_point(aes(color = "Parameters")) +
  geom_point(aes(y=gpmax.filled, colour="Modeled Parameters")) +
  scale_color_manual(values = c("red", "black"))

# R
ggplot(filled.2019.monthly, aes(x=chambT, y = R)) +
  geom_point(aes(color = "Parameters")) +
  geom_point(aes(y=r.filled, colour="Modeled Parameters")) +
  scale_color_manual(values = c("red", "black"))

# Join modeled parameters with entire dataset and mark gap filled parameters
filled.2019.monthly[, chambT.monthly := chambT]
filled.2019 <- merge(filled.2019,
                     filled.2019.monthly[, c('chambT', 'a', 'GPmax', 'R') := NULL],
                     by = c('fence', 'plot', 'year', 'month'),
                     all.x = TRUE)
filled.2019[is.na(a.filled), .N]
filled.2019[,
            filled.param := fifelse(is.na(a) | is.na(GPmax) | is.na(R),
                              1,
                              0)]
filled.2019[filled.param == 0, .N]
filled.2019[filled.param == 1, .N]

# # plot the parameters
# # a
# ggplot(filled.2019, aes(x=chambT, y = a)) +
#   geom_point(aes(color = "Parameters")) +
#   geom_point(aes(y=a.filled, colour="Modeled Parameters")) +
#   scale_color_manual(values = c("red", "black")) +
#   facet_grid(fence ~ plot)
# 
# # GPmax
# ggplot(filled.2019, aes(x=chambT, y = GPmax)) +
#   geom_point(aes(color = "Parameters")) +
#   geom_point(aes(y=gpmax.filled, colour="Modeled Parameters")) +
#   scale_color_manual(values = c("red", "black")) +
#   facet_grid(fence ~ plot)
# 
# # R
# ggplot(filled.2019, aes(x=chambT, y = R)) +
#   geom_point(aes(color = "Parameters")) +
#   geom_point(aes(y=r.filled, colour="Modeled Parameters")) +
#   scale_color_manual(values = c("red", "black")) +
#   facet_grid(fence ~ plot)


# add gap filled values to parameter columns
filled.2019[filled.param == 1,
            ':=' (a = a.filled,
                  GPmax = gpmax.filled,
                  R = r.filled)]
filled.2019[is.na(a), .N]
filled.2019[is.na(GPmax), .N]
filled.2019[is.na(R), .N]
################################################################################

### Model NEE Using Gap Filled Parameters ######################################
filled.2019[, 
            nee.filled := (a*PAR*GPmax)/((a*PAR) + GPmax) + R]
filled.2019[is.na(nee.filled), .N]
filled.2019[nee.filled < -400, .N]


# # need to fix ridiculous nee values!
# 
# 
# filled.2019[nee.filled < -400,
#             ':=' (a = a.filled,
#                   GPmax = gpmax.filled,
#                   R = r.filled)]
# filled.2019[nee.filled < -400,
#             ':=' (nee.filled = (a*PAR*GPmax)/((a*PAR) + GPmax) + R,
#                   filled.param = 1)]

# plot predicted vs. measured NEE
ggplot(filled.2019[filled.param == 0], aes(x = flux.umol, y = nee.filled)) +
  geom_point()
# these show both measured and modeled for points that have data
ggplot(filled.2019, aes(x = PAR)) +
  geom_point(aes(y = flux.umol, color = 'Measured')) +
  geom_point(aes(y = nee.filled, color = 'Modeled')) +
  scale_color_manual(breaks = c('Measured', 'Modeled'),
                     values = c('black', 'red'))
ggplot(filled.2019, aes(x = timestamp)) +
  geom_point(aes(y = flux.umol, color = 'Measured')) +
  geom_point(aes(y = nee.filled, color = 'Modeled')) +
  scale_color_manual(breaks = c('Measured', 'Modeled'),
                     values = c('black', 'red'))
# these show only measured for points that have data to highlight the measurements that will actually be gap filled
ggplot(filled.2019, aes(x = PAR)) +
  geom_point(aes(y = flux.umol, color = 'Measured')) +
  geom_point(data = filled.2019[filled.param == 1], 
             aes(y = nee.filled, color = 'Modeled')) +
  scale_color_manual(breaks = c('Measured', 'Modeled'),
                     values = c('black', 'red'))
ggplot(filled.2019, aes(x = PAR)) +
  geom_point(aes(y = flux.umol, color = 'Measured')) +
  geom_point(data = filled.2019[filled.param == 1], 
             aes(y = nee.filled, color = 'Modeled')) +
  scale_color_manual(breaks = c('Measured', 'Modeled'),
                     values = c('black', 'red')) +
  facet_grid(fence ~ plot)
ggplot(filled.2019, aes(x = timestamp)) +
  geom_point(aes(y = flux.umol, color = 'Measured')) +
  geom_point(data = filled.2019[filled.param == 1], 
             aes(y = nee.filled, color = 'Modeled')) +
  scale_color_manual(breaks = c('Measured', 'Modeled'),
                     values = c('black', 'red'))
################################################################################