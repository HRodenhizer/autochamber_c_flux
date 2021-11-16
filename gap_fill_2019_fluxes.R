################################################################################
###                         Gap Fill 2019 Fluxes                             ###
###                          Code by HGR 11/2021                             ###
################################################################################

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

data.2020[, timestamp1 := parse_date_time(paste(date, timestamp1), orders = c('Y!-m!-d! H!:M!:S!'))]

data <- rbind(data.2014, data.2015, data.2016, data.2017, data.2018, data.2019, data.2020,
              use.names = TRUE, fill = TRUE)
data[is.na(timestamp), timestamp := timestamp1]
data[, year := year(timestamp)]
data <- data[!is.na(flux.umol)]

# load hobo data

################################################################################

### Prepare 2019 data frame for gap filling ####################################
frame.2019 <- expand_grid(year = 2019,
                          fence = seq(1, 6),
                          plot = seq(1, 8),
                          DOY = as.integer(levels(factor(data.2019$DOY))),
                          half.hour = as.numeric(levels(factor(data.2019$half.hour))))
frame.2019 <- data.frame(frame.2019)

filled.2019 <- merge(data.2019, frame.2019, by = c('fence', 'plot', 'year', 'DOY', 'half.hour'),
                     all = TRUE)
################################################################################

### Gap fill chamber temp ######################################################
# check for unusual temps
data[chambT > 25, .N]
data[chambT > 35, .N]
data[chambT > 35, chambT := NA]
data[Tair > 20, .N]
data[Tair > 30, .N]

# model chambT using Tair on a plot by plot basis
model.chambT.lm <- function(df) {
  fit <- lm(chambT ~ Tair, data=df)
  return(list(intercept=coef(fit)[1], 
              slope=coef(fit)[2],
              r2 = summary(fit)$r.squared))
  }

m.chambT <- data[, 
                 model.chambT.lm(.SD),
                 by=c('fence', 'plot')]
m.chambT

# model all chamber temperatures to check model performance

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
# model light response parameters using the relationship with air temp
m.a <- lm(a ~ chambT + I(chambT^2), data = data.monthly[!(is.na(chambT) & is.na(a))])
summary(m.a)

m.gpmax <- lm(GPmax ~ chambT + I(chambT^2), data = data.monthly[is.na(chambT)==F & is.na(GPmax)==F])
summary(m.gpmax)

m.r <- lm(R ~ chambT + I(chambT^2), data = data.monthly[is.na(chambT)==F & is.na(R)==F])
summary(m.r)

# model parameters based on environmental conditions to compare with actual parameters
data.monthly[is.na(chambT)==F & is.na(a)==F,
                 a.model := coef(m.a)[[1]] + (coef(m.a)[[2]] * chambT) + (coef(m.a)[[3]] * chambT^2)]

data.monthly[is.na(chambT)==F & is.na(GPmax)==F,
                 gpmax.model := coef(m.gpmax)[[1]] + (coef(m.gpmax)[[2]] * chambT)+ (coef(m.gpmax)[[3]] * chambT^2)]

data.monthly[is.na(chambT)==F & is.na(R)==F,
                 r.model := coef(m.r)[[1]] + (coef(m.r)[[2]] * chambT)+ (coef(m.r)[[3]] * chambT^2)]

# plot parameters modeled with environmental variables with original model parameters
# a
ggplot(data.monthly, aes(x=chambT, y = a)) +
  geom_point(aes(color = "Parameters")) +
  geom_point(aes(y=a.model, colour="Modeled Parameters")) +
  scale_color_manual(values = c("red", "black"))

# GPmax
ggplot(data.monthly, aes(x=chambT, y = GPmax)) +
  geom_point(aes(color = "Parameters")) +
  geom_point(aes(y=gpmax.model, colour="Modeled Parameters")) +
  scale_color_manual(values = c("red", "black"))

# R
ggplot(data.monthly, aes(x=chambT, y = R)) +
  geom_point(aes(color = "Parameters")) +
  geom_point(aes(y=r.model, colour="Modeled Parameters")) +
  scale_color_manual(values = c("red", "black"))
################################################################################

### Model missing parameters using chamber temp ################################

# gap fill chamber temperatures


# gap fill parameters
filled.2019[!is.na(chambT) & is.na(a),
            a.model := coef(m.a)[[1]] + (coef(m.a)[[2]] * chambT) + (coef(m.a)[[3]] * chambT^2)]
filled.2019[!is.na(chambT) & is.na(GPmax),
            gpmax.model := coef(m.gpmax)[[1]] + (coef(m.gpmax)[[2]] * chambT) + (coef(m.gpmax)[[3]] * chambT^2)]
filled.2019[!is.na(chambT) & is.na(R),
            r.model := coef(m.r)[[1]] + (coef(m.r)[[2]] * chambT) + (coef(m.r)[[3]] * chambT^2)]

# create column to indicate which parameters are filled
nrow(filled.2019[is.na(a)]) == nrow(filled.2019[is.na(a) & is.na(GPmax) & is.na(R)])
filled.2019[,
            filled := fifelse(is.na(a),
                              1,
                              0)]

# plot the parameters
# a
ggplot(filled.2019, aes(x=chambT, y = a)) +
  geom_point(aes(color = "Parameters")) +
  geom_point(aes(y=a.model, colour="Modeled Parameters")) +
  scale_color_manual(values = c("red", "black"))

# GPmax
ggplot(filled.2019, aes(x=chambT, y = GPmax)) +
  geom_point(aes(color = "Parameters")) +
  geom_point(aes(y=gpmax.model, colour="Modeled Parameters")) +
  scale_color_manual(values = c("red", "black"))

# R
ggplot(filled.2019, aes(x=chambT, y = R)) +
  geom_point(aes(color = "Parameters")) +
  geom_point(aes(y=r.model, colour="Modeled Parameters")) +
  scale_color_manual(values = c("red", "black"))


# add gap filled values to parameter columns
filled.2019[filled == 1,
            ':=' (a = a.model,
                  GPmax = gpmax.model,
                  R = r.model)]
################################################################################

