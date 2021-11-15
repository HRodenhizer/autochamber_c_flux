################################################################################
###                         Gap Fill 2019 Fluxes                             ###
###                          Code by HGR 11/2021                             ###
################################################################################

### Load Libraries #############################################################
library(data.table)
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

data.2014 <- loadRdata.('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/data./CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2014/data._Processing/NEE_PAR_coefs_cip_dryp_predicted_2014.Rdata.')
data.2015 <- loadRdata.('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/data./CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2015/data._Processing/NEE_PAR_coefs_cip_dryp_predicted_missing_weird_filled_2014_2015.Rdata.')
data.2016 <- loadRdata.('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/data./CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2016/data._Processing/NEE_PAR_coefs_cip_dryp_predicted_missing_filled_2015_2016.Rdata.')
data.2017 <- loadRdata.('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/data./CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2017/data._Processing/NEE_PAR_coefs_cip_dryp_predicted_refit_2017.Rdata.')
data.2018 <- loadRdata.('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/data./CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2018/data._processing/NEE_PAR_coefs_cip_dryp_predicted_refit_2018.Rdata.')
data.2019 <- loadRdata.('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/data./CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2019/data._processing/NEE_PAR_coefs_cip_dryp_predicted_refit_2019.Rdata.')
data.2020 <- loadRdata.('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/data./CiPEHR & DryPEHR/CO2 fluxes/Autochamber/2020/data._processing/NEE_PAR_coefs_cip_dryp_predicted_refit_2020.Rdata.')

data.2020[, timestamp1 := parse_date_time(paste(date, timestamp1), orders = c('Y!-m!-d! H!:M!:S!'))]

data <- rbind(data.2014, data.2015, data.2016, data.2017, data.2018, data.2019, data.2020,
              use.names = TRUE, fill = TRUE)
data[is.na(timestamp), timestamp := timestamp1]
data[, year := year(timestamp)]
data <- data[!is.na(flux.umol)]
################################################################################

### Plot parameters by chamber temp ############################################
data.monthly <- data[, 
                     lapply(.SD, mean, na.rm = TRUE), 
                     by = .(year, month, fence, plot),
                     .SDcols = c('a', 'GPmax', 'R', 
                                 'chambT.mean')]
data.monthly[a > 0.15, a := NA]
data.monthly[GPmax > 30 | GPmax < -100, GPmax := NA]
data.monthly[R < -100, R := NA]

# plot a
ggplot(data.monthly, aes(x = chambT.mean, y = a, color = factor(year))) +
  geom_point() +
  scale_x_continuous(limits = c(0, 20)) +
  scale_color_viridis(discrete = TRUE)

# plot GPPmax
ggplot(data.monthly, aes(x = chambT.mean, y = GPmax, color = factor(year))) +
  geom_point() +
  scale_x_continuous(limits = c(0, 20)) +
  scale_color_viridis(discrete = TRUE)

# plot R
ggplot(data.monthly, aes(x = chambT.mean, y = R, color = factor(year))) +
  geom_point() +
  scale_x_continuous(limits = c(0, 20)) +
  scale_color_viridis(discrete = TRUE)

### Model parameters

################################################################################

### Model missing parameters using chamber temp ################################
frame.2019 <- expand_grid(year = 2019,
                          fence = seq(1, 6),
                          plot = seq(1, 8),
                          DOY = as.integer(levels(factor(data2019$DOY))),
                          half.hour = as.numeric(levels(factor(data2019$half.hour))))
frame.2019 <- data.frame(frame.2019)

filled.2019 <- merge(data2019, frame.2019, by = c('fence', 'plot', 'DOY', 'half.hour'))
################################################################################

