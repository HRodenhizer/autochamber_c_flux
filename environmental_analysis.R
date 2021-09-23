################################################################################
###                        Environmental Data Analysis                       ###
###                             code by HGR 7/2020                           ###
################################################################################

### Load Libraries #############################################################
library(data.table)
library(lubridate)
library(readxl)
library(ggfortify)
library(ggpubr)
library(viridis)
library(emmeans)
library(raster)
library(sf)
library(tidyverse)
################################################################################

### Load Data ##################################################################
flux.monthly <- read.csv("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_monthly.csv")
flux.annual <- read.csv("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_annual.csv") %>%
  mutate(flux.year = as.factor(flux.year))
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
weather[, season := fifelse(month <= 4 | month >= 10,
                            'ngs',
                            'gs')]
weather[, flux.year := fifelse(month >= 10,
                               year + 1,
                               year)]

# Neaten
weather.daily <- weather[flux.year >= 2009,
                         .(tair.mean = mean(Tair, na.rm = TRUE),
                           tair.min = min(Tair, na.rm = TRUE),
                           tair.max = max(Tair, na.rm = TRUE),
                           precip = sum(precip, na.rm = TRUE),
                           par = mean(par, na.rm = TRUE)),
                         by = .(flux.year, month, date)]
weather.seasonal <- weather[flux.year >= 2009,
                            .(tair.mean = mean(Tair, na.rm = TRUE),
                              tair.min = min(Tair, na.rm = TRUE),
                              tair.max = max(Tair, na.rm = TRUE),
                              precip = sum(precip, na.rm = TRUE),
                              par = mean(par, na.rm = TRUE)),
                            by = .(flux.year, season)]
weather.annual <- weather[flux.year >= 2009,
                   .(tair.mean = mean(Tair, na.rm = TRUE),
                     tair.min = min(Tair, na.rm = TRUE),
                     tair.max = max(Tair, na.rm = TRUE),
                     precip = sum(precip, na.rm = TRUE)),
                   by = .(flux.year)]

# Snow depth
snow.depth <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/snow_depth/plot_snow_depth_2009_2020.csv')

# Snow free date
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
rm(snow.free.2009.2016, snow.free.2017, snow.free.2018, snow.free.2019, 
   snow.free.2020)
################################################################################

### PCA ########################################################################
# need to finalize which variables to include
env.annual.plot <- flux.annual %>%
  filter(flux.year != 2009) %>%
  select(-c(season, matches('rh'), matches('sd'),
            max.tair.spread, min.tair.spread, matches('ndvi'),
            biomass.annual, gdd, fdd, winter.fdd, precip.cum)) %>%
  mutate(subsidence = -1*subsidence.annual,
         wtd.mean = -1*wtd.mean) %>%
  na.omit()
env.annual <- env.annual.plot %>%
  select(-c(flux.year, block, fence, plot, plot.id, treatment, matches('sum')))
env.annual.subset <- env.annual %>%
  select(tair.max = max.tair.max, tair.mean, tair.spread.mean = mean.tair.spread, 
         t5.max = max.t5.max, t5.mean, 
         t10.max = max.t10.max, t10.mean, 
         t20.max = max.t20.max, t20.mean, 
         t40.max = max.t40.max, t40.mean, 
         vwc.max = max.vwc.max, vwc.mean, vwc.min = min.vwc.min,
         gwc.max = max.gwc.max, gwc.mean, gwc.min = min.gwc.min, 
         subsidence, # adding or removing subsidence doesn't change much in pca
         wtd.mean, alt = alt.annual, 
         tp = tp.annual, w.snow.depth = winter.snow.depth, 
         w.t5.min = winter.min.t5.min, w.t10.min = winter.min.t10.min, 
         w.t20.min = winter.min.t20.min, w.t40.min = winter.min.t40.min)
# pca.annual <- prcomp(as.matrix(env.annual.subset))
# saveRDS(pca.annual,
#         '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/env_pca.rds')
pca.annual <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/env_pca.rds')

# Environmental PCA colored by subsidence
pca.plot <- autoplot(pca.annual, data = env.annual.plot, colour = 'subsidence.annual',
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3) +
  scale_color_viridis(name = 'Subsidence (cm)') +
  coord_fixed() +
  theme_bw() +
  theme()
pca.plot
# zoom in on the center mass of red
pca.plot.zoom <- autoplot(pca.annual, data = env.annual.plot, colour = 'subsidence.annual',
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3) +
  scale_color_viridis(name = 'Subsidence (cm)') +
  scale_y_continuous(limits = c(-0.008, 0.008)) +
  scale_x_continuous(limits = c(-0.0075, 0.0075)) +
  coord_fixed() +
  theme_bw()
pca.plot.zoom
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/environmental_pca.jpg',
#        pca.plot,
#        height = 6,
#        width = 6)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/environmental_pca.pdf',
#        pca.plot,
#        height = 6,
#        width = 6)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/environmental_pca_zoom.jpg',
#        pca.plot.zoom,
#        height = 6,
#        width = 6)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/environmental_pca_zoom.pdf',
#        pca.plot.zoom,
#        height = 6,
#        width = 6)

autoplot(pca.annual, data = env.annual.plot, colour = 'plot.id',
         loadings = TRUE, loadings.label = TRUE) +
  scale_color_viridis(discrete = TRUE)

autoplot(pca.annual, data = env.annual.plot, colour = 'treatment',
         loadings = TRUE, loadings.label = TRUE) +
  scale_color_manual(breaks = c('Control', 'Air Warming', 'Soil Warming', 'Air + Soil Warming'),
                     values = c("#0099cc", '#009900', "#990000", '#330000'))

autoplot(pca.annual, data = env.annual.plot, colour = 'flux.year',
         loadings = TRUE, loadings.label = TRUE) +
  scale_color_viridis(discrete = TRUE)

autoplot(pca.annual, data = env.annual.plot, colour = 'nee.sum',
         loadings = TRUE, loadings.label = TRUE) +
  scale_color_viridis()

autoplot(pca.annual, data = env.annual.plot, colour = 'gpp.sum',
         loadings = TRUE, loadings.label = TRUE) +
  scale_color_viridis()

autoplot(pca.annual, data = env.annual.plot, colour = 'reco.sum',
         loadings = TRUE, loadings.label = TRUE) +
  scale_color_viridis()

### The following are just exploratory
### PCA without permafrost thaw related factors
# pca.annual.env <- prcomp(as.matrix(select(env.annual.subset, -c(subsidence, alt, tp))))
# saveRDS(pca.annual.env,
#         '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/env_pca_no_thaw.rds')
pca.annual.env <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/env_pca_no_thaw.rds')

autoplot(pca.annual.env, data = env.annual.plot, colour = 'plot.id',
         loadings = TRUE, loadings.label = TRUE) +
  scale_color_viridis(discrete = TRUE)

autoplot(pca.annual.env, data = env.annual.plot, colour = 'treatment',
         loadings = TRUE, loadings.label = TRUE) +
  scale_color_manual(breaks = c('Control', 'Air Warming', 'Soil Warming', 'Air + Soil Warming'),
                     values = c("#0099cc", '#009900', "#990000", '#330000'))

autoplot(pca.annual.env, data = env.annual.plot, colour = 'flux.year',
         loadings = TRUE, loadings.label = TRUE) +
  scale_color_viridis(discrete = TRUE)

autoplot(pca.annual.env, data = env.annual.plot, colour = 'nee.sum',
         loadings = TRUE, loadings.label = TRUE) +
  scale_color_viridis()

autoplot(pca.annual.env, data = env.annual.plot, colour = 'gpp.sum',
         loadings = TRUE, loadings.label = TRUE) +
  scale_color_viridis()

autoplot(pca.annual.env, data = env.annual.plot, colour = 'reco.sum',
         loadings = TRUE, loadings.label = TRUE) +
  scale_color_viridis()

### PCA without permafrost thaw related factors (control only to get interannual variation)
env.annual.control <- env.annual.plot %>%
  filter(treatment == 'Control') %>%
  select(-c(flux.year, block, fence, plot, plot.id, treatment, matches('sum')))
env.annual.subset.control <- env.annual.control %>%
  select(tair.max = max.tair.max, tair.mean, tair.spread.mean = mean.tair.spread, 
         t5.max = max.t5.max, t5.mean, 
         t10.max = max.t10.max, t10.mean, 
         t20.max = max.t20.max, t20.mean, 
         t40.max = max.t40.max, t40.mean, 
         vwc.max = max.vwc.max, vwc.mean, vwc.min = min.vwc.min,
         gwc.max = max.gwc.max, gwc.mean, gwc.min = min.gwc.min, 
         wtd.mean, w.snow.depth = winter.snow.depth, 
         w.t5.min = winter.min.t5.min, w.t10.min = winter.min.t10.min, 
         w.t20.min = winter.min.t20.min, w.t40.min = winter.min.t40.min)
# pca.annual.control <- prcomp(as.matrix(env.annual.subset.control))
# saveRDS(pca.annual.control,
#         '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/env_pca_control.rds')
pca.annual.control <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/env_pca_control.rds')

autoplot(pca.annual.control, data = env.annual.plot %>%
           filter(treatment == 'Control'), colour = 'plot.id',
         loadings = TRUE, loadings.label = TRUE) +
  scale_color_viridis(discrete = TRUE)

autoplot(pca.annual.control, data = env.annual.plot %>%
           filter(treatment == 'Control'), colour = 'flux.year',
         loadings = TRUE, loadings.label = TRUE) +
  scale_color_viridis(discrete = TRUE)

autoplot(pca.annual.control, data = env.annual.plot %>%
           filter(treatment == 'Control'), colour = 'nee.sum',
         loadings = TRUE, loadings.label = TRUE) +
  scale_color_viridis()

autoplot(pca.annual.control, data = env.annual.plot %>%
           filter(treatment == 'Control'), colour = 'gpp.sum',
         loadings = TRUE, loadings.label = TRUE) +
  scale_color_viridis()

autoplot(pca.annual.control, data = env.annual.plot %>%
           filter(treatment == 'Control'), colour = 'reco.sum',
         loadings = TRUE, loadings.label = TRUE) +
  scale_color_viridis()
################################################################################

### Annual Variability #########################################################
# Air temp
ggplot(weather.annual, aes(x = flux.year)) +
  geom_point(aes(y = tair.mean, color = 'Mean Air Temp')) +
  geom_point(aes(y = tair.max, color = 'Max Air Temp')) +
  geom_point(aes(y = tair.min, color = 'Min Air Temp')) +
  scale_x_continuous(breaks = seq(2009, 2020))

# no detectable trend in air temp over time 
# (although there was a non-significant increase in mean and min)
tair.lm <- lm(tair.mean ~ flux.year, 
              data = weather.annual)
summary(tair.lm)

tair.max.lm <- lm(tair.max ~ flux.year, 
              data = weather.annual)
summary(tair.max.lm)

tair.min.lm <- lm(tair.min ~ flux.year, 
              data = weather.annual)
summary(tair.min.lm)

# Unusual years
# Z-scores
weather.annual <- weather.annual %>%
  mutate(tair.mean.z = (tair.mean - mean(tair.mean))/sd(tair.mean),
         tair.min.z = (tair.min - mean(tair.min))/sd(tair.min),
         tair.max.z = (tair.max - mean(tair.max))/sd(tair.max),
         precip.z = (precip - mean(precip))/sd(precip))

annual.temp.precip <- ggplot(weather.annual, aes(x = tair.mean, y = precip, color = tair.min)) +
  geom_hline(aes(yintercept = mean(weather.annual$precip), linetype = 'Mean'), # use this one to create a legend item with a horizontal line only
             size = 0.1) +
  geom_vline(xintercept = mean(weather.annual$tair.mean),
             size = 0.1) + # don't use the linetype in previous line, because it will add a vertical line
  geom_hline(aes(yintercept = mean(weather.annual$precip) + sd(weather.annual$precip),
                 linetype = '1 SD'),
             size = 0.1) +
  geom_vline(xintercept = mean(weather.annual$tair.mean) + sd(weather.annual$tair.mean),
             size = 0.1,
             linetype = 'dashed') +
  geom_hline(yintercept = mean(weather.annual$precip) - sd(weather.annual$precip),
             size = 0.1,
             linetype = 'dashed') +
  geom_vline(xintercept = mean(weather.annual$tair.mean) - sd(weather.annual$tair.mean),
             size = 0.1,
             linetype = 'dashed') +
  geom_point() +
  geom_text(aes(label = flux.year), 
            color = 'black', 
            size = 3,
            position = position_nudge(x = 0, y = 12)) +
  scale_color_viridis(name = expression(# atop('Min Temperature', ~(degree*C))
    'Min Temp' ~ (degree*C))) +
  scale_linetype_manual(name = NULL,
                        breaks = c('Mean', '1 SD'),
                        values = c('solid', 'dashed')) +
  scale_x_continuous(name = expression('Mean Temp' ~ (degree*C))) +
  scale_y_continuous(name = expression('Precip (mm)')) +
  theme_bw()
annual.temp.precip
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/annual_temp_precip.jpg',
#        annual.temp.precip,
#        height = 4,
#        width = 5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/annual_temp_precip.pdf',
#        annual.temp.precip,
#        height = 4,
#        width = 5)

### Coldest and Warmest 3 years
# mean air temp
tair.mean.q <- quantile(weather.annual$tair.mean, probs = c(0.25, 0.75))
tair.mean.cold <- subset(weather.annual, 
                             tair.mean < 3*sd(tair.mean))$flux.year
tair.mean.warm <- subset(weather.annual, 
                                  tair.mean > tair.mean.q[2])$flux.year

# min air temp
tair.min.q <- quantile(weather.annual$tair.min, probs = c(0.25, 0.75))
tair.min.outliers.cold <- subset(weather.annual, 
                                  tair.min < tair.min.q[1])$flux.year
tair.min.outliers.warm <- subset(weather.annual, 
                                  tair.min > tair.min.q[2])$flux.year

# max air temp
tair.max.q <- quantile(weather.annual$tair.max, probs = c(0.25, 0.75))
tair.max.outliers.cold <- subset(weather.annual, 
                                 tair.max < tair.max.q[1])$flux.year
tair.max.outliers.warm <- subset(weather.annual, 
                                 tair.max > tair.max.q[2])$flux.year

### Unusual Years with Seasonal Information
snow.depth.control <- snow.depth %>%
  filter(exp == 'CiPEHR' & treatment == 'c') %>%
  mutate(flux.year = year,
         snow.free = yday(parse_date_time(date, orders = c('m!/d!/Y!')))) %>%
  group_by(flux.year) %>%
  summarise(snow.depth = mean(snow.depth, na.rm = TRUE),
            snow.free.date = mean(snow.free, na.rm = TRUE))

weather.seasonal.wide <- weather.seasonal %>%
  select(-c(tair.min, tair.max)) %>%
  pivot_wider(names_from = 'season', 
              values_from = c(tair.mean:par),
              names_sep = '.') %>%
  select(-precip.ngs) %>%
  full_join(snow.depth.control, by = 'flux.year') %>%
  mutate(across(tair.mean.ngs:snow.free.date, 
                ~(.x - mean(.x))/sd(.x),
                .names = '{col}.z'))

weather.seasonal.control.z <- weather.seasonal.wide %>%
  select(flux.year, contains('z')) %>%
  pivot_longer(tair.mean.ngs.z:snow.free.date.z, names_to = 'measurement', values_to = 'z.score') %>%
  mutate(flux.year = factor(flux.year),
         measurement = factor(str_sub(measurement, 1, -3),
                              levels = c('tair.mean.ngs',
                                         'snow.depth',
                                         'par.ngs',
                                         'snow.free.date',
                                         'tair.mean.gs',
                                         'precip.gs',
                                         'par.gs')),
         season = factor(case_when(str_detect(measurement, 'ngs') ~ 'NGS',
                            str_detect(measurement, 'depth') ~ 'NGS',
                            str_detect(measurement, 'gs') ~ 'GS'),
                         levels = c('NGS',
                                    'GS')),
         measurement.type = factor(case_when(str_detect(measurement, 'tair') ~ 'Air Temp',
                                             str_detect(measurement, 'snow.depth') | str_detect(measurement, 'precip') ~ 'Precip/Snow Depth',
                                             str_detect(measurement, 'par') ~ 'PAR',
                                             str_detect(measurement, 'date') ~ 'Snow Free Date'),
                                   levels = c('Air Temp',
                                              'Precip/Snow Depth',
                                              'PAR',
                                              'Snow Free Date')))

weather.seasonal.control <- weather.seasonal.wide %>%
  select(-contains('z')) %>%
  pivot_longer(tair.mean.ngs:snow.free.date, names_to = 'measurement', values_to = 'value') %>%
  mutate(flux.year = factor(flux.year),
         measurement = factor(measurement,
                              levels = c('tair.mean.ngs',
                                         'snow.depth',
                                         'par.ngs',
                                         'snow.free.date',
                                         'tair.mean.gs',
                                         'precip.gs',
                                         'par.gs')),
         season = factor(case_when(str_detect(measurement, 'ngs') ~ 'NGS',
                                   str_detect(measurement, 'depth') ~ 'NGS',
                                   str_detect(measurement, 'gs') ~ 'GS'),
                         levels = c('NGS',
                                    'GS')),
         measurement.type = factor(case_when(str_detect(measurement, 'tair') ~ 'Air Temp',
                                             str_detect(measurement, 'snow.depth') | str_detect(measurement, 'precip') ~ 'Precip/Snow Depth',
                                             str_detect(measurement, 'par') ~ 'PAR',
                                             str_detect(measurement, 'date') ~ 'Snow Free Date'),
                                   levels = c('Air Temp',
                                              'Precip/Snow Depth',
                                              'PAR',
                                              'Snow Free Date'))) %>%
  full_join(weather.seasonal.control.z, by = c('flux.year', 'measurement', 'measurement.type', 'season'))
rm(weather.seasonal.control.z)

weather.seasonal.plot <- ggplot(weather.seasonal.control, 
       aes(x = flux.year, 
           y = z.score, 
           # color = measurement.type, 
           fill = measurement.type,
           group = measurement,
           alpha = season),
       color = NULL) +
  geom_hline(yintercept = 0, size = 0.1) +
  geom_col(position = position_dodge(width = 0.6), width = 0.4) +
  # scale_color_manual(values = c('#990000',
  #                               '#006666',
  #                               '#CC9900',
  #                               '#66CC00')) +
  scale_fill_manual(name = 'Measurement',
                    values = c('#990000',
                               '#006666',
                               '#CC9900',
                               '#66CC00')) +
  scale_alpha_manual(name = 'Season',
                     values = c(0.4, 1),
                     na.translate = FALSE) +
  scale_y_continuous(name = 'Normalized Value (Z-Score)') +
  facet_grid(measurement.type ~ .) +
  theme_bw() +
  theme(axis.title.x = element_blank())
weather.seasonal.plot
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/seasonal_weather.jpg',
#        weather.seasonal.plot,
#        height = 6.5,
#        width = 6.5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/seasonal_weather.pdf',
#        weather.seasonal.plot,
#        height = 6.5,
#        width = 6.5)

### Create a table of environmental variables by year
env.summary <- weather.seasonal[, .(flux.year, group = season, tair.mean, par, precip)]
env.summary[, ':=' (tair.mean = round(tair.mean, 2),
                    par = round(par, 2),
                    precip = round(precip, 2))]
env.summary <- melt(env.summary, 
                    measure.vars = c('tair.mean', 'par', 'precip'), 
                    variable.name = 'measurement')
env.summary <- dcast(env.summary, measurement + group ~ flux.year)

# Add snow depth
snow.depth <- flux.annual %>%
  mutate(group = treatment,
         measurement = 'snow.depth') %>%
  group_by(flux.year, measurement, group) %>%
  summarise(snow.depth.mean = round(mean(winter.snow.depth, na.rm = TRUE), 2)) %>%
  ungroup() %>%
  pivot_wider(names_from = 'flux.year',
              values_from = 'snow.depth.mean')

env.summary <- rbind(env.summary, snow.depth, fill = TRUE)

# Add snow free date
snow.free.date <- snow.free %>%
  mutate(group = treatment,
         measurement = 'snow.free.date') %>%
  group_by(flux.year, measurement, group) %>%
  summarise(doy.snow.free = round(mean(doy.snow.free, na.rm = TRUE))) %>%
  ungroup() %>%
  pivot_wider(names_from = 'flux.year',
              values_from = 'doy.snow.free')

env.summary <- rbind(env.summary, snow.free.date, fill = TRUE)


################################################################################

### Calculate Microtopography ##################################################
# For all plots in all years
elev <- list(brick('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/AElevStack_filled_unclipped.tif'),
             brick('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/BElevStack_filled_unclipped.tif'),
             brick('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/CElevStack_filled_unclipped.tif'))
elev.clipped <- list(brick('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/AElevStack_filled_clipped.tif'),
                     brick('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/BElevStack_filled_clipped.tif'),
                     brick('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/CElevStack_filled_clipped.tif'))
plots <- st_read('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/plot_coordinates_from_2017.shp') %>%
  filter(exp == 'CiPEHR')

# need to clip the 2009-2016 unclipped data
for (i in 1:length(elev.clipped)) {
  
  for (j in 1:8) { # 2009 - 2016
    elev[[i]][[j]] <- mask(elev[[i]][[j]], elev.clipped[[i]][[j]])
  }
  
}

weights <- list()
elev.med <- list()
mtopo <- list()
for (i in 1:length(elev)) {
  
  # neighborhood window weights
  weights[[i]] <- focalWeight(elev[[i]], 5, type = 'circle')
  weights[[i]][which(weights[[1]] > 0)] <- 1
  
  # prepare output data format
  elev.med[[i]] <- list()
  mtopo[[i]] <- list()
  
  for (j in 1:nlayers(elev[[i]])) {
    
    elev.med[[i]][[j]] <- focal(elev[[i]][[j]], 
                                weights[[i]], 
                                fun = median,
                                na.rm = TRUE)
    
    mtopo[[i]][[j]] <- elev[[i]][[j]] - elev.med[[i]][[j]]
    
  }
  
  elev.med[[i]] <- brick(elev.med[[i]])
  mtopo[[i]] <- brick(mtopo[[i]])
  
}

# extract microtopography values at all plots in all years
new.names <- c(paste0('layer.', seq(1, 12)))
names(new.names) <- paste0('mtopo.', seq(2009, 2020))
mtopo.df <- data.frame()
for (i in 1:length(mtopo)) {
  mtopo.extract <- raster::extract(mtopo[[i]], plots, df = TRUE) %>%
    filter(!(rowSums(is.na(.)) == ncol(.) - 1)) %>%
    rename(new.names)
  
  mtopo.df <- rbind.data.frame(mtopo.df, mtopo.extract)
}

plot.mtopo <- plots %>%
  cbind.data.frame(mtopo.df) %>%
  pivot_longer(names(new.names), names_to = 'flux.year', values_to = 'mtopo') %>%
  mutate(flux.year = as.factor(as.numeric(str_extract(flux.year, '[:digit:]+'))),
         plot.id = paste(fence, plot, sep = '_')) %>%
  select(flux.year, plot.id, mtopo)

flux.annual <- flux.annual %>%
  left_join(plot.mtopo, by = c('flux.year', 'plot.id'))

ggplot(flux.annual, aes(x = subsidence.annual, y = mtopo)) +
  geom_point()
################################################################################

### Impact of Subsidence on Soil Moisture ######################################
# Need to add in models!
sub.moisture <- flux.annual %>%
  mutate(treatment = factor(treatment,
                            levels = c('Control',
                                       'Air Warming',
                                       'Soil Warming',
                                       'Air + Soil Warming')),
         subsidence = subsidence.annual*-1,
         wtd.mean = wtd.mean*-1) %>%
  select(flux.year, plot.id, treatment, subsidence, mtopo, wtd.mean, wtd.sd, wtd.n, 
         vwc.mean, vwc.sd, gwc.mean, gwc.sd) %>%
  full_join(weather.annual %>%
              select(flux.year, precip.z) %>%
              mutate(flux.year = factor(flux.year)),
            by = 'flux.year') %>%
  mutate(precip.group = factor(case_when(precip.z >= 0.75 ~ 'wet',
                                  precip.z > -0.75 ~ 'average',
                                  precip.z <= -0.75 ~ 'dry'),
                               levels = c('dry', 'average', 'wet')),
         sub.group = factor(case_when(subsidence < 15 ~ 'Low Subsidence',
                               subsidence < 30 ~ 'Moderate Subsidence',
                               subsidence < 45 ~ 'High Subsidence',
                               subsidence >= 45 ~ 'Very High Subsidence'),
                            levels = c('Low Subsidence', 'Moderate Subsidence', 
                                       'High Subsidence', 'Very High Subsidence')))

# WTD
wtd.lm <- lm(wtd.mean ~ subsidence, data = subset(sub.moisture, !is.na(wtd.sd)))
step(wtd.lm)
summary(wtd.lm)
wtd.r2.label <- paste0(as.character(expression('R'^2 ~ ' = ')), ' ~ ', round(summary(wtd.lm)$r.squared, 2))

wtd.plot <- ggplot(subset(sub.moisture, !is.na(wtd.sd)),
       aes(x = subsidence, y = wtd.mean)) +
  geom_hline(yintercept = 0, size = 0.1) +
  geom_point(aes(color = flux.year, shape = treatment)) +
  geom_smooth(method = 'lm', color = 'black') +
  geom_text(aes(x = 100, y = -35, 
                label = wtd.r2.label),
            parse = TRUE,
            hjust = 'inward') +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'WTD (cm)') +
  theme_bw() +
  theme(legend.title = element_blank())
# there is high variability in wtd when magnitude of subsidence is similar to 
# pre-subsidence wtd
wtd.sd.plot <- ggplot(subset(sub.moisture, !is.na(wtd.sd)),
       aes(x = subsidence, y = wtd.sd)) +
  geom_point(aes(color = flux.year, shape = treatment)) +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'WTD SD (cm)') +
  theme_bw() +
  theme(legend.title = element_blank())

# there is high variability in wtd when magnitude of subsidence is similar to 
# pre-subsidence wtd (using 2010, because precipitation was closer to average 
# and subsidence was still very nearly 0)
# that can't explain all of it, though
# microtopography doesn't seem to explain any more of it
test <- sub.moisture %>%
  filter(flux.year == 2010) %>%
  select(plot.id, wtd.mean.2010 = wtd.mean, wtd.sd.2010 = wtd.sd) %>%
  mutate(wtd.1sd.upr = (wtd.mean.2010 - wtd.sd.2010*3)*-1,
         wtd.1sd.lwr = (wtd.mean.2010 + wtd.sd.2010*3)*-1) %>%
  full_join(sub.moisture, by = 'plot.id') %>%
  mutate(sub.wtd.mag = case_when(subsidence > wtd.1sd.lwr & subsidence < wtd.1sd.upr ~ 0,
                                 subsidence < wtd.1sd.lwr | subsidence > wtd.1sd.upr ~ subsidence - wtd.1sd.lwr))
ggplot(subset(test, !is.na(wtd.sd)),
       aes(x = subsidence, y = wtd.sd)) +
  geom_point(aes(color = sub.wtd.mag, shape = treatment)) +
  geom_text(data = subset(sub.moisture, wtd.sd > 10), 
            aes(label = plot.id),
            nudge_y = 0.25,
            size = 3) +
  scale_color_viridis(direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'WTD SD (cm)') +
  theme_bw() +
  theme(legend.title = element_blank())

ggplot(subset(test, !is.na(wtd.sd)),
       aes(x = mtopo, y = wtd.sd)) +
  geom_point(aes(color = subsidence, shape = treatment)) +
  geom_text(data = subset(sub.moisture, wtd.sd > 10), 
            aes(label = plot.id),
            nudge_y = 0.25,
            size = 3) +
  scale_color_viridis(direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Microtopography (cm)') +
  scale_y_continuous(name = 'WTD SD (cm)') +
  theme_bw() +
  theme(legend.title = element_blank())

# VWC
vwc.lm <- lm(vwc.mean ~ subsidence, data = subset(sub.moisture, !is.na(vwc.sd)))
step(vwc.lm)
summary(vwc.lm)
vwc.r2.label <- paste0(as.character(expression('R'^2 ~ ' = ')), ' ~ ', round(summary(vwc.lm)$r.squared, 2))

vwc.plot <- ggplot(sub.moisture, aes(x = subsidence, y = vwc.mean)) +
  geom_point(aes(color = flux.year, shape = treatment)) +
  geom_smooth(method = 'lm', color = 'black') +
  geom_text(aes(x = 100, y = 32, 
                label = vwc.r2.label),
            parse = TRUE,
            hjust = 'inward') +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'VWC (%)') +
  theme_bw() +
  theme(legend.title = element_blank())
vwc.sd.plot <- ggplot(sub.moisture,
       aes(x = subsidence, y = vwc.sd, color = flux.year, shape = treatment)) +
  geom_point() +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'VWC SD (%)') +
  theme_bw() +
  theme(legend.title = element_blank())

# GWC
gwc.plot <- ggplot(sub.moisture,
       aes(x = subsidence, y = gwc.mean, color = flux.year, shape = treatment)) +
  geom_point() +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'GWC (%)') +
  theme_bw() +
  theme(legend.title = element_blank())
gwc.sd.plot <- ggplot(sub.moisture,
       aes(x = subsidence, y = gwc.sd, color = flux.year, shape = treatment)) +
  geom_point() +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'GWC SD (%)') +
  theme_bw() +
  theme(legend.title = element_blank())

### Combine into a single plot
wtd.plot
wtd.sd.plot
vwc.plot
vwc.sd.plot
gwc.plot
gwc.sd.plot
sub.moisture.plot <- ggarrange(wtd.plot,
                               wtd.sd.plot,
                               vwc.plot,
                               vwc.sd.plot,
                               gwc.plot,
                               gwc.sd.plot,
                               ncol = 2,
                               nrow = 3,
                               common.legend = TRUE,
                               legend = 'right',
                               labels = LETTERS[1:6])
sub.moisture.plot

# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/subsidence_moisture_linear_models.jpg',
#        sub.moisture.plot,
#        height = 7,
#        width = 6.5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/subsidence_moisture_linear_models.pdf',
#        sub.moisture.plot,
#        height = 7,
#        width = 6.5)

# Plot relationship between WTD and subsidence by wet vs. dry vs. avg years
ggplot(subset(sub.moisture, !is.na(wtd.sd)),
       aes(x = subsidence, y = wtd.mean)) +
  geom_hline(yintercept = 0, size = 0.1) +
  geom_point(aes(color = flux.year, shape = treatment)) +
  geom_smooth(method = 'lm', color = 'black') +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'WTD (cm)') +
  facet_grid(. ~ precip.group) +
  theme_bw() +
  theme(legend.title = element_blank())

# precipitation total impacts wtd less in highly subsided plots
# basically, highly subsided plots are wet all the time, while less
# subsided plots are dry when it's dry and wet when it's wet
wtd.precip <- ggplot(subset(sub.moisture, !is.na(wtd.sd)),
       aes(x = precip.z, y = wtd.mean)) +
  geom_hline(yintercept = 0, size = 0.1) +
  geom_point(aes(color = flux.year, shape = treatment)) +
  geom_smooth(method = 'lm', color = 'black') +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Precip Z-score') +
  scale_y_continuous(name = 'WTD (cm)') +
  facet_grid(. ~ sub.group) +
  theme_bw() +
  theme(legend.title = element_blank())
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/wtd_precip_sub.jpg',
#        wtd.precip,
#        height = 5,
#        width = 6.5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/wtd_precip_sub.pdf',
#        wtd.precip,
#        height = 5,
#        width = 6.5)
################################################################################
