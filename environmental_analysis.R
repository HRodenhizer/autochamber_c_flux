################################################################################
###                        Environmental Data Analysis                       ###
###                             code by HGR 7/2020                           ###
################################################################################

### Load Libraries #############################################################
library(data.table)
library(lubridate)
library(readxl)
library(ggfortify)
library(viridis)
library(emmeans)
library(tidyverse)
################################################################################

### Load Data ##################################################################
flux.monthly <- read.csv("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_monthly.csv") %>%
  filter(flux.year >= 2010)
flux.annual <- read.csv("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_annual.csv") %>%
  filter(flux.year >= 2010) %>%
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
                     tair.max = max(Tair, na.rm = TRUE)),
                   by = .(flux.year)]

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
  mutate(subsidence = -1*subsidence.annual) %>%
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
# mean air temp
tair.mean.q <- quantile(weather.annual$tair.mean, probs = c(0.25, 0.75))
tair.mean.outliers.cold <- subset(weather.annual, 
                             tair.mean < tair.mean.q[1])$flux.year
tair.mean.outliers.warm <- subset(weather.annual, 
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
