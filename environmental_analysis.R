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
library(lme4)
library(lmerTest)
library(emmeans)
library(MuMIn)
library(mgcv)
library(raster)
library(sf)
library(gstat)
library(stars)
library(automap)
library(ggnewscale)
library(tidyverse)
################################################################################

### Load Data ##################################################################
flux.daily <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_daily.csv")
flux.monthly <- read.csv("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_monthly.csv")
flux.annual <- read.csv("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_annual.csv") %>%
  mutate(flux.year = as.factor(flux.year))
filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/weather/',
                        pattern = 'csv$',
                        full.names = TRUE)
# weather <- fread('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/hobo_half_hourly_gap_filled.csv')
filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/weather/hourly',
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
snow.depth <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/snow_depth/plot_snow_depth_2009_2021.csv')

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
rm(snow.free.2009.2016, snow.free.2017, snow.free.2018, snow.free.2019, 
   snow.free.2020, snow.free.2021)
################################################################################

######################## DEFINE FUNCTIONS TO EXTRACT AND GRAPH CI #########################
#Extract the coefficients for the fixed effects from your model, make a dataframe with them called model
extract_ci <- function(x) {coefs<-fixef(x) 
modeldf<-as.data.frame(coefs)
#calculate confidence intervals; merge fixed effects and ci into one dataframe
ci <- confint(x,method="boot",boot.type="norm",level=0.95,nsim=1000)
modelci<-merge(ci,modeldf,by="row.names",all.x=F)
#rename colnames so that they make sense and remove symbols
colnames(modelci)<-c("term","min","max","coefs")
return (modelci)}

# graph CI
graph_ci <- function(ci,figtitle,model) {ggplot(ci,aes(x=names,y=coefs))+
    geom_errorbar(aes(ymin=min,ymax=max),width=0,size=1)+
    geom_point(aes(size=2))+
    labs (title = paste(figtitle, ", AIC:", round(AIC(model),2), sep =" ") , x = "Fixed effect", y = "Effect size and 95% CI") +
    guides(size=F,shape=F)+
    theme_bw()+
    theme(axis.text.x=element_text(size=18),
          axis.title.x=element_text(size=26),
          axis.title.y=element_text(size=26,vjust=1),
          axis.text.y=element_text(size=22),
          panel.grid.minor=element_blank(),
          panel.grid.major.x=element_blank())+
    geom_hline(yintercept=0)+
    coord_flip() } 
#################################################################################

### PCA ########################################################################
# need to finalize which variables to include
env.annual.plot <- flux.annual %>%
  filter(flux.year != 2009) %>%
  select(-c(season, matches('rh'), 
            max.tair.spread, min.tair.spread, matches('ndvi'),
            gdd, fdd, winter.fdd, precip.sum)) %>%
  mutate(flux.year = as.numeric(as.character(flux.year)),
         treatment = factor(treatment, 
                            levels = c('Control', 
                                       'Air Warming', 
                                       'Soil Warming', 
                                       'Air + Soil Warming')),
         subsidence = -1*subsidence.annual,
         wtd.mean = -1*wtd.mean) %>%
  na.omit()
env.annual <- env.annual.plot %>%
  select(-c(flux.year, block, fence, plot, plot.id, treatment, matches('sum')))
env.annual.subset <- env.annual %>%
  select(t5.mean, t5.sd,
         t10.mean, t10.sd,
         t20.mean, t20.sd,
         t40.mean, t40.sd,
         vwc.mean, vwc.sd,
         gwc.mean, gwc.sd, 
         wtd.mean, wtd.sd,
         subsidence, # adding or removing subsidence doesn't change much in pca
         alt, tp, 
         biomass = biomass.annual,
         w.snow.depth = winter.snow.depth, 
         w.t5.min = winter.min.t5.min, 
         w.t10.min = winter.min.t10.min, 
         w.t20.min = winter.min.t20.min, 
         w.t40.min = winter.min.t40.min)
env.annual.subset.norm <- env.annual.subset %>%
  mutate(across(all_of(colnames(.)), ~(.x - mean(.x))/mean(.x)))
# pca.annual.norm <- prcomp(as.matrix(env.annual.subset.norm))
# saveRDS(pca.annual.norm,
#         '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/env_pca_normalized.rds')
pca.annual.norm <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/env_pca_normalized.rds')

# Environmental PCA colored by subsidence
pca.plot.norm <- autoplot(pca.annual.norm, data = env.annual.plot, 
                          colour = 'flux.year', shape = 'treatment', alpha = 0.8,
                     loadings = TRUE, loadings.colour = 'black',
                     loadings.label = TRUE, loadings.label.size = 3,
                     loadings.label.colour = 'black') +
  scale_color_viridis(name = '',
                      direction = -1,
                      breaks = seq(2010, 2020, by = 2)) +
  scale_shape_manual(name = '',
                     values = c(1, 0, 16, 15)) +
  coord_fixed() +
  theme_bw() +
  coord_cartesian()
pca.plot.norm
# zoom in on the center mass of red
pca.plot.norm.zoom <- autoplot(pca.annual.norm, data = env.annual.plot, 
                               colour = 'flux.year', shape = 'treatment', alpha = 0.8,
                          loadings = TRUE, loadings.colour = 'black',
                          loadings.label = TRUE, loadings.label.size = 3,
                          loadings.label.colour = 'black') +
  scale_color_viridis(name = '',
                      direction = -1,
                      breaks = seq(2010, 2020, by = 2)) +
  scale_shape_manual(name = '',
                     values = c(1, 0, 16, 15)) +
  scale_y_continuous(limits = c(-0.04, 0.04)) +
  scale_x_continuous(limits = c(-0.025, 0.025)) +
  coord_fixed() +
  theme_bw()
pca.plot.norm.zoom
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/environmental_pca.jpg',
#        pca.plot.norm,
#        height = 6,
#        width = 6)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/environmental_pca.pdf',
#        pca.plot.norm,
#        height = 6,
#        width = 6)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/environmental_pca_zoom.jpg',
#        pca.plot.norm.zoom,
#        height = 6,
#        width = 6)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/environmental_pca_zoom.pdf',
#        pca.plot.norm.zoom,
#        height = 6,
#        width = 6)

autoplot(pca.annual.norm, data = env.annual.plot, colour = 'plot.id',
         loadings = TRUE, loadings.label = TRUE) +
  scale_color_viridis(discrete = TRUE)

autoplot(pca.annual.norm, data = env.annual.plot, colour = 'treatment',
         loadings = TRUE, loadings.label = TRUE) +
  scale_color_manual(breaks = c('Control', 'Air Warming', 'Soil Warming', 'Air + Soil Warming'),
                     values = c("#0099cc", '#009900', "#990000", '#330000'))

autoplot(pca.annual.norm, data = env.annual.plot, colour = 'flux.year',
         loadings = TRUE, loadings.label = TRUE) +
  scale_color_viridis(discrete = TRUE)

autoplot(pca.annual.norm, data = env.annual.plot, colour = 'nee.sum',
         loadings = TRUE, loadings.label = TRUE) +
  scale_color_viridis()

autoplot(pca.annual.norm, data = env.annual.plot, colour = 'gpp.sum',
         loadings = TRUE, loadings.label = TRUE) +
  scale_color_viridis()

autoplot(pca.annual.norm, data = env.annual.plot, colour = 'reco.sum',
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

### Test if there's a detectable trend in air temp
lm.tair <- lm(tair.mean ~ I(flux.year-2009), data = weather.annual)
summary(lm.tair)
ggplot(weather.annual, aes(x = flux.year, y = tair.mean)) +
  geom_point() +
  geom_smooth(method = 'lm',
              color = 'black')

# no detectable trend in air temp over time 
# (although there was a non-significant increase in mean and min)
tair.max.lm <- lm(tair.max ~ I(flux.year-2009), 
              data = weather.annual)
summary(tair.max.lm)

tair.min.lm <- lm(tair.min ~ I(flux.year-2009), 
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
  full_join(select(weather.annual, flux.year, precip), by = 'flux.year') %>%
  mutate(ngs.precip.percent = precip.ngs/precip) %>%
  full_join(snow.depth.control, by = 'flux.year') %>%
  mutate(across(tair.mean.ngs:snow.free.date, 
                ~(.x - mean(.x))/sd(.x),
                .names = '{col}.z'))

mean.temp <- mean(weather.annual$tair.mean)
mean.temp.gs <- mean(weather.seasonal.wide$tair.mean.gs)
mean.temp.ngs <- mean(weather.seasonal.wide$tair.mean.ngs)
mean.precip <- mean(weather.annual$precip)

weather.seasonal.control.z <- weather.seasonal.wide %>%
  select(flux.year, contains('z')) %>%
  select(-c(precip.gs.z, precip.ngs.z, ngs.precip.percent.z)) %>%
  rename(precip.gs.z = precip.z) %>% # use total annual rainfall rather than May-Oct rainfall
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
  select(-c(contains('z'), precip.gs, precip.ngs, ngs.precip.percent)) %>%
  rename(precip.gs = precip) %>%
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

# write.csv(env.summary, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/tables/environmental_summary.csv',
#           row.names = FALSE)
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
  geom_point(aes(color = treatment))
ggplot(subset(flux.annual, flux.year == 2020), aes(x = subsidence.annual, y = mtopo)) +
  geom_point(aes(color = plot.id, shape = factor(treatment,
                                                 levels = c('Control',
                                                            'Air Warming',
                                                            'Soil Warming',
                                                            'Air + Soil Warming')))) +
  geom_text(aes(label = plot.id), 
            hjust = 1.5) +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  facet_grid(block~.)
################################################################################

### Impact of Subsidence on Soil Moisture (Interannual) ########################
# Random effects:
# by plot (nested in treatment, nested in fence, nested in block) allows for
# original differences in microtopography and how that might impact the response
# of wtd to subsidence (a relatively high plot could subside a lot but not see
# much difference in wtd for awhile)
#
# will not include a random effect for year, because  this causes singular fit
# and differences in measurements or precipitation across years can be included
# in the residuals

sub.moisture <- flux.annual %>%
  mutate(treatment = factor(treatment,
                            levels = c('Control',
                                       'Air Warming',
                                       'Soil Warming',
                                       'Air + Soil Warming')),
         subsidence = subsidence.annual*-1,
         wtd.mean = wtd.mean*-1) %>%
  select(flux.year, fence, plot, plot.id, treatment, subsidence, # mtopo, 
         wtd.mean, wtd.sd, wtd.n, 
         vwc.mean, vwc.sd, gwc.mean, gwc.sd) %>%
  full_join(weather.annual %>%
              select(flux.year, precip, precip.z) %>%
              mutate(flux.year = factor(flux.year)),
            by = 'flux.year') %>%
  mutate(precip = precip/10, # convert to cm
         precip.group = factor(case_when(precip.z >= 0.75 ~ 'wet',
                                  precip.z > -0.75 ~ 'average',
                                  precip.z <= -0.75 ~ 'dry'),
                               levels = c('dry', 'average', 'wet')),
         sub.group = factor(case_when(subsidence < 15 ~ '<15 cm Subsidence',
                               subsidence < 30 ~ '15-30 cm Subsidence',
                               subsidence < 45 ~ '30-45 cm Subsidence',
                               subsidence >= 45 ~ '>=45 cm Subsidence'),
                            levels = c('<15 cm Subsidence', '15-30 cm Subsidence', 
                                       '30-45 cm Subsidence', '>=45 cm Subsidence')),
         time = factor(as.numeric(flux.year)),
         fence = as.factor(fence),
         block.f = as.factor(case_when(fence %in% c(1, 2) ~ 1,
                                       fence %in% c(3, 4) ~ 2,
                                       fence %in% c(5, 6) ~ 3)),
         fence.f = as.factor(case_when(fence %in% c(1, 3, 5) ~ 1,
                                       fence %in% c(2, 4, 6) ~ 2)),
         treatment.f = as.factor(case_when(treatment == 'Control' ~ 1,
                                           treatment == 'Air Warming' ~ 2,
                                           treatment == 'Soil Warming' ~ 3,
                                           treatment == 'Air + Soil Warming' ~ 4)),
         replicate.f = as.factor(case_when(plot %in% c(1, 2, 5, 6) ~ 1,
                                           plot %in% c(3, 4, 7, 8) ~ 2)))

### WTD
# ### Model wtd with subsidence
# model1 <- lmer(wtd.mean ~ 1 +
#                  (1 | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model1)
# 
# model2 <- lmer(wtd.mean ~ subsidence +
#                  (1 | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model2)
# 
# model3 <- lmer(wtd.mean ~ subsidence +
#                  (1 + subsidence | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model3)
# 
# model4 <- lmer(wtd.mean ~ subsidence + I(subsidence^2) +
#                  (1 + subsidence | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model4)
# 
# AIC(model1, model2, model3, model4)
# 
# ranova(model4)
# 
# # check model residuals of model4
# # look at residuals
# model4.resid <- resid(model4)
# model4.fitted <- fitted(model4)
# model4.sqrt <- sqrt(abs(resid(model4)))
# 
# # graph
# par(mfrow=c(2,2), mar = c(4,4,3,2))
# plot(model4.fitted, model4.resid, main='resid, model4')
# plot(model4.fitted, model4.sqrt, main='sqrt resid, model4')
# qqnorm(model4.resid, main = 'model4')
# qqline(model4.resid)
# par(mfrow=c(1,1))
# 
# hist(sub.moisture$wtd.mean)

# # re-run with REML = TRUE
# wtd.model <- lmer(wtd.mean ~ subsidence + I(subsidence^2) +
#                  (1 + subsidence | block.f/fence.f/treatment.f/replicate.f), REML = TRUE,
#                      data = sub.moisture,
#                      control=lmerControl(check.conv.singular="warning"))
# summary(wtd.model)
# saveRDS(wtd.model,
#         '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/wtd_model.rds')
wtd.model <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/wtd_model.rds')
summary(wtd.model)

# # calculate confidence intervals to look at fixed effects
# wtd.model.ci <- extract_ci(wtd.model)
# # write.csv(wtd.model.ci, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/wtd_coefficients.csv', row.names = FALSE)
wtd.model.ci <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/wtd_coefficients.csv')

wtd.model.r2 <- r.squaredGLMM(wtd.model)
wtd.r2.label1 <- paste0(as.character(expression('R'^2 ~ 'm = ')), ' ~ ', round(wtd.model.r2[1], 2))
wtd.r2.label2 <- paste0(as.character(expression('R'^2 ~ 'c = ')), ' ~ ', round(wtd.model.r2[2], 2))

# # make confidence interval data frame for graphing
# wtd.model.fit <- expand.grid(subsidence = round(min(sub.moisture$subsidence)):round(max(sub.moisture$subsidence)))
# 
# myStats <- function(model){
#   out <- predict( model, newdata=wtd.model.fit, re.form=~0 )
#   return(out)
# }
# 
# bootObj <- bootMer(wtd.model, FUN=myStats, nsim = 1000)
# wtd.model.fit <- cbind(wtd.model.fit, predict(wtd.model, newdata=wtd.model.fit, re.form=~0 )) %>%
#   cbind(confint( bootObj,  level=0.95 ))
# colnames(wtd.model.fit) <- c('subsidence', 'fit', 'lwr', 'upr')
# # write.csv(wtd.model.fit, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/wtd_model_fit.csv', row.names = FALSE)
wtd.model.fit <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/wtd_model_fit.csv')

sub.moisture <- sub.moisture %>%
  mutate(wtd.fit = ifelse(!is.na(wtd.mean),
                             predict(wtd.model),
                             NA))
ggplot(subset(sub.moisture, !is.na(wtd.sd)),
       aes(x = subsidence, y = wtd.mean)) +
  geom_hline(yintercept = 0, size = 0.1) +
  geom_point(aes(color = flux.year, shape = treatment)) +
  geom_line(aes(x = subsidence, y = wtd.fit), color = 'black') +
  geom_line(data = wtd.model.fit, aes(x = subsidence, y = fit), color = 'red') +
  geom_text(aes(x = 100, y = -35,
                label = wtd.r2.label),
            parse = TRUE,
            hjust = 'inward') +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'WTD (cm)') +
  facet_grid(fence ~ plot) +
  theme_bw() +
  theme(legend.title = element_blank())

# ggplot(subset(sub.moisture, !is.na(wtd.sd)),
#        aes(x = mtopo, y = wtd.mean)) +
#   geom_hline(yintercept = 0, size = 0.1) +
#   geom_point(aes(color = flux.year, shape = treatment)) +
#   scale_color_viridis(discrete = TRUE,
#                       direction = -1) +
#   scale_shape_manual(values = c(1, 0, 16, 15)) +
#   scale_x_continuous(name = 'Microtopography (m)') +
#   scale_y_continuous(name = 'WTD (cm)') +
#   facet_wrap(.~plot.id, ncol = 8) +
#   theme_bw() +
#   theme(legend.title = element_blank())

wtd.plot <- ggplot(subset(sub.moisture, !is.na(wtd.sd)),
       aes(x = subsidence, y = wtd.mean)) +
  geom_hline(yintercept = 0, size = 0.1) +
  geom_point(aes(color = flux.year, shape = treatment)) +
  geom_ribbon(data = wtd.model.fit, aes(x = subsidence, ymin = lwr, ymax = upr), 
              inherit.aes = FALSE,
              fill = 'gray', 
              alpha = 0.5) +
  geom_line(data = wtd.model.fit, aes(x = subsidence, y = fit), color = 'black') +
  geom_text(aes(x = 111, y = -29.5, 
                label = wtd.r2.label1),
            inherit.aes = FALSE,
            parse = TRUE,
            hjust = 'inward',
            size = 3.5) +
  geom_text(aes(x = 111, y = -35, 
                label = wtd.r2.label2),
            inherit.aes = FALSE,
            parse = TRUE,
            hjust = 'inward',
            size = 3.5) +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'WTD (cm)') +
  theme_bw() +
  theme(legend.title = element_blank())
wtd.plot

# there is high variability in wtd when magnitude of subsidence is similar to 
# pre-subsidence wtd
## Model wtd.sd with precipitation and subsidence
# model1 <- lmer(I(log(wtd.sd)) ~ 1 +
#                  (1 | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model1)
# 
# model2 <- lmer(I(log(wtd.sd)) ~ subsidence +
#                  (1 + subsidence | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model2)
# 
# model3 <- lmer(I(log(wtd.sd)) ~ subsidence +
#                  (1 | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model3)
# 
# model4 <- lmer(I(log(wtd.sd)) ~ subsidence + I(subsidence^2) +
#                  (1 + subsidence | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model4)
# 
# AIC(model1, model2, model3, model4)
# 
# # check model residuals of model3
# # look at residuals
# model3.resid <- resid(model3)
# model3.fitted <- fitted(model3)
# model3.sqrt <- sqrt(abs(resid(model3)))
# 
# # graph
# par(mfrow=c(2,2), mar = c(4,4,3,2))
# plot(model3.fitted, model3.resid, main='resid, model3')
# plot(model3.fitted, model3.sqrt, main='sqrt resid, model3')
# qqnorm(model3.resid, main = 'model3')
# qqline(model3.resid)
# par(mfrow=c(1,1))
# 
# hist(log(sub.moisture$wtd.sd))


# # re-run with REML = TRUE
# wtd.sd.model <- lmer(I(log(wtd.sd)) ~ subsidence +
#                        (1 + subsidence | block.f/fence.f/treatment.f/replicate.f), REML = TRUE,
#                      data = sub.moisture,
#                      control=lmerControl(check.conv.singular="warning"))
# summary(wtd.sd.model)
# saveRDS(wtd.sd.model,
#         '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/wtd_sd_model.rds')
wtd.sd.model <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/wtd_sd_model.rds')
summary(wtd.sd.model)

# # calculate confidence intervals to look at fixed effects
# wtd.sd.model.ci <- extract_ci(wtd.sd.model)
# # write.csv(wtd.sd.model.ci, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/wtd_sd_coefficients.csv', row.names = FALSE)
wtd.sd.model.ci <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/wtd_sd_coefficients.csv')

wtd.sd.model.r2 <- r.squaredGLMM(wtd.sd.model)
wtd.sd.r2.label1 <- paste0(as.character(expression('R'^2 ~ 'm = ')), ' ~ ', round(wtd.sd.model.r2[1], 2))
wtd.sd.r2.label2 <- paste0(as.character(expression('R'^2 ~ 'c = ')), ' ~ ', round(wtd.sd.model.r2[2], 2))

# # make confidence interval data frame for graphing
# wtd.sd.model.fit <- expand.grid(subsidence = round(min(sub.moisture$subsidence)):round(max(sub.moisture$subsidence)))
# 
# myStats <- function(model){
#   out <- predict( model, newdata=wtd.sd.model.fit, re.form=~0 )
#   return(out)
# }
# 
# bootObj <- bootMer(wtd.sd.model, FUN=myStats, nsim = 1000)
# wtd.sd.model.fit <- cbind(wtd.sd.model.fit, predict(wtd.sd.model, newdata=wtd.sd.model.fit, re.form=~0 )) %>%
#   cbind(confint( bootObj,  level=0.95 ))
# colnames(wtd.sd.model.fit) <- c('subsidence', 'fit', 'lwr', 'upr')
# # write.csv(wtd.sd.model.fit, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/wtd_sd_model_fit.csv', row.names = FALSE)
wtd.sd.model.fit <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/wtd_sd_model_fit.csv')

sub.moisture <- sub.moisture %>%
  mutate(wtd.sd.fit = ifelse(!is.na(wtd.sd),
                             predict(wtd.sd.model),
                             NA))
ggplot(subset(sub.moisture, !is.na(wtd.sd)),
       aes(x = subsidence, y = log(wtd.sd))) +
  geom_point(aes(color = flux.year, shape = treatment)) +
  geom_line(aes(x = subsidence, y = wtd.sd.fit), color = 'black') +
  geom_line(data = wtd.sd.model.fit, aes(x = subsidence, y = fit), color = 'red') +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'WTD SD (cm)') +
  facet_grid(fence ~ plot) +
  theme_bw() +
  theme(legend.title = element_blank())

wtd.sd.plot <- ggplot(subset(sub.moisture, !is.na(wtd.sd)),
       aes(x = subsidence, y = wtd.sd)) +
  geom_point(aes(color = flux.year, shape = treatment)) +
  geom_ribbon(data = wtd.sd.model.fit, aes(x = subsidence, ymin = I(exp(lwr)), ymax = I(exp(upr))), 
              inherit.aes = FALSE,
              fill = 'gray', 
              alpha = 0.5) +
  geom_line(data = wtd.sd.model.fit, aes(x = subsidence, y = I(exp(fit))), color = 'black') +
  geom_text(aes(x = 111, y = 16, 
                label = wtd.sd.r2.label1),
            parse = TRUE,
            hjust = 'inward',
            size = 3.5) +
  geom_text(aes(x = 111, y = 14.5, 
                label = wtd.sd.r2.label2),
            parse = TRUE,
            hjust = 'inward',
            size = 3.5) +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'SD WTD (cm)') +
  theme_bw() +
  theme(legend.title = element_blank())
wtd.sd.plot

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
  scale_y_continuous(name = 'SD WTD (cm)') +
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
  scale_y_continuous(name = 'SD WTD (cm)') +
  theme_bw() +
  theme(legend.title = element_blank())

### VWC
# model1 <- lmer(vwc.mean ~ 1 +
#                  (1 | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model1)
# 
# model2 <- lmer(vwc.mean ~ subsidence +
#                  (1 | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model2)
# 
# model3 <- lmer(vwc.mean ~ subsidence +
#                  (1 + subsidence | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model3)
# 
# model4 <- lmer(vwc.mean ~ subsidence + I(subsidence^2) +
#                  (1 + subsidence | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model4)
# 
# AIC(model1, model2, model3, model4)
# 
# ranova(model4)
# 
# check model residuals of model4
# # look at residuals
# model4.resid <- resid(model4)
# model4.fitted <- fitted(model4)
# model4.sqrt <- sqrt(abs(resid(model4)))
# 
# # graph
# par(mfrow=c(2,2), mar = c(4,4,3,2))
# plot(model4.fitted, model4.resid, main='resid, model4')
# plot(model4.fitted, model4.sqrt, main='sqrt resid, model4')
# qqnorm(model4.resid, main = 'model4')
# qqline(model4.resid)
# par(mfrow=c(1,1))
# 
# hist(sub.moisture$vwc.mean)

# re-run with REML = TRUE
# vwc.model <- lmer(vwc.mean ~ subsidence + I(subsidence^2) +
#                  (1 + subsidence | block.f/fence.f/treatment.f/replicate.f), REML = TRUE,
#                      data = sub.moisture,
#                      control=lmerControl(check.conv.singular="warning"))
# summary(vwc.model)
# saveRDS(vwc.model,
#         '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/vwc_model.rds')
vwc.model <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/vwc_model.rds')
summary(vwc.model)

# # calculate confidence intervals to look at fixed effects
# vwc.model.ci <- extract_ci(vwc.model)
# # write.csv(vwc.model.ci, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/vwc_coefficients.csv', row.names = FALSE)
vwc.model.ci <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/vwc_coefficients.csv')

vwc.model.r2 <- r.squaredGLMM(vwc.model)
vwc.r2.label1 <- paste0(as.character(expression('R'^2 ~ 'm = ')), ' ~ ', round(vwc.model.r2[1], 2))
vwc.r2.label2 <- paste0(as.character(expression('R'^2 ~ 'c = ')), ' ~ ', round(vwc.model.r2[2], 2))

# # make confidence interval data frame for graphing
# vwc.model.fit <- expand.grid(subsidence = round(min(sub.moisture$subsidence)):round(max(sub.moisture$subsidence)))
# 
# myStats <- function(model){
#   out <- predict( model, newdata=vwc.model.fit, re.form=~0 )
#   return(out)
# }
# 
# bootObj <- bootMer(vwc.model, FUN=myStats, nsim = 1000)
# vwc.model.fit <- cbind(vwc.model.fit, predict(vwc.model, newdata=vwc.model.fit, re.form=~0 )) %>%
#   cbind(confint( bootObj,  level=0.95 ))
# colnames(vwc.model.fit) <- c('subsidence', 'fit', 'lwr', 'upr')
# # write.csv(vwc.model.fit, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/vwc_model_fit.csv', row.names = FALSE)
vwc.model.fit <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/vwc_model_fit.csv')

sub.moisture <- sub.moisture %>%
  mutate(vwc.fit = ifelse(!is.na(vwc.mean),
                             predict(vwc.model),
                             NA))

ggplot(sub.moisture,
       aes(x = subsidence, y = vwc.mean)) +
  geom_point(aes(color = flux.year, shape = treatment)) +
  geom_line(aes(x = subsidence, y = vwc.fit), color = 'black') +
  geom_line(data = vwc.model.fit, aes(x = subsidence, y = fit), color = 'red') +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'VWC (%)') +
  facet_wrap(~plot.id, ncol = 8) +
  theme_bw() +
  theme(legend.title = element_blank())

vwc.plot <- ggplot(sub.moisture, aes(x = subsidence, y = vwc.mean)) +
  geom_point(aes(color = flux.year, shape = treatment)) +
  geom_ribbon(data = vwc.model.fit, aes(x = subsidence, ymin = lwr, ymax = upr), 
              inherit.aes = FALSE,
              fill = 'gray', 
              alpha = 0.5) +
  geom_line(data = vwc.model.fit, aes(x = subsidence, y = fit), color = 'black') +
  geom_text(aes(x = 111, y = 28.5, 
                label = vwc.r2.label1),
            parse = TRUE,
            hjust = 'inward',
            size = 3.5) +
  geom_text(aes(x = 111, y = 22, 
                label = vwc.r2.label2),
            parse = TRUE,
            hjust = 'inward',
            size = 3.5) +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'VWC (%)') +
  theme_bw() +
  theme(legend.title = element_blank())
vwc.plot

# model1 <- lmer(vwc.sd ~ 1 +
#                  (1 | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model1)
# 
# model2 <- lmer(vwc.sd ~ subsidence +
#                  (1 | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model2)
# 
# model3 <- lmer(vwc.sd ~ subsidence +
#                  (1 + subsidence | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model3)
# 
# model4 <- lmer(vwc.sd ~ subsidence + I(subsidence^2) +
#                  (1 + subsidence | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model4)
# 
# AIC(model1, model2, model3, model4)
# 
# ranova(model4)
# 
# # check model residuals of model4
# # look at residuals
# model4.resid <- resid(model4)
# model4.fitted <- fitted(model4)
# model4.sqrt <- sqrt(abs(resid(model4)))
# 
# # graph
# par(mfrow=c(2,2), mar = c(4,4,3,2))
# plot(model4.fitted, model4.resid, main='resid, model4')
# plot(model4.fitted, model4.sqrt, main='sqrt resid, model4')
# qqnorm(model4.resid, main = 'model4')
# qqline(model4.resid)
# par(mfrow=c(1,1))
# 
# hist(sub.moisture$vwc.sd)

# # re-run with REML = TRUE
# vwc.sd.model <- lmer(vwc.sd ~ subsidence + I(subsidence^2) +
#                        (1 + subsidence | block.f/fence.f/treatment.f/replicate.f), REML = TRUE,
#                      data = sub.moisture,
#                      control=lmerControl(check.conv.singular="warning"))
# summary(vwc.sd.model)
# saveRDS(vwc.sd.model,
#         '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/vwc_sd_model.rds')
vwc.sd.model <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/vwc_sd_model.rds')
summary(vwc.sd.model)

# # calculate confidence intervals to look at fixed effects
# vwc.sd.model.ci <- extract_ci(vwc.sd.model)
# # write.csv(vwc.sd.model.ci, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/vwc_sd_coefficients.csv', row.names = FALSE)
vwc.sd.model.ci <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/vwc_sd_coefficients.csv')

vwc.sd.model.r2 <- r.squaredGLMM(vwc.sd.model)
vwc.sd.r2.label1 <- paste0(as.character(expression('R'^2 ~ 'm = ')), ' ~ ', round(vwc.sd.model.r2[1], 2))
vwc.sd.r2.label2 <- paste0(as.character(expression('R'^2 ~ 'c = ')), ' ~ ', round(vwc.sd.model.r2[2], 2))

# # make confidence interval data frame for graphing
# vwc.sd.model.fit <- expand.grid(subsidence = round(min(sub.moisture$subsidence)):round(max(sub.moisture$subsidence)))
# 
# myStats <- function(model){
#   out <- predict( model, newdata=vwc.sd.model.fit, re.form=~0 )
#   return(out)
# }
# 
# bootObj <- bootMer(vwc.sd.model, FUN=myStats, nsim = 1000)
# vwc.sd.model.fit <- cbind(vwc.sd.model.fit, predict(vwc.sd.model, newdata=vwc.sd.model.fit, re.form=~0 )) %>%
#   cbind(confint( bootObj,  level=0.95 ))
# colnames(vwc.sd.model.fit) <- c('subsidence', 'fit', 'lwr', 'upr')
# # write.csv(vwc.sd.model.fit, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/vwc_sd_model_fit.csv', row.names = FALSE)
vwc.sd.model.fit <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/vwc_sd_model_fit.csv')

sub.moisture <- sub.moisture %>%
  mutate(vwc.sd.fit = ifelse(!is.na(vwc.sd),
                             predict(vwc.sd.model),
                             NA))

ggplot(sub.moisture,
       aes(x = subsidence, y = vwc.sd)) +
  geom_point(aes(color = flux.year, shape = treatment)) +
  geom_line(aes(x = subsidence, y = vwc.sd.fit), color = 'black') +
  geom_line(data = vwc.sd.model.fit, aes(x = subsidence, y = fit), color = 'red') +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'SD VWC (%)') +
  facet_wrap(~plot.id, ncol = 8) +
  theme_bw() +
  theme(legend.title = element_blank())

vwc.sd.plot <- ggplot(sub.moisture,
       aes(x = subsidence, y = vwc.sd)) +
  geom_point(aes(color = flux.year, shape = treatment)) +
  geom_ribbon(data = vwc.sd.model.fit, aes(x = subsidence, ymin = lwr, ymax = upr), 
              inherit.aes = FALSE,
              fill = 'gray', 
              alpha = 0.5) +
  geom_line(data = vwc.sd.model.fit, aes(x = subsidence, y = fit), color = 'black') +
  geom_text(aes(x = -10, y = 0.37, 
                label = vwc.sd.r2.label1),
            parse = TRUE,
            hjust = 'inward',
            size = 3.5) +
  geom_text(aes(x = -10, y = 0.3, 
                label = vwc.sd.r2.label2),
            parse = TRUE,
            hjust = 'inward',
            size = 3.5) +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'SD VWC (%)') +
  theme_bw() +
  theme(legend.title = element_blank())
vwc.sd.plot

### GWC
# model1 <- lmer(I(log(gwc.mean)) ~ 1 +
#                  (1 | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model1)
# 
# model2 <- lmer(I(log(gwc.mean)) ~ subsidence +
#                  (1| block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model2)
# 
# model3 <- lmer(I(log(gwc.mean)) ~ subsidence +
#                  (1 + subsidence | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model3)
# 
# model4 <- lmer(I(log(gwc.mean)) ~ subsidence + I(subsidence^2) +
#                  (1 + subsidence | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model4)
# 
# AIC(model1, model2, model3, model4)
# 
# ranova(model4)
# 
# check model residuals of model4
# # look at residuals
# model4.resid <- resid(model4)
# model4.fitted <- fitted(model4)
# model4.sqrt <- sqrt(abs(resid(model4)))
# 
# # graph
# par(mfrow=c(2,2), mar = c(4,4,3,2))
# plot(model4.fitted, model4.resid, main='resid, model4')
# plot(model4.fitted, model4.sqrt, main='sqrt resid, model4')
# qqnorm(model4.resid, main = 'model4')
# qqline(model4.resid)
# par(mfrow=c(1,1))
# 
# hist(log(sub.moisture$I(log(gwc.mean))))

gwc.plot <- ggplot(sub.moisture, aes(x = subsidence, y = gwc.mean)) +
  geom_point(aes(color = flux.year, shape = treatment)) +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'GWC (%)') +
  theme_bw() +
  theme(legend.title = element_blank())
gwc.plot

# model1 <- lmer(gwc.sd ~ 1 +
#                  (1 | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model1)
# 
# model2 <- lmer(gwc.sd ~ subsidence +
#                  (1 | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model2)
# 
# model3 <- lmer(gwc.sd ~ subsidence +
#                  (1 + subsidence | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model3)
# 
# model4 <- lmer(gwc.sd ~ subsidence + I(subsidence^2) +
#                  (1 + subsidence | block.f/fence.f/treatment.f/replicate.f), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model4)
# 
# AIC(model1, model2, model3, model4)
# 
# ranova(model3)
# 
# # check model residuals of model3
# # look at residuals
# model3.resid <- resid(model3)
# model3.fitted <- fitted(model3)
# model3.sqrt <- sqrt(abs(resid(model3)))
# 
# # graph
# par(mfrow=c(2,2), mar = c(4,4,3,2))
# plot(model3.fitted, model3.resid, main='resid, model3')
# plot(model3.fitted, model3.sqrt, main='sqrt resid, model3')
# qqnorm(model3.resid, main = 'model3')
# qqline(model3.resid)
# par(mfrow=c(1,1))
# 
# hist(sub.moisture$gwc.sd)

gwc.sd.plot <- ggplot(sub.moisture,
       aes(x = subsidence, y = gwc.sd, color = flux.year, shape = treatment)) +
  geom_point() +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'SD GWC (%)') +
  theme_bw() +
  theme(legend.title = element_blank())
gwc.sd.plot

### Combine into a single plot
wtd.plot
wtd.sd.plot
vwc.plot
vwc.sd.plot
gwc.plot
gwc.sd.plot
sub.moisture.plot <- ggarrange(gwc.plot,
                               gwc.sd.plot,
                               vwc.plot,
                               vwc.sd.plot,
                               wtd.plot,
                               wtd.sd.plot,
                               ncol = 2,
                               nrow = 3,
                               common.legend = TRUE,
                               legend = 'right',
                               labels = LETTERS[1:6])
sub.moisture.plot

# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/subsidence_moisture_linear_models.jpg',
#        sub.moisture.plot,
#        height = 7,
#        width = 6.5,
#        bg = 'white') # As of 9/24/21, with no updates to R, R packages, or OS, this started plotting with a black background... I have no idea what might have changed
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
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 7))
wtd.precip
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/wtd_precip_sub.jpg',
#        wtd.precip,
#        height = 5,
#        width = 6.5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/wtd_precip_sub.pdf',
#        wtd.precip,
#        height = 5,
#        width = 6.5)
################################################################################

### Impact of Subsidence on Soil Moisture (Spatial) ############################
### Model WTD by (2 week?) precip, subsidence, and TD
hydrology <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/hydrology.csv')

hydrology <- hydrology %>%
  separate(plot.id, into = c('fence2', 'plot'), remove = FALSE) %>%
  mutate(block = case_when(fence <= 2 ~ 'a',
                           fence <= 4 ~ 'b',
                           fence <= 6 ~ 'c'),
         block.f = factor(case_when(block == 'a' ~ 1,
                                    block == 'b' ~ 2,
                                    block == 'c' ~ 3)),
         fence.f = factor(fence),
         treatment.f = as.factor(ifelse(treatment == 'Control',
                                        1,
                                        ifelse(treatment == 'Air Warming',
                                               2,
                                               ifelse(treatment == 'Soil Warming',
                                                      3,
                                                      4)))),
         fencegroup = factor(block.f:fence.f),
         wholeplot = factor(block.f:fence.f:treatment.f),
         time = factor(date),
         precip.1w = ifelse(is.na(precip.1w) & date >= as_date('2020-10-01') & date <= as_date('2020-10-04'),
                            0,
                            precip.1w),
         precip.2w = ifelse(is.na(precip.2w) & date >= as_date('2020-10-01') & date <= as_date('2020-10-04'),
                            0,
                            precip.2w),
         subsidence = subsidence*-1) %>%
  select(year, date, block, fence, plot, plot.id, treatment, wtd, precip.1w, 
         precip.2w, subsidence, td, td.date = TD_Date, block.f, fencegroup, 
         wholeplot, time)

### Create spatial WTD data
### Load WTD
wtd <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/wtd/WTD_2020_compiled.csv")
ww.locations <- st_read('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/water_wells.shp')

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
wtd[, wtd := WTD]
wtd[, WTD := NULL]
wtd[, wtd.date := ymd(paste(year, month, day, sep = '/'))]

# well 2-3.17 mislabeled as 2-2.17 for a few measurements in 2020
wtd[fence == 2 & well == 2.17 & year == 2020,
    well := 3.17]
# duplicate measurement at 5-1 2020-06-21 - I think 10 cm is right
wtd <- wtd[!(fence == 5 & well == 1 & date == as_date('2020-06-21') & wtd == 1)]

# Format location data
ww.locations <- ww.locations %>%
  separate(Name, into = c('fence', 'well'), remove = FALSE, extra = 'merge') %>%
  mutate(fence = as.integer(str_sub(fence, start = 3)),
         well = as.numeric(well)) %>%
  filter(!is.na(well)) %>%
  select(fence, well) %>%
  st_zm()

# Join wtd and well locations
wtd.sf <- left_join(wtd, ww.locations, by = c('fence', 'well')) %>%
  st_as_sf(crs = st_crs(ww.locations))

# get subsidence surface data ready for kriging
sub <- list(brick('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/subsidence_stacks/ASubStack2009-2020.tif'),
            brick('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/subsidence_stacks/BSubStack2009-2020.tif'),
            brick('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Subsidence_Clipped/ALT_Sub_Ratio_Corrected/subsidence_stacks/CSubStack2009-2020.tif'))

sub.grids <- list()
for (i in 1:length(sub)) {
  sub.grids[[i]] <- list()
  for (j in 1:nlayers(sub[[i]])) {
    sub.grids[[i]][[j]] <- sub[[i]][[j]] %>%
      as.data.frame(xy = TRUE) %>%
      st_as_sf(coords = c('x', 'y'), crs = 6397) %>%
      st_rasterize()
    names(sub.grids[[i]][[j]]) <- 'subsidence' # rename so that model can find the subsidence values
  }
}

# split wtd into list and add subsidence data for kriging
# create spatial objects containing the points for each block, year, and date
wtd.list <- list()
for(block.n in 1:length(unique(wtd.sf$block))) { # iterate over each block
  
  # create list for output
  wtd.list[[block.n]] <- list()
  
  # create subset of data by block
  wtd.subset.1 <- wtd.sf %>%
    subset(block == as.character(unique(wtd.sf$block)[[block.n]]))
  
  for (year.n in 1:length(unique(wtd.subset.1$year))) {
    
    # create list for output
    wtd.list[[block.n]][[year.n]] <- list()
    
    # create subset of data by year
    wtd.subset.2 <- wtd.subset.1 %>%
      subset(year == unique(wtd.subset.1$year)[[year.n]])
    
    for (date.n in 1:length(unique(wtd.subset.2$date))) { # iterate over each date
      
      # create subset of data by date
      wtd.subset.3 <- wtd.subset.2 %>%
        subset(date == unique(wtd.subset.2$date)[[date.n]]) %>% # select only the data from block block.n, date year.n
        st_zm(drop = TRUE, what = "ZM") # get rid of Z and M geometries to convert to sp
      
      # add subsidence data
      sub.extract <- raster::extract(sub[[block.n]][[year.n]], wtd.subset.3, df = TRUE) %>%
        as.data.frame() %>%
        rename(subsidence = 2) %>%
        select(2)
      wtd.subset.3 <- wtd.subset.3 %>%
        cbind.data.frame(sub.extract) %>%
        st_as_sf(crs = 6397) %>%
        filter(!is.na(subsidence))
      
      name <- paste(as.character(unique(wtd.subset.3$block)), 
                    as.character(unique(wtd.subset.3$year)),
                    as.character(unique(wtd.subset.3$date)),
                    sep = '.') # create a unique name for each block's output
      
      wtd.list[[block.n]][[year.n]][[date.n]] <- wtd.subset.3
      names(wtd.list[[block.n]][[year.n]])[[date.n]] <- name # name the output list element
    }
  }
  rm(block.n, year.n, wtd.subset.1, wtd.subset.2, wtd.subset.3, name)
}


# Krige wtd surfaces
### run loop to krige all surfaces using autoKrige
wtd.surface <- list()
for (block.n in 1:length(wtd.list)) {
  
  wtd.surface[[block.n]] <- list()
  
  for (year.n in 1:length(wtd.list[[block.n]])) {
    
    wtd.surface[[block.n]][[year.n]] <- list()
    
    for (date.n in 1:length(wtd.list[[block.n]][[year.n]])) {
      
      print(paste0('block = ', block.n, ', year = ', year.n, ', date = ', date.n))
      
      input <- as(wtd.list[[block.n]][[year.n]][[date.n]], 'Spatial')
      newdata <- as(sub.grids[[block.n]][[year.n]], 'Spatial')
      krige.output <- autoKrige(wtd ~ subsidence, input, newdata)
      krige.surface <- crop(raster(krige.output[[1]]),
                       sub[[block.n]])
      wtd.surface[[block.n]][[year.n]][[date.n]] <- krige.surface
      
      names(wtd.surface[[block.n]][[year.n]])[[date.n]] <- names(wtd.list[[block.n]][[year.n]])[[date.n]]
    }
  }
  rm(input, newdata, krige.output, krige.surface, block.n, year.n, date.n)
}

# # save data
# saveRDS(wtd.surface,
#         '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/hydrology_kriging_output/wtd_surfaces.rds')


### Format TD data
td <- fread("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/Thaw Depth/Processed/2020/Thaw_depth_2009-2020.csv",
            colClasses = list(character = c('date')))

# Remove rows without plot data or thaw depth data
td <- td[!is.na(plot)]
td <- td[!is.na(td)]
# # Remove DryPEHR Data
# td <- td[!is.na(as.numeric(plot))]

# Date
td[, date := as_date(parse_date_time(date, orders = c('Y!-m!-d!', 'm!/d!/y!')))]
td[, year := year(date)]
td[, block := tolower(block)]
td[, plot := tolower(plot)]
td <- unique(td)

# # This section has been fixed in the underlying data, and is no longer necessary
# # incorrect date recorded for one measurement in 2019
# # two values recorded for 2019-08-02, but first should be ~2019-07-28
# td[date == as_date('2019-08-02'),
#    fix.date := c(rep(1, 16), rep(2, 16))]
# td[fix.date == 1,
#    date := as_date('2019-07-28')]
# td[, fix.date := NULL]

# add subsidence to help with kriging
sub.df <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Thaw_Depth_Subsidence_Correction/ALT_Sub_Ratio_Corrected/ALT_Subsidence_Corrected_2009_2020.csv",
             header = TRUE,
             stringsAsFactors = FALSE) %>%
  select(year, fence, plot, subsidence)
sub.df <- unique(sub.df)
td <- merge(td, sub.df,
              by = c('year', 'fence', 'plot'))

# Plot ID
td <- td[order(date, block, fence, plot)]
td <- td[,.(year, date, block, fence, plot, td, subsidence)]

# Krige td surfaces
plots.all <- st_read('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/plot_coordinates_from_2017.shp')
td.sf <- td %>%
  left_join(plots.all, by = c('fence', 'plot')) %>%
  st_as_sf(crs = 6397)

# create spatial objects containing the points for each block, year, and date
td.list <- list()
for(i in 1:length(unique(td.sf$block))) { # iterate over each block
  
  # create list for output
  td.list[[i]] <- list()
  
  # create subset of data by block
  td.subset.1 <- td.sf %>%
    subset(block == as.character(unique(td.sf$block)[[i]]))
  
  for (j in 1:length(unique(td.subset.1$year))) {
    
    # create list for output
    td.list[[i]][[j]] <- list()
    
    # create subset of data by year
    td.subset.2 <- td.subset.1 %>%
      subset(year == unique(td.subset.1$year)[[j]])
    
    for (k in 1:length(unique(td.subset.2$date))) { # iterate over each date
      
      # create subset of data by date
      td.subset.3 <- td.subset.2 %>%
        subset(date == unique(td.subset.2$date)[[k]]) %>% # select only the data from block i, date j
        st_zm(drop = TRUE, what = "ZM") # get rid of Z and M geometries to convert to sp
      
      name <- paste(as.character(unique(td.subset.3$block)), 
                    as.character(unique(td.subset.3$year)),
                    as.character(unique(td.subset.3$date)),
                    sep = '.') # create a unique name for each block's output
     
      td.list[[i]][[j]][[k]] <- td.subset.3
      names(td.list[[i]][[j]])[[k]] <- name # name the output list element
    }
  }
  rm(i, j, td.subset.1, td.subset.2, td.subset.3, name)
}

### run loop to krige all surfaces using autoKrige
td.surface <- list()
for (block.n in 1:length(td.list)) {
  
  td.surface[[block.n]] <- list()
  
  for (year.n in 1:length(td.list[[block.n]])) {
    
    td.surface[[block.n]][[year.n]] <- list()
    
    for (date.n in 1:length(td.list[[block.n]][[year.n]])) {
      
      print(paste0('block = ', block.n, ', year = ', year.n, ', date = ', date.n))
      
      input <- as(td.list[[block.n]][[year.n]][[date.n]], 'Spatial')
      newdata <- as(sub.grids[[block.n]][[year.n]], 'Spatial')
      krige.output <- autoKrige(td ~ subsidence, input, newdata)
      krige.surface <- crop(raster(krige.output[[1]]),
                       sub[[block.n]])
      td.surface[[block.n]][[year.n]][[date.n]] <- krige.surface
      
      names(td.surface[[block.n]][[year.n]])[[date.n]] <- names(td.list[[block.n]][[year.n]])[[date.n]]
    }
  }
  rm(input, newdata, krige.output, krige.surface, block.n, year.n, date.n)
}


# # save data
# saveRDS(td.surface,
#         '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/hydrology_kriging_output/td_surfaces.rds')


### Select days for high and low precip in previous week 
# (preferrably in last couple of years near end of growing season)
ggplot(flux.daily[flux.year == 2017 & month %in% c(7, 8, 9)], 
       aes(x = date, y = precip)) +
  geom_line()
ggplot(flux.daily[flux.year == 2018 & month %in% c(7, 8, 9)], 
       aes(x = date, y = precip)) +
  geom_line()
ggplot(flux.daily[flux.year == 2018 & month %in% c(7, 8)], 
       aes(x = date, y = precip)) +
  geom_line()
ggplot(flux.daily[flux.year == 2019 & month %in% c(7, 8, 9)], 
       aes(x = date, y = precip)) +
  geom_line()
ggplot(flux.daily[flux.year == 2020 & month %in% c(7, 8, 9)], 
       aes(x = date, y = precip)) +
  geom_line()
ggplot(flux.daily[flux.year == 2020 & month == 8], 
       aes(x = date, y = precip)) +
  geom_line()

### Possible dates
# week ending 2020-08-10 for wet
# wtd on 2020-08-11 and td on ...?
# week ending 2020-08-23 for dry, but that's really soon after a wet period
# wtd on 2020-08-24 and td on ...?
### OR ###
### I used these 2018 dates!
# week ending on 2018-07-31 for dry
# wtd on 2018-07-30, td on 2020-07-27
# week ending on 2018-08-08 for wet
# wtd on 2018-08-08, td on 2020-08-10

# Join elevation (annual), and wtd and td data from selected dates into brick
hydro.brick.dry <- list()
hydro.brick.wet <- list()
for (block.n in 1:length(elev.clipped)) {
  hydro.brick.dry[[block.n]] <- brick(elev.clipped[[block.n]][[10]],
                                      wtd.surface[[block.n]][[10]][[which(unique(subset(wtd.sf, year == 2018)$date) == as_date('2018-07-30'))]],
                                      td.surface[[block.n]][[10]][[which(unique(subset(td.sf, year == 2018)$date) == as_date('2018-07-27'))]])
  hydro.brick.dry[[block.n]]@data@names <- c('elevation', 'wtd', 'td')
  hydro.brick.wet[[block.n]] <- brick(elev.clipped[[block.n]][[10]],
                                      wtd.surface[[block.n]][[10]][[which(unique(subset(wtd.sf, year == 2018)$date) == as_date('2018-08-08'))]],
                                      td.surface[[block.n]][[10]][[which(unique(subset(td.sf, year == 2018)$date) == as_date('2018-08-10'))]])
  hydro.brick.wet[[block.n]]@data@names <- c('elevation', 'wtd', 'td')
}
names(hydro.brick.dry) <- c('elevation', 'wtd', 'td')
names(hydro.brick.wet) <- c('elevation', 'wtd', 'td')

plot(hydro.brick.dry[[1]])
plot(hydro.brick.dry[[2]])
plot(hydro.brick.dry[[3]])
plot(hydro.brick.wet[[1]])
plot(hydro.brick.wet[[2]])
plot(hydro.brick.wet[[3]])


### Create virtual transect across each fence
transects <- data.frame(fence = seq(1, 6),
                        x = c(537946, 537957.5, 538007.25, 538017.5, 538127, 538138.5),
                        y = c(1100970, 1100977.5, 1101101.75, 1101113.5, 1101026, 1101033.5))
transects <- transects %>%
  rbind.data.frame(transects %>%
                     mutate(x = 22*sin(-0.588) + x,
                            y = 22*cos(0.588) + y)) %>% # to create 20 m long transects at approximately the right angle
  arrange(fence, x)

linestring <- list()
for (fence.n in 1:6) {
  transect.subset <- transects %>%
    filter(fence == fence.n) %>%
    select(-fence) %>%
    as.matrix()
  linestring[[fence.n]] <- st_linestring(transect.subset)
}

transects <- data.frame(fence = seq(1, 6),
                        geometry = st_sfc(linestring)) %>%
  st_as_sf(crs = 6397)

# calculate 11 points along each transect (every 2 meters)
transect.points <- transects %>%
  st_line_sample(n = 11) %>%
  st_as_sf(crs = 6397) %>%
  mutate(fence = seq(1, 6)) %>%
  st_cast(to = 'POINT')

# Add in fence locations
fences <- st_read('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/Fences.shp')

fence.intersection <- st_intersection(transects, fences)
fence.dist <- st_distance(fence.intersection, transect.points[seq(1, 6)*11,]) %>%
  diag() %>%
  as.data.frame() %>%
  mutate(fence = seq(1, 6)) %>%
  select(fence, fence.dist = 1) %>%
  mutate(fence.dist = as.numeric(fence.dist))

# Block A
ggplot(filter(transects, fence <= 2)) +
  geom_tile(data = as.data.frame(hydro.brick.dry[[1]][[1]], xy = TRUE),
            aes(x, y, fill = elevation),
            inherit.aes = FALSE) +
  scale_fill_viridis(na.value = 'transparent') +
  geom_sf(color = 'black') +
  geom_sf(data = td.list[[1]][[1]][[1]],
          inherit.aes = FALSE,
          color = 'black') +
  geom_sf(data = filter(transect.points, fence %in% c(1, 2)),
          inherit.aes = FALSE,
          color = 'red') +
  geom_sf(data = slice(fence.intersection, 1:2),
          inherit.aes = FALSE,
          color = 'blue') +
  coord_sf(datum = st_crs(transects))

# Block B
ggplot(filter(transects, fence > 2 & fence <= 4)) +
  geom_tile(data = as.data.frame(hydro.brick.dry[[2]][[1]], xy = TRUE),
            aes(x, y, fill = elevation),
            inherit.aes = FALSE) +
  scale_fill_viridis(na.value = 'transparent') +
  geom_sf(color = 'black') +
  geom_sf(data = td.list[[2]][[1]][[1]],
          inherit.aes = FALSE,
          color = 'black') +
  geom_sf(data = filter(transect.points, fence %in% c(3, 4)),
          inherit.aes = FALSE,
          color = 'red') +
  geom_sf(data = slice(fence.intersection, 3:4),
          inherit.aes = FALSE,
          color = 'blue') +
  coord_sf(datum = st_crs(transects))

# Block C
ggplot(filter(transects, fence > 4)) +
  geom_tile(data = as.data.frame(hydro.brick.dry[[3]][[1]], xy = TRUE),
            aes(x, y, fill = elevation),
            inherit.aes = FALSE) +
  scale_fill_viridis(na.value = 'transparent') +
  geom_sf(color = 'black') +
  geom_sf(data = td.list[[3]][[1]][[1]],
          inherit.aes = FALSE,
          color = 'black') +
  geom_sf(data = filter(transect.points, fence %in% c(5, 6)),
          inherit.aes = FALSE,
          color = 'red') +
  geom_sf(data = slice(fence.intersection, 5:6),
          inherit.aes = FALSE,
          color = 'blue') +
  coord_sf(datum = st_crs(transects))

### Extract raster elevation, td, wtd along transects
transect.extract <- data.frame()
for (i in 1:3) {
  
  tmp <- raster::extract(hydro.brick.dry[[i]], filter(transect.points, fence %in% c(i*2 - 1, i*2)),
                          layer = 1, nl = 3) %>%
    as.data.frame() %>%
    cbind.data.frame(filter(transect.points, fence %in% c(i*2 - 1, i*2))) %>%
    mutate(distance = rep(seq(10, 0)*2, 2),
           condition = 'Dry') %>%
    group_by(fence) %>%
    mutate(elev.norm = elevation - max(elevation),
           wtd.norm = elev.norm - wtd*0.01,
           td.norm = elev.norm - td*0.01) %>%
    st_as_sf(crs = 6397)
  
  transect.extract <- rbind(transect.extract, tmp)
  
  tmp <- raster::extract(hydro.brick.wet[[i]], filter(transect.points, fence %in% c(i*2 - 1, i*2)),
                         layer = 1, nl = 3) %>%
    as.data.frame() %>%
    cbind.data.frame(filter(transect.points, fence %in% c(i*2 - 1, i*2))) %>%
    mutate(distance = rep(seq(10, 0)*2, 2),
           condition = 'Wet') %>%
    group_by(fence) %>%
    mutate(elev.norm = elevation - max(elevation),
           wtd.norm = elev.norm - wtd*0.01,
           td.norm = elev.norm - td*0.01) %>%
    st_as_sf(crs = 6397)
  
  transect.extract <- rbind(transect.extract, tmp)
  
}

transect.extract.diff <- transect.extract %>%
  st_drop_geometry() %>%
  select(fence, distance, condition, wtd) %>%
  mutate(wtd = wtd*0.01) %>%
  pivot_wider(id_cols = c(fence, distance), 
              names_from = condition, 
              values_from = c(wtd),
              names_sep = '.') %>%
  mutate(wtd.diff = Dry-Wet,
         condition = 'Wet - Dry') %>%
  select(fence, distance, condition, wtd.diff)

transect.extract <- transect.extract %>%
  mutate(condition = ifelse(condition == 'Wet',
                            'Wet Conditions',
                            'Dry Conditions'))


### Plot transects
# colors needed
names <- c('Unsaturated Active Layer', 'Saturated Active Layer', 'Permafrost')
color <- c('Permafrost' = '#666666', 
           'Unsaturated Active Layer' = '#996633', 
           'Saturated Active Layer' = '#006699')
treatment.labels <- data.frame(fence = 6,
                               condition = 'Dry Conditions',
                               arrow.1.start = 14,
                               arrow.1.end = 13,
                               arrow.2.start = 14.75,
                               arrow.2.end = 15.75,
                               label.1 = 'Control',
                               label.2 = 'Soil\nWarming')

transect.plot <- ggplot(transect.extract,
       aes(x = distance)) +
  geom_ribbon(aes(ymin = wtd.norm, ymax = elev.norm, fill = 'Unsaturated Active Layer'),
              linetype = 1,
              size = 0.5,
              colour = 'black') +
  geom_ribbon(aes(ymin = td.norm, ymax = wtd.norm, fill = 'Saturated Active Layer'),
              linetype = 1,
              size = 0.25,
              colour = 'black') +
  geom_ribbon(aes(ymin = -Inf, ymax = td.norm, fill = 'Permafrost'),
              linetype = 1,
              size = 0.5,
              colour = 'black') +
  geom_line(aes(y = elev.norm), linetype = 'dashed', size = 0.5) +
  geom_vline(data = fence.dist, 
             aes(xintercept = fence.dist),
             linetype = 'dotted') +
  geom_segment(data = treatment.labels,
               aes(x = arrow.1.start, xend = arrow.1.end,
                   y = -1.9, yend = -1.9),
               arrow = arrow(length = unit(0.1, 'lines'))) +
  geom_segment(data = treatment.labels,
               aes(x = arrow.2.start, xend = arrow.2.end,
                   y = -1.9, yend = -1.9),
               arrow = arrow(length = unit(0.1, 'lines'))) +
  geom_text(data = treatment.labels,
            aes(x = arrow.1.end, y = -1.9, label = label.1),
            hjust = 'right',
            nudge_x = -0.25,
            size = 2) +
  geom_text(data = treatment.labels,
            aes(x = arrow.2.end, y = -1.9, label = label.2),
            hjust = 'left',
            nudge_x = 0.25,
            size = 2) +
  scale_fill_manual(name = '',
                    values = color,
                    breaks = names) +
  scale_x_continuous(name = 'Distance (m)',
                     breaks = seq(0, 20, by = 5),
                     expand = c(0,0)) +
  scale_y_continuous(name = 'Normalized Elevation (m)') +
  facet_grid(fence ~ condition) +
  theme_bw() +
  theme(# legend.justification=c(0,0),
        # legend.position=c(0,0),
        legend.position = 'bottom',
        legend.title = element_blank(),
        strip.text.y = element_blank(),
        panel.spacing.x = unit(0.75, 'lines'))
transect.plot

# plot difference in wtd between wet and dry conditions
transect.diff.plot <- ggplot(transect.extract.diff,
       aes(x = distance, y = wtd.diff)) +
  geom_line(size = 0.25) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'gray') +
  geom_vline(data = fence.dist, 
             aes(xintercept = fence.dist),
             linetype = 'dotted') +
  scale_y_continuous(name = expression(Delta ~ 'WTD (m)'),
                     limits = c(0, 0.4)) +
  scale_x_continuous(name = 'Distance (m)',
                     breaks = seq(0, 20, by = 5),
                     expand = c(0, 0)) +
  facet_grid(fence ~ condition) +
  theme_bw()
transect.diff.plot

wtd.transect.plot <- ggarrange(transect.plot, transect.diff.plot,
          ncol = 2,
          widths = c(1.75, 1),
          common.legend = TRUE,
          legend = 'bottom')
wtd.transect.plot

# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/wtd_transect_plot.jpg',
#        wtd.transect.plot,
#        height = 7,
#        width = 6.5,
#        bg = 'white')
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/wtd_transect_plot.pdf',
#        wtd.transect.plot,
#        height = 7,
#        width = 6.5,
#        bg = 'white')
################################################################################