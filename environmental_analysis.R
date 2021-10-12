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
sub.moisture <- flux.annual %>%
  mutate(treatment = factor(treatment,
                            levels = c('Control',
                                       'Air Warming',
                                       'Soil Warming',
                                       'Air + Soil Warming')),
         subsidence = subsidence.annual*-1,
         wtd.mean = wtd.mean*-1) %>%
  select(flux.year, fence, plot, plot.id, treatment, subsidence, mtopo, wtd.mean, wtd.sd, wtd.n, 
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
         block.f = as.factor(ifelse(fence == 1 | fence == 2,
                                    1,
                                    ifelse(fence == 3 | fence == 4,
                                           2,
                                           3))),
         fence.f = as.factor(ifelse(fence == 1 | fence == 3 | fence == 5,
                                    1,
                                    2)),
         treatment.f = as.factor(ifelse(treatment == 'Control',
                                        1,
                                        ifelse(treatment == 'Air Warming',
                                               2,
                                               ifelse(treatment == 'Soil Warming',
                                                      3,
                                                      4)))),
         fencegroup = factor(block.f:fence.f),
         wholeplot = factor(block.f:fence.f:treatment.f))

### WTD

# Add in model with each wtd measurement at wells rather than plots?
# would need to calculate subsidence at wells
# This would maybe get at spatial variation better than annual averages?

## Model wtd with subsidence
# model1 <- lmer(wtd.mean ~ 1 +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model1)
# 
# model2 <- lmer(wtd.mean ~ subsidence +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model2)
# 
# model3 <- lmer(wtd.mean ~ subsidence + I(subsidence^2) +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model3)
# 
# AIC(model1, model2, model3)
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
# hist(sub.moisture$wtd.mean)

# re-run with REML = TRUE
# wtd.model <- lmer(wtd.mean ~ subsidence + I(subsidence^2) +
#                        (1 | block.f/fencegroup/wholeplot) + (1|time), REML = TRUE,
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
wtd.r2.label <- paste0(as.character(expression('R'^2 ~ 'm = ')), ' ~ ', round(wtd.model.r2[1], 2))

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

# wtd.fit <- sub.moisture %>%
#   mutate(wtd.fit = ifelse(!is.na(wtd.mean),
#                              predict(wtd.model),
#                              NA))
# ggplot(subset(sub.moisture, !is.na(wtd.sd)),
#        aes(x = subsidence, y = wtd.mean)) +
#   geom_hline(yintercept = 0, size = 0.1) +
#   geom_point(aes(color = flux.year, shape = treatment)) +
#   geom_line(data = wtd.fit, aes(x = subsidence, y = wtd.fit), color = 'black') +
#   geom_line(data = wtd.model.fit, aes(x = subsidence, y = fit), color = 'red') +
#   geom_text(aes(x = 100, y = -35, 
#                 label = wtd.r2.label),
#             parse = TRUE,
#             hjust = 'inward') +
#   scale_color_viridis(discrete = TRUE,
#                       direction = -1) +
#   scale_shape_manual(values = c(1, 0, 16, 15)) +
#   scale_x_continuous(name = 'Subsidence (cm)') +
#   scale_y_continuous(name = 'WTD (cm)') +
#   facet_wrap(.~plot.id, ncol = 8) +
#   theme_bw() +
#   theme(legend.title = element_blank())
# 
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
wtd.plot

# there is high variability in wtd when magnitude of subsidence is similar to 
# pre-subsidence wtd
## Model wtd.sd with precipitation and subsidence
# model1 <- lmer(wtd.sd ~ 1 +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model1)
# 
# model2 <- lmer(wtd.sd ~ subsidence +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model2)
# 
# model3 <- lmer(wtd.sd ~ subsidence + I(subsidence^2) +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model3)
# 
# AIC(model1, model2, model3)
# 
# # check model residuals of model3
# # look at residuals
# model2.resid <- resid(model2)
# model2.fitted <- fitted(model2)
# model2.sqrt <- sqrt(abs(resid(model2)))
# 
# # graph
# par(mfrow=c(2,2), mar = c(4,4,3,2))
# plot(model2.fitted, model2.resid, main='resid, model3')
# plot(model2.fitted, model2.sqrt, main='sqrt resid, model3')
# qqnorm(model2.resid, main = 'model3')
# qqline(model2.resid)
# par(mfrow=c(1,1))
# 
# hist(sub.moisture$wtd.sd)


# # re-run with REML = TRUE
# wtd.sd.model <- lmer(wtd.sd ~ subsidence +
#                        (1 | block.f/fencegroup/wholeplot) + (1|time), REML = TRUE,
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
wtd.sd.r2.label <- paste0(as.character(expression('R'^2 ~ 'm = ')), ' ~ ', round(wtd.sd.model.r2[1], 2))

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
       aes(x = subsidence, y = wtd.sd)) +
  geom_point(aes(color = flux.year, shape = treatment)) +
  geom_line(aes(x = subsidence, y = wtd.sd.fit), color = 'black') +
  geom_line(data = wtd.sd.model.fit, aes(x = subsidence, y = fit), color = 'red') +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'WTD SD (cm)') +
  facet_wrap(~plot.id, ncol = 8) +
  theme_bw() +
  theme(legend.title = element_blank())

wtd.sd.plot <- ggplot(subset(sub.moisture, !is.na(wtd.sd)),
       aes(x = subsidence, y = wtd.sd)) +
  geom_point(aes(color = flux.year, shape = treatment)) +
  geom_ribbon(data = wtd.sd.model.fit, aes(x = subsidence, ymin = lwr, ymax = upr), 
              inherit.aes = FALSE,
              fill = 'gray', 
              alpha = 0.5) +
  geom_line(data = wtd.sd.model.fit, aes(x = subsidence, y = fit), color = 'black') +
  geom_text(aes(x = 100, y = 17, 
                label = wtd.sd.r2.label),
            parse = TRUE,
            hjust = 'inward') +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'WTD SD (cm)') +
  theme_bw() +
  theme(legend.title = element_blank())
wtd.sd.plot

# test if the relationship is actually linear and due to subsidence or precipitation
ggplot(subset(sub.moisture, !is.na(wtd.sd)),
       aes(x = subsidence, y = wtd.sd)) +
  geom_point(aes(color = flux.year, shape = treatment)) +
  geom_line(data = test, aes(x = subsidence, y = fit), inherit.aes = FALSE, color = 'black') +
  # geom_smooth(method = 'gam', color = 'black') +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'WTD SD (cm)') +
  theme_bw() +
  theme(legend.title = element_blank()) +
  facet_wrap(~plot.id, ncol = 8)

ggplot(subset(sub.moisture, !is.na(wtd.sd)),
       aes(x = precip, y = wtd.sd)) +
  geom_point(aes(color = subsidence, shape = treatment)) +
  geom_line(data = test, aes(x = precip, y = fit), inherit.aes = FALSE, color = 'black') +
  # geom_smooth(method = 'gam', color = 'black') +
  scale_color_viridis(direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Precipitation (cm)') +
  scale_y_continuous(name = 'WTD SD (cm)') +
  theme_bw() +
  theme(legend.title = element_blank()) +
  facet_wrap(~plot.id, ncol = 8)

ggplot(subset(sub.moisture, !is.na(wtd.sd)),
       aes(x = precip, y = subsidence)) +
  geom_point(aes(color = flux.year, shape = treatment)) +
  geom_smooth(method = 'glm', color = 'black') +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Precip Z-score') +
  scale_y_continuous(name = 'Subsidence (cm)') +
  theme_bw() +
  theme(legend.title = element_blank()) +
  facet_wrap(~plot.id, ncol = 8)

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

### VWC
# model1 <- lmer(vwc.mean ~ 1 +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model1)
# 
# model2 <- lmer(vwc.mean ~ subsidence +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model2)
# 
# model3 <- lmer(vwc.mean ~ subsidence + I(subsidence^2) +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model3)
# 
# AIC(model1, model2, model3)
# 
# ranova(model2)
# 
# check model residuals of model2
# # look at residuals
# model2.resid <- resid(model2)
# model2.fitted <- fitted(model2)
# model2.sqrt <- sqrt(abs(resid(model2)))
# 
# # graph
# par(mfrow=c(2,2), mar = c(4,4,3,2))
# plot(model2.fitted, model2.resid, main='resid, model2')
# plot(model2.fitted, model2.sqrt, main='sqrt resid, model2')
# qqnorm(model2.resid, main = 'model2')
# qqline(model2.resid)
# par(mfrow=c(1,1))
# 
# hist(sub.moisture$vwc.mean)

# re-run with REML = TRUE
# vwc.model <- lmer(vwc.mean ~ subsidence +
#                        (1 | block.f/fencegroup/wholeplot) + (1|time), REML = TRUE,
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
vwc.r2.label <- paste0(as.character(expression('R'^2 ~ 'm = ')), ' ~ ', round(vwc.model.r2[1], 2))

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
  scale_y_continuous(name = 'WTD SD (cm)') +
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
vwc.plot

# model1 <- lmer(vwc.sd ~ 1 +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model1)
# 
# model2 <- lmer(vwc.sd ~ subsidence +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model2)
# 
# model3 <- lmer(vwc.sd ~ subsidence + I(subsidence^2) +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model3)
# 
# AIC(model1, model2, model3)
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
# hist(sub.moisture$vwc.sd)

# re-run with REML = TRUE
# vwc.sd.model <- lmer(vwc.sd ~ subsidence + I(subsidence^2) +
#                        (1 | block.f/fencegroup/wholeplot) + (1|time), REML = TRUE,
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
vwc.sd.r2.label <- paste0(as.character(expression('R'^2 ~ 'm = ')), ' ~ ', round(vwc.sd.model.r2[1], 2))

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
  scale_y_continuous(name = 'WTD SD (cm)') +
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
  geom_text(aes(x = 100, y = 0.94, 
                label = vwc.sd.r2.label),
            parse = TRUE,
            hjust = 'inward') +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'VWC SD (%)') +
  theme_bw() +
  theme(legend.title = element_blank())
vwc.sd.plot

### GWC
# model1 <- lmer(gwc.mean ~ 1 +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model1)
# 
# model2 <- lmer(gwc.mean ~ subsidence +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model2)
# 
# model3 <- lmer(gwc.mean ~ subsidence + I(subsidence^2) +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model3)
# 
# AIC(model1, model2, model3)
# 
# ranova(model3)
# 
# check model residuals of model3
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
# hist(sub.moisture$gwc.mean)

gwc.plot <- ggplot(sub.moisture, aes(x = subsidence, y = gwc.mean)) +
  geom_point(aes(color = flux.year, shape = treatment)) +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'gwc (%)') +
  theme_bw() +
  theme(legend.title = element_blank())
gwc.plot

# model1 <- lmer(gwc.sd ~ 1 +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model1)
# 
# model2 <- lmer(gwc.sd ~ subsidence +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model2)
# 
# model3 <- lmer(gwc.sd ~ subsidence + I(subsidence^2) +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = sub.moisture,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model3)
# 
# AIC(model1, model2, model3)
# 
# ranova(model3)
# 
# check model residuals of model3
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
# hist(sub.moisture$gwc.mean)

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
gwc.sd.plot

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
#        width = 6.5,
#        bg = 'white') # As of 9/24/21, with no updates to R, R packages, or OS, this started plotting with a black background... I have no idea what might have changed
# # ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/subsidence_moisture_linear_models.pdf',
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

### variable selection
# # choose between 1 week or 2 week precipitation sums
# model1 <- lmer(wtd ~ 1 +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = hydrology,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model1)
# model2 <- lmer(wtd ~ precip.1w +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = hydrology,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model2)
# model3 <- lmer(wtd ~ precip.2w +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = hydrology,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model3)
# AIC(model1, model2, model3)
# 
# # 2 week precip is better
# model1 <- lmer(wtd ~ precip.2w +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = hydrology,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model1)
# model2 <- lmer(wtd ~ precip.2w + subsidence +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = hydrology,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model2)
# model3 <- lmer(wtd ~ precip.2w*subsidence +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = hydrology,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model3)
# AIC(model1, model2, model3)
# 
# # include interaction between precip and subsidence
# model1 <- lmer(wtd ~ precip.2w*subsidence +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = hydrology,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model1)
# model2 <- lmer(wtd ~ precip.2w*subsidence + td +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = hydrology,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model2)
# model3 <- lmer(wtd ~ precip.2w*subsidence*td +
#                  (1 | block.f/fencegroup/wholeplot) + (1|time), REML = FALSE,
#                data = hydrology,
#                control=lmerControl(check.conv.singular="warning"))
# summary(model3)
# AIC(model1, model2, model3)
# 
# # test if random effects are necessary
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
# hist(sub.moisture$wtd.mean)

# # re-run with REML = TRUE
# all.wtd.model <- lmer(wtd ~ precip.2w*subsidence*td +
#                     (1 | block.f/fencegroup/wholeplot) + (1|time), REML = TRUE,
#                   data = hydrology,
#                   control=lmerControl(check.conv.singular="warning"))
# summary(all.wtd.model)
# saveRDS(all.wtd.model,
#         '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/all_wtd_model.rds')
all.wtd.model <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/all_wtd_model.rds')
summary(all.wtd.model)

# # calculate confidence intervals to look at fixed effects
# all.wtd.model.ci <- extract_ci(all.wtd.model)
# # write.csv(all.wtd.model.ci, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/all_wtd_coefficients.csv', row.names = FALSE)
all.wtd.model.ci <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/all_wtd_coefficients.csv')

all.wtd.model.r2 <- r.squaredGLMM(all.wtd.model)
wtd.r2.label <- paste0(as.character(expression('R'^2 ~ 'm = ')), ' ~ ', round(all.wtd.model.r2[1], 2))

# # make confidence interval data frame for graphing
# all.wtd.model.fit <- expand.grid(subsidence = seq(round(min(hydrology$subsidence)), round(max(hydrology$subsidence)), by = 5),
#                                  precip.2w = seq(round(min(hydrology$precip.2w)), round(max(hydrology$precip.2w)), by = 5),
#                                  td = seq(round(min(hydrology$td)), round(max(hydrology$td)), by = 5))
# 
# myStats <- function(model){
#   out <- predict( model, newdata=all.wtd.model.fit, re.form=~0 )
#   return(out)
# }
# 
# bootObj <- bootMer(all.wtd.model, FUN=myStats, nsim = 1000)
# all.wtd.model.fit <- cbind(all.wtd.model.fit, predict(all.wtd.model, newdata=all.wtd.model.fit, re.form=~0 )) %>%
#   cbind(confint( bootObj,  level=0.95 ))
# colnames(all.wtd.model.fit) <- c('subsidence', 'fit', 'lwr', 'upr')
# # write.csv(all.wtd.model.fit, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/all_wtd_model_fit.csv', row.names = FALSE)
all.wtd.model.fit <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/all_wtd_model_fit.csv')

hydrology <- hydrology %>%
  mutate(wtd.fit = ifelse(!is.na(wtd),
                          predict(all.wtd.model),
                          NA))
         
ggplot(hydrology, aes(x = precip.2w, y = wtd)) +
  geom_point(aes(color = factor(year))) +
  geom_line(aes(y = wtd.fit)) +
  facet_wrap(~plot.id, ncol = 8) +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Precipitation (cm)') +
  scale_y_continuous(name = 'WTD (cm)') +
  theme_bw() +
  theme(legend.title = element_blank())

ggplot(hydrology, aes(x = subsidence, y = wtd)) +
  geom_point(aes(color = factor(year))) +
  geom_line(aes(y = wtd.fit)) +
  facet_wrap(~plot.id, ncol = 8) +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = 'WTD (cm)') +
  theme_bw() +
  theme(legend.title = element_blank())

ggplot(hydrology, aes(x = td, y = wtd)) +
  geom_point(aes(color = factor(year))) +
  geom_line(aes(y = wtd.fit)) +
  facet_wrap(~plot.id, ncol = 8) +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_x_continuous(name = 'Thaw Depth (cm)') +
  scale_y_continuous(name = 'WTD (cm)') +
  theme_bw() +
  theme(legend.title = element_blank())

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
wtd[, wtd.date := ymd(paste(year, month, day, sep = '/'))]

# Format location data
ww.locations <- ww.locations %>%
  separate(Name, into = c('fence', 'well'), remove = FALSE, extra = 'merge') %>%
  mutate(fence = as.integer(str_sub(fence, start = 3)),
         well = as.numeric(well)) %>%
  filter(!is.na(well)) %>%
  select(fence, well) %>%
  st_zm()

# Join wtd and well locations
wtd <- left_join(wtd, ww.locations, by = c('fence', 'well')) %>%
  st_as_sf(crs = st_crs(ww.locations))

# Krige wtd surfaces
# use elevation as template for kriging
elev <- list(brick('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/AElevStack_filled_clipped.tif'),
             brick('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/BElevStack_filled_clipped.tif'),
             brick('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/CElevStack_filled_clipped.tif'))


### Format TD data
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

# Krige td surfaces
td.sf <- td %>%
  left_join(plots, by = c('fence', 'plot')) %>%
  st_as_sf(crs = crs(plots))

# create spatial objects containing the points for each block (once for sf objects, once for sp)
td.list <- list()
for(i in 1:length(unique(td.sf$block))) { # iterate over each block
  td.list[[i]] <- list()
  td.subset <- td %>%
    subset(block == as.character(unique(td.sf$block)[[i]]))
  for (j in 1:length(unique(td.subset$date))) { # iterate over each date
    print(paste('I: ', i, ', J: ', j))
    name <- paste(as.character(unique(td.sf$block)[[i]]), 
                  as.character(unique(td.subset$date)[[j]]), 
                  sep = '') # create a unique name for each block's output
    tmp <- td.sf %>%
      filter(block == as.character(unique(td.sf$block)[[i]]) & 
               date == unique(td.subset$date)[[j]]) %>% # select only the data from block i, date j
      st_zm(drop = TRUE, what = "ZM") %>% # get rid of Z and M geometries to convert to sp
      as('Spatial') # convert to sp
    td.list[[i]][[j]] <- tmp
    assign(name, td.list[[i]][[j]]) # name the sp object
    }
  rm(i, j, td.subset, name, tmp)
}

### start here ###

##############################Calculate and fit variograms##############################
#a2018
a2018.vgm <- variogram(Elevation~Easting+Northing, a2018sp, alpha = c(0:1)*90)
vgm.anis = vgm(0.025, "Exp", 5, 0, anis =  c(90, 0.8))
a2018.fit <- fit.variogram(a2018.vgm, model = vgm.anis)
plot(a2018.vgm, a2018.fit)

A2018grid <- A2017raster %>%
  as('SpatialPixelsDataFrame')

a2018k <-  krige(Elevation~1, a2018sp, A2018grid, a2018.fit)

ggplot(a2018) +
  geom_tile(data = as.data.frame(a2018k), aes(x,y, fill = var1.pred), inherit.aes = FALSE) +
  geom_sf(aes(colour = Elevation)) +
  scale_colour_viridis("Elevation") +
  ggtitle('a2018')

################################################################################