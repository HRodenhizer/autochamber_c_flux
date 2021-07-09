################################################################################
###                        Environmental Data Analysis                       ###
###                             code by HGR 7/2020                           ###
################################################################################

### Load Libraries #############################################################
library(data.table)
library(lubridate)
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
weather[, flux.year := fifelse(month >= 10,
                               year + 1,
                               year)]

# Neaten
weather <- weather[, .(flux.year, date, Tair, par, precip, rh)]
weather.daily <- weather[flux.year >= 2009,
                         .(tair.mean = mean(Tair, na.rm = TRUE),
                           tair.min = min(Tair, na.rm = TRUE),
                           tair.max = max(Tair, na.rm = TRUE),
                           precip = sum(precip, na.rm = TRUE)),
                         by = .(flux.year, date)]
weather.annual <- weather[flux.year >= 2009,
                   .(tair.mean = mean(Tair, na.rm = TRUE),
                     tair.min = min(Tair, na.rm = TRUE),
                     tair.max = max(Tair, na.rm = TRUE),
                     precip = sum(precip, na.rm = TRUE)),
                   by = .(flux.year)]

################################################################################

### PCA ########################################################################
# need to finalize which variables to include
env.annual.plot <- flux.annual %>%
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
pca.annual <- prcomp(as.matrix(env.annual.subset))

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
tair.anova <- lm(tair.mean ~ as.factor(flux.year), 
              data = weather.daily)
summary(tair.anova)
tair.contrast <- emmeans()

ggplot(weather.daily, aes(x = flux.year, y = tair.mean, group = flux.year)) +
  geom_boxplot() +
  
  scale_x_continuous(breaks = seq(2009, 2020))
################################################################################
