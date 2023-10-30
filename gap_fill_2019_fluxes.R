################################################################################
###                         Gap Fill 2019 Fluxes                             ###
###                          Code by HGR 11/2021                             ###
################################################################################

### Load Libraries #############################################################
library(data.table)
library(lubridate)
library(tidyverse)
library(viridis)
library(gbm)
################################################################################

### Load Data ##################################################################
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

### Gap Fill with GBM ##########################################################
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
#           '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_seasonal_filled_2019.csv',
#           row.names = FALSE)
################################################################################