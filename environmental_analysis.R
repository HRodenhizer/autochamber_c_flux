################################################################################
###                        Environmental Data Analysis                       ###
###                             code by HGR 7/2020                           ###
################################################################################

### Load Libraries #############################################################
library(tidyverse)
################################################################################

### Load Data ##################################################################
flux.monthly <- read.csv("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_monthly.csv")
flux.annual <- read.csv("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_annual.csv")
################################################################################

### PCA ########################################################################
# need to finalize which variables to include
env.annual <- flux.annual %>%
  select(-c('flux.year', 'block', 'fence', 'plot', 'plot.id', 'treatment',
            'season', matches('sum'), matches('rh'), matches('sd'),
            'max.tair.spread', 'min.tair.spread'))
pca.annual <- prcomp(flux.annual)