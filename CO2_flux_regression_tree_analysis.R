#############################################################################################################################
###                              Model Growing Season CO2 Fluxes using a Regression Tree                                  ###
###                                                code by HGR 2/2020                                                     ###
#############################################################################################################################

### Load Libraries ##########################################################################################################
# library(tree)
library(partykit)
library(data.table)
library(tidyverse)
#############################################################################################################################

### Load Data ###############################################################################################################
flux.daily <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_daily_neat.csv")
flux.weekly <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_weekly_neat.csv")
flux.annual <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_annual.csv")
#############################################################################################################################

### Gradient Boosted Regression Tree ########################################################################################
set.seed(21591)

### Daily
nee.daily <- flux.daily[!is.na(nee.sum), -c('date', 'year', 'flux.year',
                                            'season', 'month', 'week', 'doy',
                                            'block', 'fence', 'plot', 'plot.id',
                                            'treatment', 'deployed', 'reco.sum',
                                            'gpp.sum', 'alt.doy', 'ndvi.doy')]
gpp.daily <- flux.daily[!is.na(gpp.sum),  -c('date', 'year', 'flux.year',
                                             'season', 'month', 'week', 'doy',
                                             'block', 'fence', 'plot', 'plot.id',
                                             'treatment', 'deployed', 'reco.sum',
                                             'nee.sum', 'alt.doy', 'ndvi.doy')]
reco.daily <- flux.daily[!is.na(reco.sum),  -c('date', 'year', 'flux.year',
                                               'season', 'month', 'week', 'doy',
                                               'block', 'fence', 'plot', 'plot.id',
                                               'treatment', 'deployed', 'nee.sum',
                                               'gpp.sum', 'alt.doy', 'ndvi.doy')]
# set.seed doesn't seem to apply to sample
# train.nee.daily <- sample(1:nrow(nee.daily), 0.8*nrow(nee.daily))
# train.gpp.daily <- sample(1:nrow(gpp.daily), 0.8*nrow(gpp.daily))
# train.reco.daily <- sample(1:nrow(reco.daily), 0.8*nrow(reco.daily))
# saveRDS(train.nee.daily, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_nee_daily_80.rds')
# saveRDS(train.gpp.daily, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_gpp_daily_80.rds')
# saveRDS(train.reco.daily, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_reco_daily_80.rds')
train.nee.daily <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_nee_daily_80.rds')
train.gpp.daily <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_gpp_daily_80.rds')
train.reco.daily <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_reco_daily_80.rds')

# NEE 
nee.daily.tree <- ctree(nee.sum~.,
                        data = nee.daily,
                        subset = train.nee.daily) # this produces a huge tree... don't try to plot
nee.daily.tree
# plot(nee.daily.tree)
# saveRDS(nee.daily.tree, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/nee_daily_tree.rds')
nee.daily.tree <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/nee_daily_tree.rds')
nee.daily.fit <- nee.daily[-train.nee.daily, fit := predict(nee.daily.tree, nee.daily[-train.nee.daily,])]
ggplot(nee.daily.fit, aes(x = fit, y = nee.sum)) +
  geom_point()

# Reco
reco.daily.tree <- ctree(reco.sum~.,
                        data = reco.daily,
                        subset = train.reco.daily) # this produces a huge tree... don't try to plot
reco.daily.tree
# plot(reco.daily.tree)
# saveRDS(reco.daily.tree, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/reco_daily_tree.rds')
reco.daily.tree <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/reco_daily_tree.rds')
reco.daily.fit <- reco.daily[-train.reco.daily, fit := predict(reco.daily.tree, reco.daily[-train.reco.daily,])]
ggplot(reco.daily.fit, aes(x = fit, y = reco.sum)) +
  geom_point()

# GPP
gpp.daily.tree <- ctree(gpp.sum~.,
                        data = gpp.daily,
                        subset = train.gpp.daily) # this produces a huge tree... don't try to plot
gpp.daily.tree
# plot(gpp.daily.tree)
# saveRDS(gpp.daily.tree, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/gpp_daily_tree.rds')
gpp.daily.tree <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/gpp_daily_tree.rds')
gpp.daily.fit <- gpp.daily[-train.gpp.daily, fit := predict(gpp.daily.tree, gpp.daily[-train.gpp.daily,])]
ggplot(gpp.daily.fit, aes(x = fit, y = gpp.sum)) +
  geom_point()

### Weekly
# Should I limit the number of splits using a smaller alpha value?
# Have to split the data set into training ahead of time, does not work with subset.
# use:
# control = ctree_control(alpha =10^-15)
nee.weekly <- flux.weekly[!is.na(nee.sum), -c('year', 'flux.year',
                                            'season', 'month', 'week',
                                            'block', 'fence', 'plot', 'plot.id',
                                            'treatment', 'reco.sum',
                                            'gpp.sum', 'alt.doy', 'ndvi.doy')]
gpp.weekly <- flux.weekly[!is.na(gpp.sum),  -c('year', 'flux.year',
                                               'season', 'month', 'week',
                                               'block', 'fence', 'plot', 'plot.id',
                                               'treatment', 'reco.sum',
                                               'nee.sum', 'alt.doy', 'ndvi.doy')]
reco.weekly <- flux.weekly[!is.na(reco.sum),  -c('year', 'flux.year',
                                                 'season', 'month', 'week',
                                                 'block', 'fence', 'plot', 'plot.id',
                                                 'treatment', 'nee.sum',
                                                 'gpp.sum', 'alt.doy', 'ndvi.doy')]
# set.seed doesn't seem to apply to sample
# train.nee.weekly <- sample(1:nrow(nee.weekly), 0.8*nrow(nee.weekly))
# train.gpp.weekly <- sample(1:nrow(gpp.weekly), 0.8*nrow(gpp.weekly))
# train.reco.weekly <- sample(1:nrow(reco.weekly), 0.8*nrow(reco.weekly))
# saveRDS(train.nee.weekly, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_nee_weekly_80.rds')
# saveRDS(train.gpp.weekly, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_gpp_weekly_80.rds')
# saveRDS(train.reco.weekly, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_reco_weekly_80.rds')
train.nee.weekly <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_nee_weekly_80.rds')
train.gpp.weekly <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_gpp_weekly_80.rds')
train.reco.weekly <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_reco_weekly_80.rds')

# NEE
nee.weekly.subset <- nee.weekly[train.nee.weekly]
nee.weekly.tree <- ctree(nee.sum~.,
                        data = nee.weekly.subset,
                        control = ctree_control(alpha =10^-15)) # this produces a huge tree... don't try to plot
nee.weekly.tree
# plot(nee.weekly.tree)
# saveRDS(nee.weekly.tree, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/nee_weekly_tree.rds')

# this gives slightly different values every time
nee.imp <- data.frame(Importance = varimp(nee.weekly.tree)) %>%
  rownames_to_column(var = 'Variable') %>%
  arrange(Importance) %>%
  mutate(Variable = factor(Variable))

ggplot(nee.imp, aes(x = Importance, y = fct_reorder(Variable,
                                         Importance))) +
  geom_col()
nee.weekly.fit <- nee.weekly[-train.nee.weekly, fit := predict(nee.weekly.tree, nee.weekly[-train.nee.weekly,])]
ggplot(nee.weekly.fit, aes(x = fit, y = nee.sum)) +
  geom_point()

# Reco
reco.weekly.subset <- reco.weekly[train.reco.weekly]
reco.weekly.tree <- ctree(reco.sum~.,
                         data = reco.weekly.subset,
                         control = ctree_control(alpha =10^-15)) # lowering alpha results in fewer splits
reco.weekly.tree
plot(reco.weekly.tree)
# saveRDS(reco.weekly.tree, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/reco_weekly_tree.rds')
varimp(reco.weekly.tree)
reco.weekly.fit <- reco.weekly[-train.reco.weekly, fit := predict(reco.weekly.tree, reco.weekly[-train.reco.weekly,])]
ggplot(reco.weekly.fit, aes(x = fit, y = reco.sum)) +
  geom_point()

# GPP
gpp.weekly.subset <- gpp.weekly[train.gpp.weekly]
gpp.weekly.tree <- ctree(gpp.sum~.,
                        data = gpp.weekly.subset,
                        control = ctree_control(alpha =10^-15)) # this produces a huge tree... don't try to plot
gpp.weekly.tree
plot(gpp.weekly.tree)
# saveRDS(gpp.weekly.tree, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/gpp_weekly_tree.rds')
varimp(gpp.weekly.tree)
gpp.weekly.fit <- gpp.weekly[-train.gpp.weekly, fit := predict(gpp.weekly.tree, gpp.weekly[-train.gpp.weekly,])]
ggplot(gpp.weekly.fit, aes(x = fit, y = gpp.sum)) +
  geom_point()

