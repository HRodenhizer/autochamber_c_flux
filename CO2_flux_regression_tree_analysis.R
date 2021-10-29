#############################################################################################################################
###                              Model Growing Season CO2 Fluxes using a Regression Tree                                  ###
###                                                code by HGR 2/2020                                                     ###
#############################################################################################################################

### Load Libraries ##########################################################################################################
library(gbm)
library(caret)
library(partykit)
library(data.table)
library(tidyverse)
#############################################################################################################################

### Load Data ###############################################################################################################
# flux.daily <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_daily_neat.csv")
flux.weekly <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_weekly_neat.csv")
flux.monthly <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_monthly.csv")
flux.monthly <- flux.monthly[flux.year >= 2010]
flux.seasonal <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_annual.csv")
flux.seasonal <- flux.seasonal[flux.year >= 2010]
#############################################################################################################################

### Gradient Boosted Regression Tree ########################################################################################
set.seed(21591)

### Seasonal
nee.seasonal <- flux.seasonal[!is.na(nee.sum), 
                              c('nee.sum',
                                'tp.annual',
                                'subsidence.annual',
                                'alt.annual', 
                                'vwc.mean', 'vwc.sd',
                                'gwc.mean', 'gwc.sd',
                                'wtd.mean',
                                'precip.sum', 'winter.snow.depth',
                                'winter.min.t10.min',
                                't10.mean', 't10.sd',
                                'tair.mean', 'tair.sd',
                                'biomass.annual')]
gpp.seasonal <- flux.seasonal[!is.na(gpp.sum),  
                              c('gpp.sum', 
                                'tp.annual',
                                'subsidence.annual',
                                'alt.annual', 
                                'max.vwc.max', 'vwc.mean', 'min.vwc.min',
                                'max.gwc.max', 'gwc.mean', 'min.gwc.min',
                                'wtd.mean',
                                'precip.sum', 'winter.snow.depth',
                                'winter.min.t10.min',
                                'max.t10.max', 't10.mean',
                                'max.tair.max', 'tair.mean',
                                'biomass.annual')]
reco.seasonal <- flux.seasonal[!is.na(reco.sum), 
                               c('reco.sum', 
                                 'tp.annual',
                                 'subsidence.annual',
                                 'alt.annual', 
                                 'max.vwc.max', 'vwc.mean', 'min.vwc.min',
                                 'max.gwc.max', 'gwc.mean', 'min.gwc.min',
                                 'wtd.mean',
                                 'precip.sum', 'winter.snow.depth',
                                 'winter.min.t10.min',
                                 'max.t10.max', 't10.mean',
                                 'max.tair.max', 'tair.mean',
                                 'biomass.annual')]

### Test covariance
nee.seasonal %>%
  GGally::ggpairs( upper=list(continuous='points'), lower=list(continuous='cor') )
reco.seasonal %>%
  GGally::ggpairs( upper=list(continuous='points'), lower=list(continuous='cor') )
gpp.seasonal %>%
  GGally::ggpairs( upper=list(continuous='points'), lower=list(continuous='cor') )

# set.seed doesn't seem to apply to sample
# train.nee.seasonal <- sample(1:nrow(nee.seasonal), 0.8*nrow(nee.seasonal))
# train.gpp.seasonal <- sample(1:nrow(gpp.seasonal), 0.8*nrow(gpp.seasonal))
# train.reco.seasonal <- sample(1:nrow(reco.seasonal), 0.8*nrow(reco.seasonal))
# saveRDS(train.nee.seasonal, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_nee_seasonal_80.rds')
# saveRDS(train.gpp.seasonal, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_gpp_seasonal_80.rds')
# saveRDS(train.reco.seasonal, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_reco_seasonal_80.rds')
train.nee.seasonal <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_nee_seasonal_80.rds')
train.gpp.seasonal <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_gpp_seasonal_80.rds')
train.reco.seasonal <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_reco_seasonal_80.rds')

# ### NEE Conditional Regression Tree
# # nee.seasonal.tree <- ctree(nee.sum~.,
# #                         data = nee.seasonal,
# #                         subset = train.nee.seasonal)
# # saveRDS(nee.seasonal.tree, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/nee_seasonal_tree.rds')
# nee.seasonal.tree <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/nee_seasonal_tree.rds')
# nee.seasonal.tree
# plot(nee.seasonal.tree)
# ### I can't figure out how to adjust the margins and size like this so am 
# # exporting manually once it looks ok on my screen
# # jpeg('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/nee_seasonal.jpeg',
# #      width = 6.5,
# #      height = 4,
# #      units = 'in',
# #      res = 300)
# # par(oma = c(1,1,1,1))
# # plot(nee.seasonal.tree)
# # dev.off()
# nee.seasonal.fit <- nee.seasonal[-train.nee.seasonal, fit := predict(nee.seasonal.tree, nee.seasonal[-train.nee.seasonal,])]
# ggplot(nee.seasonal.fit, aes(x = fit, y = nee.sum)) +
#   geom_point()

# NEE GBM
# # figure out good parameters to use
# grid <- expand.grid(.n.trees=seq(100, 500, by = 200),
#                   .interaction.depth=seq(1,4,by=1),
#                   .shrinkage=c(.001,.01,.1),
#                   .n.minobsinnode=10)
# control <- trainControl(method = "CV")
# gbm.train <- train(nee.sum~.,
#                    data = nee.seasonal[train.nee.seasonal,][complete.cases(nee.seasonal[train.nee.seasonal,]),],
#                    method = 'gbm',
#                    trControl = control,
#                    tuneGrid = grid)
# gbm.train
# # run model
# nee.seasonal.gbm <- gbm(nee.sum~.,
#                data = nee.seasonal[train.nee.seasonal,],
#                distribution = "gaussian",
#                n.trees = 100,
#                shrinkage = 0.1,
#                interaction.depth = 1)
# summary(nee.seasonal.gbm)
# # saveRDS(nee.seasonal.gbm, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/nee_seasonal_gbm.rds')
nee.seasonal.gbm <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/nee_seasonal_gbm.rds')
summary(nee.seasonal.gbm)

# Variable Influence Plot
nee.seasonal.influence <- nee.seasonal.gbm %>%
  summary() %>%
  as.data.frame() %>%
  arrange(rel.inf) %>%
  mutate(var = case_when(var == 'biomass.annual' ~ 'Biomass', 
                              var == 'vwc.mean' ~ 'Mean VWC', 
                              var == 't10.sd' ~ 'SD Soil Temp', 
                              var == 'tair.mean' ~ 'Mean Air Temp', 
                              var == 'gwc.sd' ~ 'SD GWC', 
                              var == 'winter.snow.depth' ~ 'Snow Depth', 
                              var == 'subsidence.annual' ~ 'Subsidence',
                              var == 'wtd.mean' ~ 'Mean WTD', 
                              var == 'tair.sd' ~ 'SD Air Temp', 
                              var == 'vwc.sd' ~ 'SD VWC', 
                              var == 'winter.min.t10.min' ~ 'Winter Min Soil Temp',  
                              var == 'tp.annual' ~ 'Thaw Penetration', 
                              var == 'alt.annual' ~ 'ALT', 
                              var == 't10.mean' ~ 'Mean Soil Temp', 
                              var == 'gwc.mean' ~ 'Mean GWC', 
                              var == 'precip.sum' ~ 'Precipitation')) %>%
  mutate(variable = factor(var, levels = .$var),
         response = 'nee',
         timescale = 'seasonal')
ggplot(nee.seasonal.influence, aes(x = rel.inf, y = variable)) +
  geom_col(fill = 'black') +
  scale_x_continuous(name = 'Relative Influence') +
  theme_bw() +
  theme(axis.title.y = element_blank())


nee.seasonal.pred <- nee.seasonal %>%
  slice(-1*train.nee.seasonal) %>%
  mutate(nee.pred = predict(nee.seasonal.gbm,
                            newdata = nee.seasonal[-train.nee.seasonal,],
                            n.trees = 100),
         nee.resid = nee.pred - nee.sum)
mean(nee.seasonal.pred$nee.resid^2)
ggplot(nee.seasonal.pred, aes(x = nee.sum, y = nee.pred)) +
  geom_point()
# plot(nee.seasonal.gbm, i = 'biomass.annual')
# pretty.gbm.tree(nee.seasonal.gbm, i.tree = 1)

# ### Reco
# # reco.seasonal.tree <- ctree(reco.sum~.,
# #                         data = reco.seasonal,
# #                         subset = train.reco.seasonal)
# # saveRDS(reco.seasonal.tree, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/reco_seasonal_tree.rds')
# reco.seasonal.tree <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/reco_seasonal_tree.rds')
# reco.seasonal.tree
# plot(reco.seasonal.tree)
# reco.seasonal.fit <- reco.seasonal[-train.reco.seasonal, fit := predict(reco.seasonal.tree, reco.seasonal[-train.reco.seasonal,])]
# ggplot(reco.seasonal.fit, aes(x = fit, y = reco.sum)) +
#   geom_point()

# Reco GBM
# # figure out good parameters to use
# gbm.train <- train(reco.sum~.,
#                    data = reco.seasonal[train.reco.seasonal,][complete.cases(reco.seasonal[train.reco.seasonal,]),],
#                    method = 'gbm',
#                    trControl = control,
#                    tuneGrid = grid)
# gbm.train
# # run model
# reco.seasonal.gbm <- gbm(reco.sum~., 
#                data = reco.seasonal[train.reco.seasonal,], 
#                distribution = "gaussian", 
#                n.trees = 500, 
#                shrinkage = 0.01, 
#                interaction.depth = 4)
# summary(reco.seasonal.gbm)
# saveRDS(reco.seasonal.gbm, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/reco_seasonal_gbm.rds')
reco.seasonal.gbm <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/reco_seasonal_gbm.rds')
reco.seasonal.pred <- reco.seasonal %>%
  slice(-1*train.reco.seasonal) %>%
  mutate(reco.pred = predict(reco.seasonal.gbm,
                            newdata = reco.seasonal[-train.reco.seasonal,],
                            n.trees = 100),
         reco.resid = reco.pred - reco.sum)
mean(reco.seasonal.pred$reco.resid^2)
ggplot(reco.seasonal.pred, aes(x = reco.sum, y = reco.pred)) +
  geom_point()
plot(reco.seasonal.gbm, i = 'alt.annual')
pretty.gbm.tree(reco.seasonal.gbm, i.tree = 1)

# ### GPP
# # gpp.seasonal.tree <- ctree(gpp.sum~.,
# #                         data = gpp.seasonal,
# #                         subset = train.gpp.seasonal)
# # saveRDS(gpp.seasonal.tree, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/gpp_seasonal_tree.rds')
# gpp.seasonal.tree <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/gpp_seasonal_tree.rds')
# gpp.seasonal.tree
# plot(gpp.seasonal.tree)
# gpp.seasonal.fit <- gpp.seasonal[-train.gpp.seasonal, fit := predict(gpp.seasonal.tree, gpp.seasonal[-train.gpp.seasonal,])]
# ggplot(gpp.seasonal.fit, aes(x = fit, y = gpp.sum)) +
#   geom_point()

# GPP GBM
# # figure out good parameters to use
# grid <- expand.grid(.n.trees=seq(500, 1000, by = 200),
#                     .interaction.depth=seq(1,4,by=1),
#                     .shrinkage=c(.001,.01,.1),
#                     .n.minobsinnode=10)
# gbm.train <- train(gpp.sum~.,
#                    data = gpp.seasonal[train.gpp.seasonal,][complete.cases(gpp.seasonal[train.gpp.seasonal,]),],
#                    method = 'gbm',
#                    trControl = control,
#                    tuneGrid = grid)
# gbm.train
# # run model
# gpp.seasonal.gbm <- gbm(gpp.sum~., 
#                 data = gpp.seasonal[train.gpp.seasonal,], 
#                 distribution = "gaussian", 
#                 n.trees = 500, 
#                 shrinkage = 0.01, 
#                 interaction.depth = 3)
# summary(gpp.gbm)
# # saveRDS(gpp.gbm, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/gpp_seasonal_gbm.rds')
gpp.seasonal.gbm <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/gpp_seasonal_gbm.rds')
gpp.seasonal.pred <- gpp.seasonal %>%
  slice(-1*train.gpp.seasonal) %>%
  mutate(gpp.pred = predict(gpp.seasonal.gbm,
                             newdata = gpp.seasonal[-train.gpp.seasonal,],
                             n.trees = 100),
         gpp.resid = gpp.pred - gpp.sum)
mean(gpp.seasonal.pred$gpp.resid^2)
ggplot(gpp.seasonal.pred, aes(x = gpp.sum, y = gpp.pred)) +
  geom_point()
plot(gpp.seasonal.gbm, i = 'biomass.annual')
pretty.gbm.tree(gpp.seasonal.gbm, i.tree = 1)


### Monthly
nee.monthly <- flux.monthly[!is.na(nee.sum), 
                            c('nee.sum',
                              'tp.to.date',
                              'subsidence.annual',
                              'td', 
                              'max.vwc.max', 'vwc.mean', 'min.vwc.min',
                              'max.gwc.max', 'gwc.mean', 'min.gwc.min',
                              'wtd.mean', 'wtd.sd',
                              'precip', 
                              't10.mean',
                              'max.vwc.max.2m', 'vwc.mean.2m', 'min.vwc.min.2m',
                              'max.gwc.max.2m', 'gwc.mean.2m', 'min.gwc.min.2m',
                              'gdd.2m', 'fdd.2m')]
gpp.monthly <- flux.monthly[!is.na(gpp.sum), 
                            c('gpp.sum',
                              'tp.to.date',
                              'subsidence.annual',
                              'td', 
                              'max.vwc.max', 'vwc.mean', 'min.vwc.min',
                              'max.gwc.max', 'gwc.mean', 'min.gwc.min',
                              'wtd.mean', 'wtd.sd',
                              'precip', 
                              't10.mean',
                              'max.vwc.max.2m', 'vwc.mean.2m', 'min.vwc.min.2m',
                              'max.gwc.max.2m', 'gwc.mean.2m', 'min.gwc.min.2m',
                              'gdd.2m', 'fdd.2m')]
reco.monthly <- flux.monthly[!is.na(reco.sum), 
                             c('reco.sum',
                               'tp.to.date',
                               'subsidence.annual',
                               'td', 
                               'max.vwc.max', 'vwc.mean', 'min.vwc.min',
                               'max.gwc.max', 'gwc.mean', 'min.gwc.min',
                               'wtd.mean', 'wtd.sd',
                               'precip', 
                               't10.mean',
                               'max.vwc.max.2m', 'vwc.mean.2m', 'min.vwc.min.2m',
                               'max.gwc.max.2m', 'gwc.mean.2m', 'min.gwc.min.2m',
                               'gdd.2m', 'fdd.2m')]
# set.seed doesn't seem to apply to sample
# train.nee.monthly <- sample(1:nrow(nee.monthly), 0.8*nrow(nee.monthly))
# train.gpp.monthly <- sample(1:nrow(gpp.monthly), 0.8*nrow(gpp.monthly))
# train.reco.monthly <- sample(1:nrow(reco.monthly), 0.8*nrow(reco.monthly))
# saveRDS(train.nee.monthly, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_nee_monthly_80.rds')
# saveRDS(train.gpp.monthly, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_gpp_monthly_80.rds')
# saveRDS(train.reco.monthly, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_reco_monthly_80.rds')
train.nee.monthly <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_nee_monthly_80.rds')
train.gpp.monthly <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_gpp_monthly_80.rds')
train.reco.monthly <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_reco_monthly_80.rds')

# # NEE
# # Should I remove some of the variables to simplify/focus more on hydrology and active layer?
# # Could remove air temp variables to see what happens.
# nee.monthly.tree <- ctree(nee.sum~.,
#                         data = nee.monthly,
#                         subset = train.nee.monthly)
# # saveRDS(nee.monthly.tree, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/nee_monthly_tree.rds')
# nee.monthly.tree <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/nee_monthly_tree.rds')
# nee.monthly.tree
# plot(nee.monthly.tree)
# nee.monthly.fit <- nee.monthly[-train.nee.monthly, fit := predict(nee.monthly.tree, nee.monthly[-train.nee.monthly,])]
# ggplot(nee.monthly.fit, aes(x = fit, y = nee.sum)) +
#   geom_point()

# NEE GBM
# figure out good parameters to use
gbm.train <- train(nee.sum~.,
                   data = nee.monthly[train.nee.monthly,][complete.cases(nee.monthly[train.nee.monthly,]),],
                   method = 'gbm',
                   trControl = control,
                   tuneGrid = grid)
gbm.train
# run model
nee.monthly.gbm <- gbm(nee.sum~., 
               data = nee.monthly[train.nee.monthly,], 
               distribution = "gaussian", 
               n.trees = 700, 
               shrinkage = 0.1, 
               interaction.depth = 4)
summary(nee.monthly.gbm)
# saveRDS(nee.monthly.gbm, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/nee_monthly_gbm.rds')
nee.monthly.pred <- nee.monthly %>%
  slice(-1*train.nee.monthly) %>%
  mutate(nee.pred = predict(nee.monthly.gbm,
                            newdata = nee.monthly[-train.nee.monthly,],
                            n.trees = 100),
         nee.resid = nee.pred - nee.sum)
mean(nee.monthly.pred$nee.resid^2)
ggplot(nee.monthly.pred, aes(x = nee.sum, y = nee.pred)) +
  geom_point()
plot(nee.monthly.gbm, i = 't10.mean')
pretty.gbm.tree(nee.monthly.gbm, i.tree = 1)

# # Reco
# # Should I remove some of the variables to simplify/focus more on hydrology and active layer?
# # Could remove air temp variables to see what happens.
# reco.monthly.tree <- ctree(reco.sum~.,
#                         data = reco.monthly,
#                         subset = train.reco.monthly)
# # saveRDS(reco.monthly.tree, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/reco_monthly_tree.rds')
# reco.monthly.tree <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/reco_monthly_tree.rds')
# reco.monthly.tree
# plot(reco.monthly.tree)
# reco.monthly.fit <- reco.monthly[-train.reco.monthly, fit := predict(reco.monthly.tree, reco.monthly[-train.reco.monthly,])]
# ggplot(reco.monthly.fit, aes(x = fit, y = reco.sum)) +
#   geom_point()

# Reco GBM
# figure out good parameters to use
gbm.train <- train(reco.sum~.,
                   data = reco.monthly[train.reco.monthly,][complete.cases(reco.monthly[train.reco.monthly,]),],
                   method = 'gbm',
                   trControl = control,
                   tuneGrid = grid)
gbm.train
# run model
reco.monthly.gbm <- gbm(reco.sum~., 
                       data = reco.monthly[train.reco.monthly,], 
                       distribution = "gaussian", 
                       n.trees = 900, 
                       shrinkage = 0.1, 
                       interaction.depth = 4)
summary(reco.monthly.gbm)
# saveRDS(reco.monthly.gbm, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/reco_monthly_gbm.rds')
reco.monthly.pred <- reco.monthly %>%
  slice(-1*train.reco.monthly) %>%
  mutate(reco.pred = predict(reco.monthly.gbm,
                            newdata = reco.monthly[-train.reco.monthly,],
                            n.trees = 100),
         reco.resid = reco.pred - reco.sum)
mean(reco.monthly.pred$reco.resid^2)
ggplot(reco.monthly.pred, aes(x = reco.sum, y = reco.pred)) +
  geom_point()
plot(reco.monthly.gbm, i = 't10.mean')
pretty.gbm.tree(reco.monthly.gbm, i.tree = 1)

# # GPP
# # Should I remove some of the variables to simplify/focus more on hydrology and active layer?
# # Could remove air temp variables to see what happens.
# gpp.monthly.tree <- ctree(gpp.sum~.,
#                         data = gpp.monthly,
#                         subset = train.gpp.monthly)
# # saveRDS(gpp.monthly.tree, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/gpp_monthly_tree.rds')
# gpp.monthly.tree <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/gpp_monthly_tree.rds')
# gpp.monthly.tree
# plot(gpp.monthly.tree)
# gpp.monthly.fit <- gpp.monthly[-train.gpp.monthly, fit := predict(gpp.monthly.tree, gpp.monthly[-train.gpp.monthly,])]
# ggplot(gpp.monthly.fit, aes(x = fit, y = gpp.sum)) +
#   geom_point()

# GPP GBM
# figure out good parameters to use
gbm.train <- train(gpp.sum~.,
                   data = gpp.monthly[train.gpp.monthly,][complete.cases(gpp.monthly[train.gpp.monthly,]),],
                   method = 'gbm',
                   trControl = control,
                   tuneGrid = grid)
gbm.train
# run model
gpp.monthly.gbm <- gbm(gpp.sum~., 
                       data = gpp.monthly[train.gpp.monthly,], 
                       distribution = "gaussian", 
                       n.trees = 900, 
                       shrinkage = 0.1, 
                       interaction.depth = 4)
summary(gpp.monthly.gbm)
# saveRDS(gpp.monthly.gbm, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/gpp_monthly_gbm.rds')
gpp.monthly.pred <- gpp.monthly %>%
  slice(-1*train.gpp.monthly) %>%
  mutate(gpp.pred = predict(gpp.monthly.gbm,
                            newdata = gpp.monthly[-train.gpp.monthly,],
                            n.trees = 100),
         gpp.resid = gpp.pred - gpp.sum)
mean(gpp.monthly.pred$gpp.resid^2)
ggplot(gpp.monthly.pred, aes(x = gpp.sum, y = gpp.pred)) +
  geom_point()
plot(gpp.monthly.gbm, i = 'max.vwc.max')
pretty.gbm.tree(gpp.monthly.gbm, i.tree = 1)



# ### Daily
# nee.daily <- flux.daily[!is.na(nee.sum), -c('date', 'year', 'flux.year',
#                                             'season', 'month', 'week', 'doy',
#                                             'block', 'fence', 'plot', 'plot.id',
#                                             'treatment', 'deployed', 'reco.sum',
#                                             'gpp.sum', 'alt.doy', 'ndvi.doy')]
# gpp.daily <- flux.daily[!is.na(gpp.sum),  -c('date', 'year', 'flux.year',
#                                              'season', 'month', 'week', 'doy',
#                                              'block', 'fence', 'plot', 'plot.id',
#                                              'treatment', 'deployed', 'reco.sum',
#                                              'nee.sum', 'alt.doy', 'ndvi.doy')]
# reco.daily <- flux.daily[!is.na(reco.sum),  -c('date', 'year', 'flux.year',
#                                                'season', 'month', 'week', 'doy',
#                                                'block', 'fence', 'plot', 'plot.id',
#                                                'treatment', 'deployed', 'nee.sum',
#                                                'gpp.sum', 'alt.doy', 'ndvi.doy')]
# # set.seed doesn't seem to apply to sample
# # train.nee.daily <- sample(1:nrow(nee.daily), 0.8*nrow(nee.daily))
# # train.gpp.daily <- sample(1:nrow(gpp.daily), 0.8*nrow(gpp.daily))
# # train.reco.daily <- sample(1:nrow(reco.daily), 0.8*nrow(reco.daily))
# # saveRDS(train.nee.daily, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_nee_daily_80.rds')
# # saveRDS(train.gpp.daily, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_gpp_daily_80.rds')
# # saveRDS(train.reco.daily, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_reco_daily_80.rds')
# train.nee.daily <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_nee_daily_80.rds')
# train.gpp.daily <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_gpp_daily_80.rds')
# train.reco.daily <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_reco_daily_80.rds')
# 
# # NEE 
# nee.daily.tree <- ctree(nee.sum~.,
#                         data = nee.daily,
#                         subset = train.nee.daily) # this produces a huge tree... don't try to plot
# nee.daily.tree
# # plot(nee.daily.tree)
# # saveRDS(nee.daily.tree, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/nee_daily_tree.rds')
# nee.daily.tree <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/nee_daily_tree.rds')
# nee.daily.fit <- nee.daily[-train.nee.daily, fit := predict(nee.daily.tree, nee.daily[-train.nee.daily,])]
# ggplot(nee.daily.fit, aes(x = fit, y = nee.sum)) +
#   geom_point()
# 
# # Reco
# reco.daily.tree <- ctree(reco.sum~.,
#                         data = reco.daily,
#                         subset = train.reco.daily) # this produces a huge tree... don't try to plot
# reco.daily.tree
# # plot(reco.daily.tree)
# # saveRDS(reco.daily.tree, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/reco_daily_tree.rds')
# reco.daily.tree <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/reco_daily_tree.rds')
# reco.daily.fit <- reco.daily[-train.reco.daily, fit := predict(reco.daily.tree, reco.daily[-train.reco.daily,])]
# ggplot(reco.daily.fit, aes(x = fit, y = reco.sum)) +
#   geom_point()
# 
# # GPP
# gpp.daily.tree <- ctree(gpp.sum~.,
#                         data = gpp.daily,
#                         subset = train.gpp.daily) # this produces a huge tree... don't try to plot
# gpp.daily.tree
# # plot(gpp.daily.tree)
# # saveRDS(gpp.daily.tree, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/gpp_daily_tree.rds')
# gpp.daily.tree <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/gpp_daily_tree.rds')
# gpp.daily.fit <- gpp.daily[-train.gpp.daily, fit := predict(gpp.daily.tree, gpp.daily[-train.gpp.daily,])]
# ggplot(gpp.daily.fit, aes(x = fit, y = gpp.sum)) +
#   geom_point()
# 
# ### Weekly
# # Should I limit the number of splits using a smaller alpha value?
# # Have to split the data set into training ahead of time, does not work with subset.
# # use:
# # control = ctree_control(alpha =10^-15)
# nee.weekly <- flux.weekly[!is.na(nee.sum), -c('year', 'flux.year',
#                                             'season', 'month', 'week',
#                                             'block', 'fence', 'plot', 'plot.id',
#                                             'treatment', 'reco.sum',
#                                             'gpp.sum', 'alt.doy', 'ndvi.doy')]
# gpp.weekly <- flux.weekly[!is.na(gpp.sum),  -c('year', 'flux.year',
#                                                'season', 'month', 'week',
#                                                'block', 'fence', 'plot', 'plot.id',
#                                                'treatment', 'reco.sum',
#                                                'nee.sum', 'alt.doy', 'ndvi.doy')]
# reco.weekly <- flux.weekly[!is.na(reco.sum),  -c('year', 'flux.year',
#                                                  'season', 'month', 'week',
#                                                  'block', 'fence', 'plot', 'plot.id',
#                                                  'treatment', 'nee.sum',
#                                                  'gpp.sum', 'alt.doy', 'ndvi.doy')]
# # set.seed doesn't seem to apply to sample
# # train.nee.weekly <- sample(1:nrow(nee.weekly), 0.8*nrow(nee.weekly))
# # train.gpp.weekly <- sample(1:nrow(gpp.weekly), 0.8*nrow(gpp.weekly))
# # train.reco.weekly <- sample(1:nrow(reco.weekly), 0.8*nrow(reco.weekly))
# # saveRDS(train.nee.weekly, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_nee_weekly_80.rds')
# # saveRDS(train.gpp.weekly, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_gpp_weekly_80.rds')
# # saveRDS(train.reco.weekly, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_reco_weekly_80.rds')
# train.nee.weekly <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_nee_weekly_80.rds')
# train.gpp.weekly <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_gpp_weekly_80.rds')
# train.reco.weekly <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/train_reco_weekly_80.rds')
# 
# # NEE
# nee.weekly.subset <- nee.weekly[train.nee.weekly]
# nee.weekly.tree <- ctree(nee.sum~.,
#                         data = nee.weekly.subset,
#                         control = ctree_control(alpha =10^-15)) # this produces a huge tree... don't try to plot
# nee.weekly.tree
# # plot(nee.weekly.tree)
# # saveRDS(nee.weekly.tree, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/nee_weekly_tree.rds')
# 
# # this gives slightly different values every time
# nee.imp <- data.frame(Importance = varimp(nee.weekly.tree)) %>%
#   rownames_to_column(var = 'Variable') %>%
#   arrange(Importance) %>%
#   mutate(Variable = factor(Variable))
# 
# ggplot(nee.imp, aes(x = Importance, y = fct_reorder(Variable,
#                                          Importance))) +
#   geom_col()
# nee.weekly.fit <- nee.weekly[-train.nee.weekly, fit := predict(nee.weekly.tree, nee.weekly[-train.nee.weekly,])]
# ggplot(nee.weekly.fit, aes(x = fit, y = nee.sum)) +
#   geom_point()
# 
# # Reco
# reco.weekly.subset <- reco.weekly[train.reco.weekly]
# reco.weekly.tree <- ctree(reco.sum~.,
#                          data = reco.weekly.subset,
#                          control = ctree_control(alpha =10^-15)) # lowering alpha results in fewer splits
# reco.weekly.tree
# plot(reco.weekly.tree)
# # saveRDS(reco.weekly.tree, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/reco_weekly_tree.rds')
# varimp(reco.weekly.tree)
# reco.weekly.fit <- reco.weekly[-train.reco.weekly, fit := predict(reco.weekly.tree, reco.weekly[-train.reco.weekly,])]
# ggplot(reco.weekly.fit, aes(x = fit, y = reco.sum)) +
#   geom_point()
# 
# # GPP
# gpp.weekly.subset <- gpp.weekly[train.gpp.weekly]
# gpp.weekly.tree <- ctree(gpp.sum~.,
#                         data = gpp.weekly.subset,
#                         control = ctree_control(alpha =10^-15)) # this produces a huge tree... don't try to plot
# gpp.weekly.tree
# plot(gpp.weekly.tree)
# # saveRDS(gpp.weekly.tree, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/gpp_weekly_tree.rds')
# varimp(gpp.weekly.tree)
# gpp.weekly.fit <- gpp.weekly[-train.gpp.weekly, fit := predict(gpp.weekly.tree, gpp.weekly[-train.gpp.weekly,])]
# ggplot(gpp.weekly.fit, aes(x = fit, y = gpp.sum)) +
#   geom_point()

