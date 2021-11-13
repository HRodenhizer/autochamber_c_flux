#############################################################################################################################
###                              Model Growing Season CO2 Fluxes using a Regression Tree                                  ###
###                                                code by HGR 2/2020                                                     ###
#############################################################################################################################

### Load Libraries ##########################################################################################################
library(gbm)
library(caret)
library(partykit)
library(lime)
library(data.table)
library(lubridate)
library(viridis)
library(ggpubr)
library(ggnewscale)
library(tidyverse)
#############################################################################################################################

### Load Data ###############################################################################################################
flux.monthly <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_monthly.csv")
flux.monthly <- flux.monthly[flux.year >= 2010]
flux.monthly[, treatment := factor(treatment,
                                   levels = c('Control',
                                              'Air Warming',
                                              'Soil Warming',
                                              'Air + Soil Warming'))]
flux.seasonal <- fread("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_annual.csv")
flux.seasonal <- flux.seasonal[flux.year >= 2010]
flux.seasonal[, ':=' (tp.annual = tp, alt.annual = alt)]
flux.seasonal[, treatment := factor(treatment,
                                    levels = c('Control',
                                               'Air Warming',
                                               'Soil Warming',
                                               'Air + Soil Warming'))]
newgroups <- as.data.table(read.table("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/Logistics/LabMeetings/Fall2018/2018_datajam_groupings.csv",
                                      sep=",", dec=".", header=TRUE))
newgroups.join <- newgroups[!is.na(as.numeric(plot)), .(ID, fence, plot)]
newgroups.join[, plot := as.integer(plot)]
newgroups.join[, ID := factor(str_to_title(ID), levels = c('Shallow Dry', 'Deep Dry', 'Deep Wet'))]
flux.monthly <- merge(flux.monthly, newgroups.join, by = c('fence', 'plot'), all.x = TRUE)
flux.seasonal <- merge(flux.seasonal, newgroups.join, by = c('fence', 'plot'), all.x = TRUE)
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
                                'vwc.mean', 'vwc.sd',
                                'gwc.mean', 'gwc.sd',
                                'wtd.mean',
                                'precip.sum', 'winter.snow.depth',
                                'winter.min.t10.min',
                                't10.mean', 't10.sd',
                                'tair.mean', 'tair.sd',
                                'biomass.annual')]
reco.seasonal <- flux.seasonal[!is.na(reco.sum), 
                               c('reco.sum', 
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

# ### Test covariance
# nee.seasonal %>%
#   GGally::ggpairs( upper=list(continuous='points'), lower=list(continuous='cor') )
# reco.seasonal %>%
#   GGally::ggpairs( upper=list(continuous='points'), lower=list(continuous='cor') )
# gpp.seasonal %>%
#   GGally::ggpairs( upper=list(continuous='points'), lower=list(continuous='cor') )

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

# ### NEE GBM
# # figure out good parameters to use
# grid <- expand.grid(.n.trees=seq(200, 800, by = 200),
#                   .interaction.depth=seq(1,6,by=1),
#                   .shrinkage=c(.001,.01,.1),
#                   .n.minobsinnode=c(5, 10))
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
#                n.trees = 600,
#                shrinkage = 0.01,
#                interaction.depth = 6,
#                n.minobsinnode = 5)
# summary(nee.seasonal.gbm)
# saveRDS(nee.seasonal.gbm, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/nee_seasonal_gbm.rds')
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
         var = factor(seq(1, n())),
         response = 'nee',
         timescale = 'seasonal')

# plot relative influence
ggplot(nee.seasonal.influence, aes(x = rel.inf, y = variable)) +
  geom_col(fill = 'black') +
  scale_x_continuous(name = 'Relative Influence',
                     expand = expansion(mult = c(0, .05))) +
  theme_bw() +
  theme(axis.title.y = element_blank())

# determine model performance with a linear model on the test data
nee.seasonal.pred <- nee.seasonal %>%
  slice(-1*train.nee.seasonal) %>%
  mutate(nee.pred = predict(nee.seasonal.gbm,
                            newdata = nee.seasonal[-train.nee.seasonal,],
                            n.trees = 100),
         nee.resid = nee.pred - nee.sum,
         response = 'nee',
         timescale = 'seasonal')
mean(nee.seasonal.pred$nee.resid^2)
nee.seasonal.lm <- lm(nee.pred ~ nee.sum,
                      data = nee.seasonal.pred)
summary(nee.seasonal.lm)
nee.seasonal.r2 <- summary(nee.seasonal.lm)$r.squared
nee.seasonal.r2.label <- paste0(as.character(expression('R'^2 ~ ' = ')), ' ~ ', round(nee.seasonal.r2[1], 2))

nee.seasonal.fit.plot <- ggplot(nee.seasonal.pred, aes(x = nee.sum, y = nee.pred)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'black') +
  geom_text(x = -200, y = 150, label = nee.seasonal.r2.label,
            hjust = 0,
            vjust = 0,
            parse = TRUE) +
  scale_x_continuous(name = expression('Measured NEE (gC m'^-2*')')) +
  scale_y_continuous(name = expression('Predicted NEE (gC m'^-2*')')) +
  theme_bw()
nee.seasonal.fit.plot

# plot partial dependence plots of top predictors (with real data points underneath)
plot.pdp <- function(df1, df2, predictor, response, color.var, shape.var) {
  # print(paste('Running predictor.name <- xxx'))
  predictor.name <- case_when(predictor == 'biomass.annual' ~ expression('Biomass (g m'^-2*')'), 
                              predictor == 'tair.mean' ~ expression('Mean Air Temp ('*degree*'C)'), 
                              predictor == 'tair.sd' ~ expression('SD Air Temp ('*degree*'C)'), 
                              predictor == 'wtd.mean' ~ expression('Mean WTD (cm)'), 
                              predictor == 'vwc.mean' ~ expression('Mean VWC (%)'), 
                              predictor == 'vwc.sd' ~ expression('SD VWC (%)'), 
                              predictor == 'gwc.mean' ~ expression('Mean GWC (%)'), 
                              predictor == 'gwc.sd' ~ expression('SD GWC(%)'), 
                              predictor == 't10.mean' ~ expression('Mean Soil Temp ('*degree*'C)'), 
                              predictor == 't10.sd' ~ expression('SD Soil Temp ('*degree*'C)'), 
                              predictor == 'winter.min.t10.min' ~ expression('Winter Min Soil Temp ('*degree*'C)'),  
                              predictor == 'subsidence.annual' ~ expression('Subsidence (cm)'),
                              predictor == 'tp.annual' ~ expression('Thaw Penetration (cm)'), 
                              predictor == 'alt.annual' ~ expression('ALT (cm)'), 
                              predictor == 'winter.snow.depth' ~ expression('Snow Depth (cm)'), 
                              predictor == 'precip.sum' ~ expression('Precipitation (mm)'))
  # print(paste('Running response.name <- xxx'))
  response.name <- case_when(response == 'nee.sum' ~ expression('NEE (gC m'^-2*')'),
                             response == 'gpp.sum' ~ expression('GPP (gC m'^-2*')'),
                             response == 'reco.sum' ~ expression('Reco (gC m'^-2*')'))
  # print(paste('Running color.name <- xxx'))
  color.name <- case_when(color.var == 'flux.year' ~ 'Year',
                          color.var == 'month' ~ 'Month')
  # print(paste('Running color.limits <- xxx'))
  color.limits <- case_when(color.var == 'flux.year' ~ c(2010, 2020),
                            color.var == 'month' ~ c(5, 9))
  # print(paste('Running color.breaks <- xxx'))
  color.breaks <- if (color.var == 'flux.year') {
    seq(2010, 2020, by = 2)
  } else if (color.var == 'month') {
    seq(5, 9)
  }
  # print(paste('Running color.breaks <- xxx'))
  color.labels <- if (color.var == 'flux.year') {
    seq(2010, 2020, by = 2)
  } else if (color.var == 'month') {
    month.name[seq(5, 9)]
  }
  shape.values <- if (shape.var == 'treatment') {
    c(1, 0, 16, 15)
  } else if (shape.var == 'ID') {
    c(1, 16, 15)
  }
  # print(paste('Running color.limits <- xxx'))
  plot <- ggplot(filter(df1, !is.na(get(response)) & !is.na(get(predictor))), 
                 aes_string(x = predictor)) +
    geom_point(aes_string(y = response, color = color.var, shape = shape.var)) +
    scale_color_viridis(name = color.name,
                        limits = color.limits,
                        breaks = color.breaks) +
    scale_shape_manual(values = shape.values,
                       guide = guide_legend(order = 2)) +
    new_scale('color') +
    geom_line(data = df2, 
              aes(y = yhat, color = 'Marginal Effect')) +
    scale_color_manual(breaks = c('Marginal Effect'),
                       values = c('black'),
                       guide = guide_legend(order = 1)) +
    scale_x_continuous(name = predictor.name) +
    scale_y_continuous(name = response.name) +
    theme_bw() +
    theme(legend.title = element_blank())
  
  return(plot)
}

example.plots.seasonal <- flux.seasonal %>%
  filter(fence == 4 & plot == 1 | fence == 4 & plot == 6 | fence == 1 & plot == 5)

nee.seasonal.pd.biomass <- nee.seasonal.gbm %>%
  pdp::partial(pred.var = "biomass.annual", n.trees = nee.seasonal.gbm$n.trees,
               grid.resolution = 100)
nee.seasonal.plot.1 <- plot.pdp(df1 = flux.seasonal, df2 = nee.seasonal.pd.biomass,
                                predictor = 'biomass.annual', response = 'nee.sum',
                                color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.seasonal,
             aes(x = biomass.annual, y = nee.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.seasonal,
            aes(x = biomass.annual, y = nee.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7)# +
# geom_text(data = example.plots.seasonal,
#           aes(x = biomass.annual, y = nee.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
nee.seasonal.plot.1

nee.seasonal.pd.vwc.mean <- nee.seasonal.gbm %>%
  pdp::partial(pred.var = "vwc.mean", n.trees = nee.seasonal.gbm$n.trees,
               grid.resolution = 100)
nee.seasonal.plot.2 <- plot.pdp(df1 = flux.seasonal, df2 = nee.seasonal.pd.vwc.mean,
                                predictor = 'vwc.mean', response = 'nee.sum',
                                color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.seasonal,
             aes(x = vwc.mean, y = nee.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.seasonal,
            aes(x = vwc.mean, y = nee.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7)# +
# geom_text(data = example.plots.seasonal,
#           aes(x = vwc.mean, y = nee.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
nee.seasonal.plot.2

nee.seasonal.pd.gwc.sd <- nee.seasonal.gbm %>%
  pdp::partial(pred.var = "gwc.sd", n.trees = nee.seasonal.gbm$n.trees,
               grid.resolution = 100)
nee.seasonal.plot.3 <- plot.pdp(df1 = flux.seasonal, df2 = nee.seasonal.pd.gwc.sd,
                                predictor = 'gwc.sd', response = 'nee.sum',
                                color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.seasonal,
             aes(x = gwc.sd, y = nee.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.seasonal,
            aes(x = gwc.sd, y = nee.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7)# +
# geom_text(data = example.plots.seasonal,
#           aes(x = gwc.sd, y = nee.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
nee.seasonal.plot.3

nee.seasonal.pd.gwc.mean <- nee.seasonal.gbm %>%
  pdp::partial(pred.var = "gwc.mean", n.trees = nee.seasonal.gbm$n.trees,
               grid.resolution = 100)
nee.seasonal.plot.4 <- plot.pdp(df1 = flux.seasonal, df2 = nee.seasonal.pd.gwc.mean,
                                predictor = 'gwc.mean', response = 'nee.sum',
                                color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.seasonal,
             aes(x = gwc.mean, y = nee.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.seasonal,
            aes(x = gwc.mean, y = nee.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7)# +
# geom_text(data = example.plots.seasonal,
#           aes(x = gwc.mean, y = nee.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
nee.seasonal.plot.4

nee.seasonal.pd.plot <- ggarrange(nee.seasonal.plot.1,
                                  nee.seasonal.plot.2,
                                  nee.seasonal.plot.3,
                                  nee.seasonal.plot.4,
                                  nrow = 2,
                                  ncol = 2,
                                  common.legend = TRUE,
                                  legend = 'right',
                                  labels = seq(1, 4))
nee.seasonal.pd.plot

# explore a few points using LIME
lime.points <- flux.seasonal[-train.nee.seasonal, ][(fence == 1 & plot == 5 | 
                                                       fence == 4 & plot %in% c(1, 6)) &
                                                      flux.year == 2019,]
explainer <- lime(nee.seasonal[train.nee.seasonal,], nee.seasonal.gbm)
explanation <- lime::explain(lime.points, explainer, n_features = 5)

# ### Reco GBM
# # figure out good parameters to use
# grid <- expand.grid(.n.trees=seq(200, 800, by = 200),
#                   .interaction.depth=seq(1,6,by=1),
#                   .shrinkage=c(.001,.01,.1),
#                   .n.minobsinnode=c(5, 10))
# gbm.train <- train(reco.sum~.,
#                    data = reco.seasonal[train.reco.seasonal,][complete.cases(reco.seasonal[train.reco.seasonal,]),],
#                    method = 'gbm',
#                    trControl = control,
#                    tuneGrid = grid)
# gbm.train
# # run model
# reco.seasonal.gbm <- gbm(reco.sum~.,
#                          data = reco.seasonal[train.reco.seasonal,],
#                          distribution = "gaussian",
#                          n.trees = 800,
#                          shrinkage = 0.1,
#                          interaction.depth = 5,
#                          n.minobsinnode = 5)
# summary(reco.seasonal.gbm)
# saveRDS(reco.seasonal.gbm, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/reco_seasonal_gbm.rds')
reco.seasonal.gbm <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/reco_seasonal_gbm.rds')
reco.seasonal.influence <- reco.seasonal.gbm %>%
  summary() %>%
  as.data.frame() %>%
  arrange(rel.inf) %>%
  mutate(var = case_when(var == 'alt.annual' ~ 'ALT', 
                         var == 'tair.sd' ~ 'SD Air Temp', 
                         var == 'subsidence.annual' ~ 'Subsidence',
                         var == 'biomass.annual' ~ 'Biomass', 
                         var == 't10.sd' ~ 'SD Soil Temp', 
                         var == 'gwc.sd' ~ 'SD GWC', 
                         var == 'vwc.mean' ~ 'Mean VWC', 
                         var == 'wtd.mean' ~ 'Mean WTD', 
                         var == 'gwc.mean' ~ 'Mean GWC', 
                         var == 't10.mean' ~ 'Mean Soil Temp', 
                         var == 'vwc.sd' ~ 'SD VWC', 
                         var == 'winter.snow.depth' ~ 'Snow Depth', 
                         var == 'tp.annual' ~ 'Thaw Penetration', 
                         var == 'tair.mean' ~ 'Mean Air Temp', 
                         var == 'winter.min.t10.min' ~ 'Winter Min Soil Temp',  
                         var == 'precip.sum' ~ 'Precipitation')) %>%
  mutate(variable = factor(var, levels = .$var),
         var = factor(seq(1, n())),
         response = 'reco',
         timescale = 'seasonal')
ggplot(reco.seasonal.influence, aes(x = rel.inf, y = variable)) +
  geom_col(fill = 'black') +
  scale_x_continuous(name = 'Relative Influence',
                     expand = expansion(mult = c(0, .05))) +
  theme_bw() +
  theme(axis.title.y = element_blank())


reco.seasonal.pred <- reco.seasonal %>%
  slice(-1*train.reco.seasonal) %>%
  mutate(reco.pred = predict(reco.seasonal.gbm,
                            newdata = reco.seasonal[-train.reco.seasonal,],
                            n.trees = 100),
         reco.resid = reco.pred - reco.sum,
         response = 'reco',
         timescale = 'seasonal')
mean(reco.seasonal.pred$reco.resid^2)

reco.seasonal.lm <- lm(reco.pred ~ reco.sum,
                      data = reco.seasonal.pred)
summary(reco.seasonal.lm)
reco.seasonal.r2 <- summary(reco.seasonal.lm)$r.squared
reco.seasonal.r2.label <- paste0(as.character(expression('R'^2 ~ ' = ')), ' ~ ', round(reco.seasonal.r2[1], 2))

reco.seasonal.fit.plot <- ggplot(reco.seasonal.pred, aes(x = reco.sum, y = reco.pred)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'black') +
  geom_text(x = 200, y = 575, label = reco.seasonal.r2.label,
            hjust = 0,
            vjust = 1,
            parse = TRUE) +
  scale_x_continuous(name = expression('Measured Reco (gC m'^-2*')')) +
  scale_y_continuous(name = expression('Predicted Reco (gC m'^-2*')')) +
  theme_bw()
reco.seasonal.fit.plot

# plot partial dependence plots on top of data points
reco.seasonal.pd.biomass <- reco.seasonal.gbm %>%
  pdp::partial(pred.var = "biomass.annual", n.trees = reco.seasonal.gbm$n.trees,
               grid.resolution = 100)
reco.seasonal.plot.1 <- plot.pdp(df1 = flux.seasonal, df2 = reco.seasonal.pd.biomass,
                                predictor = 'biomass.annual', response = 'reco.sum',
                                color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.seasonal,
             aes(x = biomass.annual, y = reco.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.seasonal,
            aes(x = biomass.annual, y = reco.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7)# +
# geom_text(data = example.plots.seasonal,
#           aes(x = biomass.annual, y = reco.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
reco.seasonal.plot.1

reco.seasonal.pd.alt <- reco.seasonal.gbm %>%
  pdp::partial(pred.var = "alt.annual", n.trees = reco.seasonal.gbm$n.trees,
               grid.resolution = 100)
reco.seasonal.plot.2 <- plot.pdp(df1 = flux.seasonal, df2 = reco.seasonal.pd.alt,
                                predictor = 'alt.annual', response = 'reco.sum',
                                color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.seasonal,
             aes(x = alt.annual, y = reco.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.seasonal,
            aes(x = alt.annual, y = reco.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7)# +
# geom_text(data = example.plots.seasonal,
#           aes(x = alt.annual, y = reco.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
reco.seasonal.plot.2

reco.seasonal.pd.tair.sd <- reco.seasonal.gbm %>%
  pdp::partial(pred.var = "tair.sd", n.trees = reco.seasonal.gbm$n.trees,
               grid.resolution = 100)
reco.seasonal.plot.3 <- plot.pdp(df1 = flux.seasonal, df2 = reco.seasonal.pd.tair.sd,
                                predictor = 'tair.sd', response = 'reco.sum',
                                color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.seasonal,
             aes(x = tair.sd, y = reco.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.seasonal,
            aes(x = tair.sd, y = reco.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7)# +
# geom_text(data = example.plots.seasonal,
#           aes(x = tair.sd, y = reco.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
reco.seasonal.plot.3

reco.seasonal.pd.subsidence <- reco.seasonal.gbm %>%
  pdp::partial(pred.var = "subsidence.annual", n.trees = reco.seasonal.gbm$n.trees,
               grid.resolution = 100)
reco.seasonal.plot.4 <- plot.pdp(df1 = flux.seasonal, df2 = reco.seasonal.pd.subsidence,
                                predictor = 'subsidence.annual', response = 'reco.sum',
                                color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.seasonal,
             aes(x = subsidence.annual, y = reco.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.seasonal,
            aes(x = subsidence.annual, y = reco.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7) +
  scale_x_reverse(name = expression('Subsidence (cm)'),
                  breaks = seq(-100, 0, by = 25),
                  labels = seq(-100, 0, by = 25)*-1)# +
# geom_text(data = example.plots.seasonal,
#           aes(x = subsidence.annual, y = reco.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
reco.seasonal.plot.4

reco.seasonal.pd.plot <- ggarrange(reco.seasonal.plot.1,
                                  reco.seasonal.plot.2,
                                  reco.seasonal.plot.3,
                                  reco.seasonal.plot.4,
                                  nrow = 2,
                                  ncol = 2,
                                  common.legend = TRUE,
                                  legend = 'right',
                                  labels = seq(1, 4))
reco.seasonal.pd.plot

# ### GPP GBM
# # figure out good parameters to use
# grid <- expand.grid(.n.trees=seq(200, 800, by = 200),
#                     .interaction.depth=seq(1,6,by=1),
#                     .shrinkage=c(.001,.01,.1),
#                     .n.minobsinnode=c(5, 10))
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
#                 n.trees = 800,
#                 shrinkage = 0.01,
#                 interaction.depth = 6,
#                 n.minobsinnode = 5)
# summary(gpp.seasonal.gbm)
# # saveRDS(gpp.seasonal.gbm, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/gpp_seasonal_gbm.rds')
gpp.seasonal.gbm <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/gpp_seasonal_gbm.rds')
summary(gpp.seasonal.gbm)

# Variable Influence Plot
gpp.seasonal.influence <- gpp.seasonal.gbm %>%
  summary() %>%
  as.data.frame() %>%
  arrange(rel.inf) %>%
  mutate(var = case_when(var == 'biomass.annual' ~ 'Biomass', 
                         var == 'gwc.sd' ~ 'SD GWC', 
                         var == 'subsidence.annual' ~ 'Subsidence',
                         var == 'alt.annual' ~ 'ALT', 
                         var == 't10.sd' ~ 'SD Soil Temp', 
                         var == 'tair.mean' ~ 'Mean Air Temp', 
                         var == 'tair.sd' ~ 'SD Air Temp', 
                         var == 'tp.annual' ~ 'Thaw Penetration', 
                         var == 'vwc.sd' ~ 'SD VWC', 
                         var == 'wtd.mean' ~ 'Mean WTD', 
                         var == 't10.mean' ~ 'Mean Soil Temp', 
                         var == 'vwc.mean' ~ 'Mean VWC', 
                         var == 'gwc.mean' ~ 'Mean GWC', 
                         var == 'winter.min.t10.min' ~ 'Winter Min Soil Temp',  
                         var == 'winter.snow.depth' ~ 'Snow Depth', 
                         var == 'precip.sum' ~ 'Precipitation')) %>%
  mutate(variable = factor(var, levels = .$var),
         var = factor(seq(1, n())),
         response = 'gpp',
         timescale = 'seasonal')
ggplot(gpp.seasonal.influence, aes(x = rel.inf, y = variable)) +
  geom_col(fill = 'black') +
  scale_x_continuous(name = 'Relative Influence',
                     expand = expansion(mult = c(0, .05))) +
  theme_bw() +
  theme(axis.title.y = element_blank())

gpp.seasonal.pred <- gpp.seasonal %>%
  slice(-1*train.gpp.seasonal) %>%
  mutate(gpp.pred = predict(gpp.seasonal.gbm,
                             newdata = gpp.seasonal[-train.gpp.seasonal,],
                             n.trees = 100),
         gpp.resid = gpp.pred - gpp.sum,
         response = 'gpp',
         timescale = 'seasonal')
mean(gpp.seasonal.pred$gpp.resid^2)
gpp.seasonal.lm <- lm(gpp.pred ~ gpp.sum,
                      data = gpp.seasonal.pred)
summary(gpp.seasonal.lm)
gpp.seasonal.r2 <- summary(gpp.seasonal.lm)$r.squared
gpp.seasonal.r2.label <- paste0(as.character(expression('R'^2 ~ ' = ')), ' ~ ', round(gpp.seasonal.r2[1], 2))

gpp.seasonal.fit.plot <- ggplot(gpp.seasonal.pred, aes(x = gpp.sum, y = gpp.pred)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'black') +
  geom_text(x = 50, y = 550, label = gpp.seasonal.r2.label,
            hjust = 0,
            vjust = 1,
            parse = TRUE) +
  scale_x_continuous(name = expression('Measured GPP (gC m'^-2*')')) +
  scale_y_continuous(name = expression('Predicted GPP (gC m'^-2*')')) +
  theme_bw()
gpp.seasonal.fit.plot

# plot partial dependence plots on top of data points
gpp.seasonal.pd.biomass <- gpp.seasonal.gbm %>%
  pdp::partial(pred.var = "biomass.annual", n.trees = gpp.seasonal.gbm$n.trees,
               grid.resolution = 100)
gpp.seasonal.plot.1 <- plot.pdp(df1 = flux.seasonal, df2 = gpp.seasonal.pd.biomass,
                                 predictor = 'biomass.annual', response = 'gpp.sum',
                                 color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.seasonal,
             aes(x = biomass.annual, y = gpp.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.seasonal,
            aes(x = biomass.annual, y = gpp.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7)# +
# geom_text(data = example.plots.seasonal,
#           aes(x = biomass.annual, y = gpp.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
gpp.seasonal.plot.1

gpp.seasonal.pd.alt <- gpp.seasonal.gbm %>%
  pdp::partial(pred.var = "alt.annual", n.trees = gpp.seasonal.gbm$n.trees,
               grid.resolution = 100)
gpp.seasonal.plot.2 <- plot.pdp(df1 = flux.seasonal, df2 = gpp.seasonal.pd.alt,
                                 predictor = 'alt.annual', response = 'gpp.sum',
                                 color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.seasonal,
             aes(x = alt.annual, y = gpp.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.seasonal,
            aes(x = alt.annual, y = gpp.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7)# +
# geom_text(data = example.plots.seasonal,
#           aes(x = alt.annual, y = gpp.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
gpp.seasonal.plot.2

gpp.seasonal.pd.gwc.sd <- gpp.seasonal.gbm %>%
  pdp::partial(pred.var = "gwc.sd", n.trees = gpp.seasonal.gbm$n.trees,
               grid.resolution = 100)
gpp.seasonal.plot.3 <- plot.pdp(df1 = flux.seasonal, df2 = gpp.seasonal.pd.gwc.sd,
                                 predictor = 'gwc.sd', response = 'gpp.sum',
                                 color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.seasonal,
             aes(x = gwc.sd, y = gpp.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.seasonal,
            aes(x = gwc.sd, y = gpp.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7)# +
# geom_text(data = example.plots.seasonal,
#           aes(x = gwc.sd, y = gpp.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
gpp.seasonal.plot.3

gpp.seasonal.pd.tair.sd <- gpp.seasonal.gbm %>%
  pdp::partial(pred.var = "tair.sd", n.trees = gpp.seasonal.gbm$n.trees,
               grid.resolution = 100)
gpp.seasonal.plot.4 <- plot.pdp(df1 = flux.seasonal, df2 = gpp.seasonal.pd.tair.sd,
                                 predictor = 'tair.sd', response = 'gpp.sum',
                                 color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.seasonal,
             aes(x = tair.sd, y = gpp.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.seasonal,
            aes(x = tair.sd, y = gpp.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7)# +
# geom_text(data = example.plots.seasonal,
#           aes(x = tair.sd, y = gpp.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
gpp.seasonal.plot.4

gpp.seasonal.pd.plot <- ggarrange(gpp.seasonal.plot.1,
                                   gpp.seasonal.plot.2,
                                   gpp.seasonal.plot.3,
                                   gpp.seasonal.plot.4,
                                   nrow = 2,
                                   ncol = 2,
                                   common.legend = TRUE,
                                   legend = 'right',
                                   labels = seq(1, 4))
gpp.seasonal.pd.plot


### Monthly
nee.monthly <- flux.monthly[!is.na(nee.sum), 
                            c('nee.sum',
                              'tp.to.date',
                              'subsidence.annual',
                              'td', 
                              'vwc.mean', 'vwc.sd',
                              'gwc.mean', 'gwc.sd',
                              'wtd.mean', 'wtd.sd',
                              'precip', 
                              'tair.mean',
                              't10.mean', 't10.sd',
                              'vwc.mean.2m',
                              'gwc.mean.2m',
                              'gdd', 'fdd',
                              'gdd.2m',
                              'biomass.annual')]
gpp.monthly <- flux.monthly[!is.na(gpp.sum), 
                            c('gpp.sum',
                              'tp.to.date',
                              'subsidence.annual',
                              'td', 
                              'vwc.mean', 'vwc.sd',
                              'gwc.mean', 'gwc.sd',
                              'wtd.mean', 'wtd.sd',
                              'precip', 
                              'tair.mean',
                              't10.mean', 't10.sd',
                              'vwc.mean.2m',
                              'gwc.mean.2m',
                              'gdd', 'fdd',
                              'gdd.2m',
                              'biomass.annual')]
reco.monthly <- flux.monthly[!is.na(reco.sum), 
                             c('reco.sum',
                               'tp.to.date',
                               'subsidence.annual',
                               'td', 
                               'vwc.mean', 'vwc.sd',
                               'gwc.mean', 'gwc.sd',
                               'wtd.mean', 'wtd.sd',
                               'precip', 
                               'tair.mean',
                               't10.mean', 't10.sd',
                               'vwc.mean.2m',
                               'gwc.mean.2m',
                               'gdd', 'fdd',
                               'gdd.2m',
                               'biomass.annual')]
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


# ### NEE GBM
# # figure out good parameters to use
# grid <- expand.grid(.n.trees=seq(600, 1000, by = 200),
#                   .interaction.depth=seq(4, 7, by=1),
#                   .shrinkage=c(.01, .1),
#                   .n.minobsinnode=c(5, 7, 10))
# gbm.train <- train(nee.sum~.,
#                    data = nee.monthly[train.nee.monthly,][complete.cases(nee.monthly[train.nee.monthly,]),],
#                    method = 'gbm',
#                    trControl = control,
#                    tuneGrid = grid)
# gbm.train
# run model
# nee.monthly.gbm <- gbm(nee.sum~.,
#                data = nee.monthly[train.nee.monthly,],
#                distribution = "gaussian",
#                n.trees = 800,
#                shrinkage = 0.1,
#                interaction.depth = 6,
#                n.minobsinnode = 7)
# summary(nee.monthly.gbm)
# # saveRDS(nee.monthly.gbm, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/nee_monthly_gbm.rds')
nee.monthly.gbm <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/nee_monthly_gbm.rds')
summary(nee.monthly.gbm)

# Variable Influence Plot
nee.monthly.influence <- nee.monthly.gbm %>%
  summary() %>%
  as.data.frame() %>%
  arrange(rel.inf) %>%
  mutate(var = case_when(var == 'tair.mean' ~ 'Mean Air Temp',
                         var == 'vwc.sd' ~ 'SD VWC', 
                         var == 'vwc.mean' ~ 'Mean VWC', 
                         var == 't10.mean' ~ 'Mean Soil Temp', 
                         var == 'gdd.2m' ~ '2 Month GDD',
                         var == 'gwc.sd' ~ 'SD GWC', 
                         var == 'wtd.sd' ~ 'SD WTD', 
                         var == 't10.sd' ~ 'SD Soil Temp', 
                         var == 'subsidence.annual' ~ 'Subsidence',
                         var == 'gwc.mean.2m' ~ '2 Month Mean GWC', 
                         var == 'wtd.mean' ~ 'Mean WTD', 
                         var == 'gwc.mean' ~ 'Mean GWC', 
                         var == 'tp.to.date' ~ 'Thaw Penetration', 
                         var == 'td' ~ 'Thaw Depth',
                         var == 'gdd' ~ 'GDD',
                         var == 'precip' ~ 'Precipitation', 
                         var == 'fdd' ~ 'FDD',
                         var == 'vwc.mean.2m' ~ '2 Month Mean VWC',
                         var == 'biomass.annual' ~ 'Biomass')) %>%
  mutate(variable = factor(var, levels = .$var),
         var = factor(seq(1, n())),
         response = 'nee',
         timescale = 'monthly')

# plot relative influence
ggplot(nee.monthly.influence, aes(x = rel.inf, y = variable)) +
  geom_col(fill = 'black') +
  scale_x_continuous(name = 'Relative Influence',
                     expand = expansion(mult = c(0, .05))) +
  theme_bw() +
  theme(axis.title.y = element_blank())

# determine model performance with a linear model on the test data
nee.monthly.pred <- nee.monthly %>%
  slice(-1*train.nee.monthly) %>%
  mutate(nee.pred = predict(nee.monthly.gbm,
                            newdata = nee.monthly[-train.nee.monthly,],
                            n.trees = 100),
         nee.resid = nee.pred - nee.sum,
         response = 'nee',
         timescale = 'monthly')
mean(nee.monthly.pred$nee.resid^2)
nee.monthly.lm <- lm(nee.pred ~ nee.sum,
                      data = nee.monthly.pred)
summary(nee.monthly.lm)
nee.monthly.r2 <- summary(nee.monthly.lm)$r.squared
nee.monthly.r2.label <- paste0(as.character(expression('R'^2 ~ ' = ')), ' ~ ', round(nee.monthly.r2[1], 2))

# plot model performance
nee.monthly.fit.plot <- ggplot(nee.monthly.pred, aes(x = nee.sum, y = nee.pred)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'black') +
  geom_text(x = -50, y = 115, label = nee.monthly.r2.label,
            hjust = 0,
            vjust = 1,
            parse = TRUE) +
  scale_x_continuous(name = expression('Measured NEE (gC m'^-2*')')) +
  scale_y_continuous(name = expression('Predicted NEE (gC m'^-2*')')) +
  theme_bw()
nee.monthly.fit.plot

example.plots.monthly <- flux.monthly %>%
  filter((fence == 4 & plot == 1 | fence == 4 & plot == 6 | fence == 1 & plot == 5) &
           !(is.na(nee.sum) | is.na(gpp.sum) | is.na(reco.sum)))

nee.monthly.pd.tair.mean <- nee.monthly.gbm %>%
  pdp::partial(pred.var = "tair.mean", n.trees = nee.monthly.gbm$n.trees,
               grid.resolution = 100)
nee.monthly.plot.1 <- plot.pdp(df1 = flux.monthly, df2 = nee.monthly.pd.tair.mean,
                               predictor = 'tair.mean', response = 'nee.sum',
                               color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.monthly,
             aes(x = tair.mean, y = nee.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.monthly,
            aes(x = tair.mean, y = nee.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7)# +
# geom_text(data = example.plots.monthly,
#           aes(x = tair.mean, y = nee.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
nee.monthly.plot.1

nee.monthly.pd.biomass <- nee.monthly.gbm %>%
  pdp::partial(pred.var = "biomass.annual", n.trees = nee.monthly.gbm$n.trees,
               grid.resolution = 100)
nee.monthly.plot.2 <- plot.pdp(df1 = flux.monthly, df2 = nee.monthly.pd.biomass,
                                predictor = 'biomass.annual', response = 'nee.sum',
                                color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.monthly,
             aes(x = biomass.annual, y = nee.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.monthly,
            aes(x = biomass.annual, y = nee.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7)# +
# geom_text(data = example.plots.monthly,
#           aes(x = biomass.annual, y = nee.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
nee.monthly.plot.2

nee.monthly.pd.vwc.mean <- nee.monthly.gbm %>%
  pdp::partial(pred.var = "vwc.mean", n.trees = nee.monthly.gbm$n.trees,
               grid.resolution = 100)
nee.monthly.plot.3 <- plot.pdp(df1 = flux.monthly, df2 = nee.monthly.pd.vwc.mean,
                                predictor = 'vwc.mean', response = 'nee.sum',
                                color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.monthly,
             aes(x = vwc.mean, y = nee.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.monthly,
            aes(x = vwc.mean, y = nee.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7)# +
# geom_text(data = example.plots.monthly,
#           aes(x = vwc.mean, y = nee.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
nee.monthly.plot.3

nee.monthly.pd.gwc.sd <- nee.monthly.gbm %>%
  pdp::partial(pred.var = "gwc.sd", n.trees = nee.monthly.gbm$n.trees,
               grid.resolution = 100)
nee.monthly.plot.4 <- plot.pdp(df1 = flux.monthly, df2 = nee.monthly.pd.gwc.sd,
                                predictor = 'gwc.sd', response = 'nee.sum',
                                color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.monthly,
             aes(x = gwc.sd, y = nee.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.monthly,
            aes(x = gwc.sd, y = nee.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7)# +
# geom_text(data = example.plots.monthly,
#           aes(x = gwc.sd, y = nee.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
nee.monthly.plot.4

nee.monthly.pd.plot <- ggarrange(nee.monthly.plot.1,
                                  nee.monthly.plot.2,
                                  nee.monthly.plot.3,
                                  nee.monthly.plot.4,
                                  nrow = 2,
                                  ncol = 2,
                                  common.legend = TRUE,
                                  legend = 'right',
                                  labels = seq(1, 4))
nee.monthly.pd.plot


# ### Reco GBM
# # figure out good parameters to use
# grid <- expand.grid(.n.trees=seq(600, 1000, by = 200),
#                   .interaction.depth=seq(3, 6, by=1),
#                   .shrinkage=c(.01, .1),
#                   .n.minobsinnode=c(5, 7, 10))
# gbm.train <- train(reco.sum~.,
#                    data = reco.monthly[train.reco.monthly,][complete.cases(reco.monthly[train.reco.monthly,]),],
#                    method = 'gbm',
#                    trControl = control,
#                    tuneGrid = grid)
# gbm.train
# # run model
# reco.monthly.gbm <- gbm(reco.sum~.,
#                        data = reco.monthly[train.reco.monthly,],
#                        distribution = "gaussian",
#                        n.trees = 1000,
#                        shrinkage = 0.1,
#                        interaction.depth = 6,
#                        n.minobsinnode = 7)
# summary(reco.monthly.gbm)
# # saveRDS(reco.monthly.gbm, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/reco_monthly_gbm.rds')
reco.monthly.gbm <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/reco_monthly_gbm.rds')
summary(reco.monthly.gbm)

# Variable Influence Plot
reco.monthly.influence <- reco.monthly.gbm %>%
  summary() %>%
  as.data.frame() %>%
  arrange(rel.inf) %>%
  mutate(var = case_when(var == 't10.mean' ~ 'Mean Soil Temp', 
                         var == 'subsidence.annual' ~ 'Subsidence',
                         var == 'vwc.mean' ~ 'Mean VWC', 
                         var == 'gwc.sd' ~ 'SD GWC', 
                         var == 'tp.to.date' ~ 'Thaw Penetration', 
                         var == 'vwc.sd' ~ 'SD VWC', 
                         var == 'wtd.mean' ~ 'Mean WTD', 
                         var == 't10.sd' ~ 'SD Soil Temp', 
                         var == 'gwc.mean' ~ 'Mean GWC', 
                         var == 'gwc.mean.2m' ~ '2 Month Mean GWC', 
                         var == 'precip' ~ 'Precipitation', 
                         var == 'tair.mean' ~ 'Mean Air Temp',
                         var == 'td' ~ 'Thaw Depth',
                         var == 'fdd' ~ 'FDD',
                         var == 'vwc.mean.2m' ~ '2 Month Mean VWC',
                         var == 'wtd.sd' ~ 'SD WTD', 
                         var == 'gdd' ~ 'GDD',
                         var == 'gdd.2m' ~ '2 Month GDD',
                         var == 'biomass.annual' ~ 'Biomass')) %>%
  mutate(variable = factor(var, levels = .$var),
         var = factor(seq(1, n())),
         response = 'reco',
         timescale = 'monthly')
ggplot(reco.monthly.influence, aes(x = rel.inf, y = variable)) +
  geom_col(fill = 'black') +
  scale_x_continuous(name = 'Relative Influence',
                     expand = expansion(mult = c(0, .05))) +
  theme_bw() +
  theme(axis.title.y = element_blank())

reco.monthly.pred <- reco.monthly %>%
  slice(-1*train.reco.monthly) %>%
  mutate(reco.pred = predict(reco.monthly.gbm,
                            newdata = reco.monthly[-train.reco.monthly,],
                            n.trees = 100),
         reco.resid = reco.pred - reco.sum,
         response = 'reco',
         timescale = 'monthly')
mean(reco.monthly.pred$reco.resid^2)
reco.monthly.lm <- lm(reco.pred ~ reco.sum,
                      data = reco.monthly.pred)
summary(reco.monthly.lm)
reco.monthly.r2 <- summary(reco.monthly.lm)$r.squared
reco.monthly.r2.label <- paste0(as.character(expression('R'^2 ~ ' = ')), ' ~ ', round(reco.monthly.r2[1], 2))

reco.monthly.fit.plot <- ggplot(reco.monthly.pred, aes(x = reco.sum, y = reco.pred)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'black') +
  geom_text(x = 10, y = 175, label = reco.monthly.r2.label,
            hjust = 0,
            vjust = 0,
            parse = TRUE) +
  scale_x_continuous(name = expression('Measured Reco (gC m'^-2*')')) +
  scale_y_continuous(name = expression('Predicted Reco (gC m'^-2*')')) +
  theme_bw()
reco.monthly.fit.plot

# plot model performance
nee.monthly.fit.plot <- ggplot(nee.monthly.pred, aes(x = nee.sum, y = nee.pred)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'black') +
  geom_text(x = -50, y = 115, label = nee.monthly.r2.label,
            hjust = 0,
            vjust = 1,
            parse = TRUE) +
  scale_x_continuous(name = expression('Measured NEE (gC m'^-2*')')) +
  scale_y_continuous(name = expression('Predicted NEE (gC m'^-2*')')) +
  theme_bw()
nee.monthly.fit.plot

# plot partial dependence plots with data points
reco.monthly.pd.t10.mean <- reco.monthly.gbm %>%
  pdp::partial(pred.var = "t10.mean", n.trees = reco.monthly.gbm$n.trees,
               grid.resolution = 100)
reco.monthly.plot.1 <- plot.pdp(df1 = flux.monthly, df2 = reco.monthly.pd.t10.mean,
                               predictor = 't10.mean', response = 'reco.sum',
                               color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.monthly,
             aes(x = t10.mean, y = reco.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.monthly,
            aes(x = t10.mean, y = reco.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7)# +
# geom_text(data = example.plots.monthly,
#           aes(x = t10.mean, y = reco.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
reco.monthly.plot.1

reco.monthly.pd.biomass <- reco.monthly.gbm %>%
  pdp::partial(pred.var = "biomass.annual", n.trees = reco.monthly.gbm$n.trees,
               grid.resolution = 100)
reco.monthly.plot.2 <- plot.pdp(df1 = flux.monthly, df2 = reco.monthly.pd.biomass,
                               predictor = 'biomass.annual', response = 'reco.sum',
                               color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.monthly,
             aes(x = biomass.annual, y = reco.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.monthly,
            aes(x = biomass.annual, y = reco.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7)# +
# geom_text(data = example.plots.monthly,
#           aes(x = biomass.annual, y = reco.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
reco.monthly.plot.2

reco.monthly.pd.gwc.sd <- reco.monthly.gbm %>%
  pdp::partial(pred.var = "gwc.sd", n.trees = reco.monthly.gbm$n.trees,
               grid.resolution = 100)
reco.monthly.plot.3 <- plot.pdp(df1 = flux.monthly, df2 = reco.monthly.pd.gwc.sd,
                               predictor = 'gwc.sd', response = 'reco.sum',
                               color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.monthly,
             aes(x = gwc.sd, y = reco.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.monthly,
            aes(x = gwc.sd, y = reco.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7)# +
# geom_text(data = example.plots.monthly,
#           aes(x = gwc.sd, y = reco.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
reco.monthly.plot.3

reco.monthly.pd.subsidence <- reco.monthly.gbm %>%
  pdp::partial(pred.var = "subsidence.annual", n.trees = reco.monthly.gbm$n.trees,
               grid.resolution = 100)
reco.monthly.plot.4 <- plot.pdp(df1 = flux.monthly, df2 = reco.monthly.pd.subsidence,
                                 predictor = 'subsidence.annual', response = 'reco.sum',
                                 color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.monthly,
             aes(x = subsidence.annual, y = reco.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.monthly,
            aes(x = subsidence.annual, y = reco.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7) +
  scale_x_reverse(name = expression('Subsidence (cm)'),
                  breaks = seq(-100, 0, by = 25),
                  labels = seq(-100, 0, by = 25)*-1)# +
# geom_text(data = example.plots.monthly,
#           aes(x = subsidence.annual, y = reco.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
reco.monthly.plot.4

reco.monthly.pd.plot <- ggarrange(reco.monthly.plot.1,
                                 reco.monthly.plot.2,
                                 reco.monthly.plot.3,
                                 reco.monthly.plot.4,
                                 nrow = 2,
                                 ncol = 2,
                                 common.legend = TRUE,
                                 legend = 'right',
                                 labels = seq(1, 4))
reco.monthly.pd.plot


# ###GPP GBM
# # figure out good parameters to use
# grid <- expand.grid(.n.trees=seq(600, 1000, by = 200),
#                   .interaction.depth=seq(3, 6, by=1),
#                   .shrinkage=c(.01, .1),
#                   .n.minobsinnode=c(5, 7, 10))
# gbm.train <- train(gpp.sum~.,
#                    data = gpp.monthly[train.gpp.monthly,][complete.cases(gpp.monthly[train.gpp.monthly,]),],
#                    method = 'gbm',
#                    trControl = control,
#                    tuneGrid = grid)
# gbm.train
# # run model
# gpp.monthly.gbm <- gbm(gpp.sum~.,
#                        data = gpp.monthly[train.gpp.monthly,],
#                        distribution = "gaussian",
#                        n.trees = 1000,
#                        shrinkage = 0.1,
#                        interaction.depth = 5,
#                        n.minobsinnode = 5)
# summary(gpp.monthly.gbm)
# # saveRDS(gpp.monthly.gbm, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/gpp_monthly_gbm.rds')
gpp.monthly.gbm <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/gpp_monthly_gbm.rds')
summary(gpp.monthly.gbm)

# Variable Influence Plot
gpp.monthly.influence <- gpp.monthly.gbm %>%
  summary() %>%
  as.data.frame() %>%
  arrange(rel.inf) %>%
  mutate(var = case_when(var == 't10.mean' ~ 'Mean Soil Temp', 
                         var == 'vwc.sd' ~ 'SD VWC', 
                         var == 'vwc.mean' ~ 'Mean VWC', 
                         var == 'tair.mean' ~ 'Mean Air Temp',
                         var == 'gwc.sd' ~ 'SD GWC', 
                         var == 'subsidence.annual' ~ 'Subsidence',
                         var == 't10.sd' ~ 'SD Soil Temp', 
                         var == 'gdd.2m' ~ '2 Month GDD',
                         var == 'precip' ~ 'Precipitation', 
                         var == 'wtd.mean' ~ 'Mean WTD', 
                         var == 'gwc.mean' ~ 'Mean GWC', 
                         var == 'tp.to.date' ~ 'Thaw Penetration', 
                         var == 'wtd.sd' ~ 'SD WTD', 
                         var == 'gwc.mean.2m' ~ '2 Month Mean GWC', 
                         var == 'td' ~ 'Thaw Depth',
                         var == 'fdd' ~ 'FDD',
                         var == 'gdd' ~ 'GDD',
                         var == 'vwc.mean.2m' ~ '2 Month Mean VWC',
                         var == 'biomass.annual' ~ 'Biomass')) %>%
  mutate(variable = factor(var, levels = .$var),
         var = factor(seq(1, n())),
         response = 'gpp',
         timescale = 'monthly')
ggplot(gpp.monthly.influence, aes(x = rel.inf, y = variable)) +
  geom_col(fill = 'black') +
  scale_x_continuous(name = 'Relative Influence',
                     expand = expansion(mult = c(0, .05))) +
  theme_bw() +
  theme(axis.title.y = element_blank())

# plot model performance
gpp.monthly.pred <- gpp.monthly %>%
  slice(-1*train.gpp.monthly) %>%
  mutate(gpp.pred = predict(gpp.monthly.gbm,
                             newdata = gpp.monthly[-train.gpp.monthly,],
                             n.trees = 100),
         gpp.resid = gpp.pred - gpp.sum,
         response = 'gpp',
         timescale = 'monthly')
mean(gpp.monthly.pred$gpp.resid^2)
gpp.monthly.lm <- lm(gpp.pred ~ gpp.sum,
                      data = gpp.monthly.pred)
summary(gpp.monthly.lm)
gpp.monthly.r2 <- summary(gpp.monthly.lm)$r.squared
gpp.monthly.r2.label <- paste0(as.character(expression('R'^2 ~ ' = ')), ' ~ ', round(gpp.monthly.r2[1], 2))

gpp.monthly.fit.plot <- ggplot(gpp.monthly.pred, aes(x = gpp.sum, y = gpp.pred)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'black') +
  geom_text(x = 0, y = 270, label = gpp.monthly.r2.label,
            hjust = 0,
            vjust = 1,
            parse = TRUE) +
  scale_x_continuous(name = expression('Measured GPP (gC m'^-2*')')) +
  scale_y_continuous(name = expression('Predicted GPP (gC m'^-2*')')) +
  theme_bw()
gpp.monthly.fit.plot

# plot partial dependence plots with data points
gpp.monthly.pd.t10.mean <- gpp.monthly.gbm %>%
  pdp::partial(pred.var = "t10.mean", n.trees = gpp.monthly.gbm$n.trees,
               grid.resolution = 100)
gpp.monthly.plot.1 <- plot.pdp(df1 = flux.monthly, df2 = gpp.monthly.pd.t10.mean,
                                predictor = 't10.mean', response = 'gpp.sum',
                                color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.monthly,
             aes(x = t10.mean, y = gpp.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.monthly,
            aes(x = t10.mean, y = gpp.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7)# +
# geom_text(data = example.plots.monthly,
#           aes(x = t10.mean, y = gpp.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
gpp.monthly.plot.1

gpp.monthly.pd.biomass <- gpp.monthly.gbm %>%
  pdp::partial(pred.var = "biomass.annual", n.trees = gpp.monthly.gbm$n.trees,
               grid.resolution = 100)
gpp.monthly.plot.2 <- plot.pdp(df1 = flux.monthly, df2 = gpp.monthly.pd.biomass,
                                predictor = 'biomass.annual', response = 'gpp.sum',
                                color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.monthly,
             aes(x = biomass.annual, y = gpp.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.monthly,
            aes(x = biomass.annual, y = gpp.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7)# +
# geom_text(data = example.plots.monthly,
#           aes(x = biomass.annual, y = gpp.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
gpp.monthly.plot.2

gpp.monthly.pd.tair.mean <- gpp.monthly.gbm %>%
  pdp::partial(pred.var = "tair.mean", n.trees = gpp.monthly.gbm$n.trees,
               grid.resolution = 100)
gpp.monthly.plot.3 <- plot.pdp(df1 = flux.monthly, df2 = gpp.monthly.pd.tair.mean,
                                predictor = 'tair.mean', response = 'gpp.sum',
                                color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.monthly,
             aes(x = tair.mean, y = gpp.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.monthly,
            aes(x = tair.mean, y = gpp.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7)# +
# geom_text(data = example.plots.monthly,
#           aes(x = tair.mean, y = gpp.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
gpp.monthly.plot.3

gpp.monthly.pd.vwc.sd <- gpp.monthly.gbm %>%
  pdp::partial(pred.var = "vwc.sd", n.trees = gpp.monthly.gbm$n.trees,
               grid.resolution = 100)
gpp.monthly.plot.4 <- plot.pdp(df1 = flux.monthly, df2 = gpp.monthly.pd.vwc.sd,
                               predictor = 'vwc.sd', response = 'gpp.sum',
                               color.var = 'flux.year', shape.var = 'treatment') +
  geom_point(data = example.plots.monthly,
             aes(x = vwc.sd, y = gpp.sum),
             inherit.aes = FALSE,
             shape = 1, size = 3) +
  geom_text(data = example.plots.monthly,
            aes(x = vwc.sd, y = gpp.sum, label = plot.id),
            inherit.aes = FALSE, size = 3, nudge_y = -7)# +
# geom_text(data = example.plots.monthly,
#           aes(x = vwc.sd, y = gpp.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
gpp.monthly.plot.4

gpp.monthly.pd.plot <- ggarrange(gpp.monthly.plot.1,
                                  gpp.monthly.plot.2,
                                  gpp.monthly.plot.3,
                                  gpp.monthly.plot.4,
                                  nrow = 2,
                                  ncol = 2,
                                  common.legend = TRUE,
                                  legend = 'right',
                                  labels = seq(1, 4))
gpp.monthly.pd.plot


### Plot Variable Importance and Model Performance
### Functions
# A function to plot the inset 
get_inset <- function(data.df, fit.df){
  p <- ggplot(data.df, aes(x = flux.measured, y = flux.pred)) +
    geom_point() +
    geom_smooth(method = 'lm', color = 'black') +
    geom_text(data = fit.df,
              aes(x = x, y = y, label = label), 
              hjust = 0,
              vjust = 1,
              parse = TRUE,
              size = 3) +
    scale_x_continuous(name = expression('Measured (gC m'^-2*')')) +
    scale_y_continuous(name = expression('Predicted (gC m'^-2*')')) +
    theme_bw() +
    theme(plot.background = element_rect(color = 'black'),
          text = element_text(size = 8))
  return(p)
}

# This function allows us to specify which facets to annotate
annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) 
{
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = FALSE, params = list(grob = grob, 
                                           xmin = xmin, xmax = xmax, 
                                           ymin = ymin, ymax = ymax))
}


### Get all of the variable importance data together
# seasonal
variable.influence.seasonal <- nee.seasonal.influence %>%
  rbind.data.frame(reco.seasonal.influence,
                   gpp.seasonal.influence) %>%
  mutate(variable.label = variable,
         variable = paste0(response, as.character(variable.label)),
         var = seq(1, n()),
         timescale = str_to_title(timescale),
         response = case_when(response %in% c('nee', 'gpp') ~ str_to_upper(response),
                              TRUE ~ 'Reco'))

# monthly
variable.influence.monthly <- nee.monthly.influence %>%
  rbind.data.frame(reco.monthly.influence,
                   gpp.monthly.influence) %>%
  mutate(variable.label = variable,
         variable = paste0(response, as.character(variable.label)),
         var = seq(1, n()),
         timescale = str_to_title(timescale),
         response = case_when(response %in% c('nee', 'gpp') ~ str_to_upper(response),
                              TRUE ~ 'Reco'))


### Get all of the model fit data together
# seasonal
flux.seasonal.pred <- nee.seasonal.pred %>%
  rename(flux.measured = nee.sum, flux.resid = nee.resid, flux.pred = nee.pred) %>%
  rbind.data.frame(reco.seasonal.pred %>%
                     rename(flux.measured = reco.sum, flux.resid = reco.resid, flux.pred = reco.pred),
                   gpp.seasonal.pred %>%
                     rename(flux.measured = gpp.sum, flux.resid = gpp.resid, flux.pred = gpp.pred)) %>%
  mutate(timescale = str_to_title(timescale),
         response = case_when(response %in% c('nee', 'gpp') ~ str_to_upper(response),
                              TRUE ~ 'Reco'))

model.fit.seasonal <- data.frame(response = c('GPP', 'NEE', 'Reco')) %>%
  mutate(label = c(gpp.seasonal.r2.label,
                   nee.seasonal.r2.label,
                   reco.seasonal.r2.label),
         x = c(5, -200, 100),
         y = c(595, 160, 575))

seasonal.grob.dimensions <- data.frame(xmin = 9,
                                      xmax = 24.5,
                                      ymin = 1,
                                      ymax = 12)

# monthly
flux.monthly.pred <- nee.monthly.pred %>%
  rename(flux.measured = nee.sum, flux.resid = nee.resid, flux.pred = nee.pred) %>%
  rbind.data.frame(reco.monthly.pred %>%
                     rename(flux.measured = reco.sum, flux.resid = reco.resid, flux.pred = reco.pred),
                   gpp.monthly.pred %>%
                     rename(flux.measured = gpp.sum, flux.resid = gpp.resid, flux.pred = gpp.pred)) %>%
  mutate(timescale = str_to_title(timescale),
         response = case_when(response %in% c('nee', 'gpp') ~ str_to_upper(response),
                              TRUE ~ 'Reco'))

model.fit.monthly <- data.frame(response = c('GPP', 'NEE', 'Reco')) %>%
  mutate(label = c(gpp.monthly.r2.label,
                   nee.monthly.r2.label,
                   reco.monthly.r2.label),
         x = c(0, -55, 15),
         y = c(280, 120, 195))

monthly.grob.dimensions <- data.frame(xmin = 10,
                                      xmax = 39.25,
                                      ymin = 1.25,
                                      ymax = 14.25)


### Create the insets
# seasonal
seasonal.insets <- flux.seasonal.pred %>% 
  select(response, flux.measured, flux.pred) %>%
  group_by(response) %>%
  group_split() %>%
  purrr::map2(model.fit.seasonal %>%
                group_by(response) %>%
                group_split(),
              ~annotation_custom2(grob = ggplotGrob(get_inset(.x, .y)), 
                                  data = data.frame(response = unique(.$response)),
                                  ymin = seasonal.grob.dimensions$ymin, 
                                  ymax = seasonal.grob.dimensions$ymax, 
                                  xmin = seasonal.grob.dimensions$xmin, 
                                  xmax = seasonal.grob.dimensions$xmax)
  )

# monthly
monthly.insets <- flux.monthly.pred %>% 
  select(response, flux.measured, flux.pred) %>%
  group_by(response) %>%
  group_split() %>%
  purrr::map2(model.fit.monthly %>%
                group_by(response) %>%
                group_split(),
              ~annotation_custom2(grob = ggplotGrob(get_inset(.x, .y)), 
                                  data = data.frame(response = unique(.$response)),
                                  ymin = monthly.grob.dimensions$ymin, 
                                  ymax = monthly.grob.dimensions$ymax, 
                                  xmin = monthly.grob.dimensions$xmin, 
                                  xmax = monthly.grob.dimensions$xmax)
  )




### Plot
# seasonal
seasonal.influence.plot <- ggplot(variable.influence.seasonal, 
       aes(x = rel.inf, 
           y = reorder(variable, var))) +
  geom_col(fill = 'black') +
  seasonal.insets +
  scale_x_continuous(name = 'Relative Influence',
                     breaks = seq(0, 25, by = 5),
                     minor_breaks = seq(0, 25, by = 1),
                     expand = expansion(mult = c(0, .05))) +
  scale_y_discrete(breaks = variable.influence.seasonal$variable,
                   labels = as.character(variable.influence.seasonal$variable.label)) +
  facet_grid(response ~ timescale,
             scales = 'free') +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        plot.margin = margin(10, 10, 10, 20, unit = 'pt'))
seasonal.influence.plot
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/gbm_influence_plot_seasonal.jpg',
#        seasonal.influence.plot,
#        height = 6,
#        width = 3.5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/gbm_influence_plot_seasonal.pdf',
#        seasonal.influence.plot,
#        height = 6,
#        width = 3.5)

# monthly
monthly.influence.plot <- ggplot(variable.influence.monthly) +
  geom_col(aes(x = rel.inf, 
               y = reorder(variable, var)),
               fill = 'black') + 
  monthly.insets +
  scale_x_continuous(name = 'Relative Influence',
                     breaks = seq(0, 40, by = 5),
                     minor_breaks = seq(0, 40, by = 1),
                     expand = expansion(mult = c(0, .05))) +
  scale_y_discrete(breaks = variable.influence.monthly$variable,
                   labels = as.character(variable.influence.monthly$variable.label)) +
  facet_grid(response ~ timescale,
             scales = 'free') +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        plot.margin = margin(10, 10, 10, 20, unit = 'pt'))
monthly.influence.plot
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/gbm_influence_plot_monthly.jpg',
#        monthly.influence.plot,
#        height = 7,
#        width = 4)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/gbm_influence_plot_monthly.pdf',
#        monthly.influence.plot,
#        height = 7,
#        width = 4)


### Join into one plot
influence.plot <- ggarrange(monthly.influence.plot +
            theme(strip.background.y = element_blank(),
                  strip.text.y = element_blank()),
          seasonal.influence.plot,
          ncol = 2,
          widths = c(0.925, 1))
influence.plot
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/gbm_influence_plot.jpg',
#        influence.plot,
#        height = 10,
#        width = 10,
#        bg = 'white')
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/gbm_influence_plot.pdf',
#        influence.plot,
#        height = 10,
#        width = 10,
#        bg = 'white')

### Explore individual important relationships
# Are soil temp and air temp mostly responding to seasonal variation?
# Soil Temp and GPP, monthly
ggplot(filter(flux.monthly, !is.na(gpp.sum)), 
       aes(x = t10.mean, y = gpp.sum, 
                         color = month)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('Mean Soil Temp ('*degree*'C)')) +
  scale_y_continuous(name = expression('GPP (gC m'^-2*')')) +
  scale_color_viridis(name = 'Month',
                      limits = c(5, 9)) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  theme_bw()

# SD VWC and GPP, monthly
ggplot(filter(flux.monthly, !is.na(gpp.sum)), 
       aes(x = vwc.sd, y = gpp.sum, 
                         color = month)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('SD VWC (%)')) +
  scale_y_continuous(name = expression('GPP (gC m'^-2*')')) +
  scale_color_viridis(name = 'Month',
                      limits = c(5, 9)) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  theme_bw()

# Mean VWC and GPP, monthly
ggplot(filter(flux.monthly, !is.na(gpp.sum)), 
       aes(x = vwc.mean, y = gpp.sum, 
                         color = month)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('Mean VWC (%)')) +
  scale_y_continuous(name = expression('GPP (gC m'^-2*')')) +
  scale_color_viridis(name = 'Month',
                      limits = c(5, 9)) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  theme_bw()

# Air Temp and NEE, monthly
ggplot(filter(flux.monthly, !is.na(nee.sum)), 
       aes(x = tair.mean, y = nee.sum, 
                         color = month)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('Mean Air Temp ('*degree*'C)')) +
  scale_y_continuous(name = expression('NEE (gC m'^-2*')')) +
  scale_color_viridis(name = 'Month',
                      limits = c(5, 9)) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  theme_bw()

# Biomass and NEE, monthly
ggplot(filter(flux.monthly, !is.na(nee.sum)), 
       aes(x = biomass.annual, y = nee.sum, 
                         color = month)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('Biomass (g m'^-2*')')) +
  scale_y_continuous(name = expression('NEE (gC m'^-2*')')) +
  scale_color_viridis(name = 'Month',
                      limits = c(5, 9)) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  theme_bw()

# Mean VWC and NEE, monthly
ggplot(filter(flux.monthly, !is.na(nee.sum)), 
       aes(x = vwc.mean, y = nee.sum, 
                         color = month)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('Mean VWC (%)')) +
  scale_y_continuous(name = expression('NEE (gC m'^-2*')')) +
  scale_color_viridis(name = 'Month',
                      limits = c(5, 9)) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  theme_bw()

# SD VWC and NEE, monthly
ggplot(filter(flux.monthly, !is.na(nee.sum)), 
       aes(x = vwc.sd, y = nee.sum, 
           color = month)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('SD VWC (%)')) +
  scale_y_continuous(name = expression('NEE (gC m'^-2*')')) +
  scale_color_viridis(name = 'Month',
                      limits = c(5, 9)) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  theme_bw()

# Soil Temp and Reco, monthly
ggplot(filter(flux.monthly, !is.na(reco.sum)), 
       aes(x = t10.mean, y = reco.sum, 
           color = month)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('Mean Soil Temp ('*degree*'C)')) +
  scale_y_continuous(name = expression('Reco (gC m'^-2*')')) +
  scale_color_viridis(name = 'Month',
                      limits = c(5, 9)) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  theme_bw()

# Biomass and Reco, monthly
ggplot(filter(flux.monthly, !is.na(reco.sum)), 
       aes(x = biomass.annual, y = reco.sum, 
           color = month)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('Biomass (g m'^-2*')')) +
  scale_y_continuous(name = expression('Reco (gC m'^-2*')')) +
  scale_color_viridis(name = 'Month',
                      limits = c(5, 9)) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  theme_bw()

# subsidence and Reco, monthly
ggplot(filter(flux.monthly, !is.na(reco.sum)), 
       aes(x = subsidence.annual*-1, y = reco.sum, 
                         color = month)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('Subsidence (cm)')) +
  scale_y_continuous(name = expression('Reco (gC m'^-2*')')) +
  scale_color_viridis(name = 'Month',
                      limits = c(5, 9)) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  theme_bw()

# Mean VWC and Reco, monthly
ggplot(filter(flux.monthly, !is.na(reco.sum)), 
       aes(x = vwc.mean, y = reco.sum, 
                         color = month)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('Mean VWC (%)')) +
  scale_y_continuous(name = expression('Reco (gC m'^-2*')')) +
  scale_color_viridis(name = 'Month',
                      limits = c(5, 9)) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  theme_bw()

# Biomass and GPP, seasonal
ggplot(filter(flux.seasonal, !is.na(gpp.sum)), 
       aes(x = biomass.annual, y = gpp.sum, 
                          color = flux.year)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('Biomass (g m'^-2*')')) +
  scale_y_continuous(name = expression('GPP (gC m'^-2*')')) +
  scale_color_viridis(name = 'Year',
                      breaks = seq(2010, 2020, by = 2)) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  theme_bw()

# SD GWC and GPP, seasonal
ggplot(filter(flux.seasonal, !is.na(gpp.sum)), 
       aes(x = gwc.sd, y = gpp.sum, 
                          color = flux.year)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('SD GWC (%)')) +
  scale_y_continuous(name = expression('GPP (gC m'^-2*')')) +
  scale_color_viridis(name = 'Year',
                      breaks = seq(2010, 2020, by = 2)) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  theme_bw()

# subsidence and GPP, seasonal
ggplot(filter(flux.seasonal, !is.na(gpp.sum)), 
       aes(x = subsidence.annual*-1, y = gpp.sum, 
                          color = flux.year)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('Subsidence (cm)')) +
  scale_y_continuous(name = expression('GPP (gC m'^-2*')')) +
  scale_color_viridis(name = 'Year',
                      breaks = seq(2010, 2020, by = 2)) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  theme_bw()

# ALT and GPP, seasonal
ggplot(filter(flux.seasonal, !is.na(gpp.sum)), 
       aes(x = alt.annual, y = gpp.sum, 
           color = flux.year)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('ALT (cm)')) +
  scale_y_continuous(name = expression('GPP (gC m'^-2*')')) +
  scale_color_viridis(name = 'Year',
                      breaks = seq(2010, 2020, by = 2)) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  theme_bw()

# Biomass and NEE, seasonal
ggplot(filter(flux.seasonal, !is.na(nee.sum)), 
       aes(x = biomass.annual, y = nee.sum, 
                          color = flux.year)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('Biomass (g m'^-2*')')) +
  scale_y_continuous(name = expression('NEE (gC m'^-2*')')) +
  scale_color_viridis(name = 'Year',
                      breaks = seq(2010, 2020, by = 2)) +
  scale_shape_manual(name = 'Year',
                     values = c(1, 0, 16, 15)) +
  theme_bw()

# Mean VWC and NEE, seasonal
ggplot(filter(flux.seasonal, !is.na(nee.sum)), 
       aes(x = vwc.mean, y = nee.sum, 
                          color = flux.year)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('Mean VWC (%)')) +
  scale_y_continuous(name = expression('NEE (gC m'^-2*')')) +
  scale_color_viridis(name = 'Year',
                      breaks = seq(2010, 2020, by = 2)) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  theme_bw()

# SD Soil Temp and NEE, seasonal
ggplot(filter(flux.seasonal, !is.na(nee.sum)), 
       aes(x = t10.sd, y = nee.sum, 
                          color = flux.year)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('SD Soil Temp ('*degree*'C)')) +
  scale_y_continuous(name = expression('NEE (gC m'^-2*')')) +
  scale_color_viridis(name = 'Year',
                      breaks = seq(2010, 2020, by = 2)) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  theme_bw()

# Mean Air Temp and NEE, seasonal
ggplot(filter(flux.seasonal, !is.na(nee.sum)), 
       aes(x = tair.mean, y = nee.sum, 
           color = flux.year)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('Mean Air Temp ('*degree*'C)')) +
  scale_y_continuous(name = expression('NEE (gC m'^-2*')')) +
  scale_color_viridis(name = 'Year',
                      breaks = seq(2010, 2020, by = 2)) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  theme_bw()

# ALT and Reco, seasonal
ggplot(filter(flux.seasonal, !is.na(reco.sum)), 
       aes(x = alt, y = reco.sum, 
                          color = flux.year)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('ALT (cm)')) +
  scale_y_continuous(name = expression('Reco (gC m'^-2*')')) +
  scale_color_viridis(name = 'Year',
                      breaks = seq(2010, 2020, by = 2)) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  theme_bw()

# SD Air Temp and Reco, seasonal
ggplot(filter(flux.seasonal, !is.na(reco.sum)), 
       aes(x = tair.sd, y = reco.sum, 
                          color = flux.year)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('SD Air Temp ('*degree*'C)')) +
  scale_y_continuous(name = expression('Reco (gC m'^-2*')')) +
  scale_color_viridis(name = 'Year',
                      breaks = seq(2010, 2020, by = 2)) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  theme_bw()

# Subsidence and Reco, seasonal
ggplot(filter(flux.seasonal, !is.na(reco.sum)), 
       aes(x = subsidence.annual*-1, y = reco.sum, 
           color = flux.year)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('Subsidence (cm)')) +
  scale_y_continuous(name = expression('Reco (gC m'^-2*')')) +
  scale_color_viridis(name = 'Year',
                      breaks = seq(2010, 2020, by = 2)) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  theme_bw()

# Biomass and Reco, seasonal
ggplot(filter(flux.seasonal, !is.na(reco.sum)), 
       aes(x = biomass.annual, y = reco.sum, 
           color = flux.year)) +
  geom_point(aes(shape = treatment)) +
  geom_smooth(method = 'gam', color = 'black') +
  scale_x_continuous(name = expression('Biomass (g m'^-2*')')) +
  scale_y_continuous(name = expression('Reco (gC m'^-2*')')) +
  scale_color_viridis(name = 'Year',
                      breaks = seq(2010, 2020, by = 2)) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1, 0, 16, 15)) +
  theme_bw()
################################################################################

### Time Series Analysis #######################################################
# Add in eddy covariance estimate for winter
# a function to load and return a file
loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}


flux.eddy <- read.csv("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/Ameriflux/AMF_US-EML_BASE_HH_3-5.csv",
                 skip = 2,
                 na.strings = c('-9999'),
                 quote = "\"'")
flux.eddy <- data.table(flux.eddy)
co2.2018.2019 <- loadRData("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2018-2019/AK18_Carbon_new_30Apr2019.Rdata")
co2.2018.2019[, u_var := NULL]
co2.2018.2019[, v_var := NULL]
co2.2018.2019[, w_var := NULL]
co2.2019.2020 <- loadRData("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2019-2020/AK19_Carbon.Rdata")
co2.2020.2021 <- loadRData("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2020-2021/AK20_Carbon.Rdata")
ch4.2018.2019 <- loadRData("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2018-2019/AK18_CO2&CH4.Rdata")
ch4.2019.2020 <- loadRData("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2019-2020/AK19_CO2&CH4.Rdata")
ch4.2020.2021 <- loadRData("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2020-2021/AK20_CO2&CH4.Rdata")

# Format ameriflux.eddy data
flux.eddy[, ts := parse_date_time(TIMESTAMP_START, orders = c('Y!m!*d!H!M!'))]
flux.eddy[, ts.end := parse_date_time(TIMESTAMP_END, orders = c('Y!m!*d!H!M!'))]
flux.eddy[, year := year(ts)]
flux.eddy[, month := month(ts)]
flux.eddy[, date := parse_date_time(paste(year(ts), month(ts), day(ts), sep = '-'), orders = c('Y!-m!*-d!'))]
flux.eddy[, ts.2 := parse_date_time(paste('0000-', month(ts), '-', day(ts), ' ', hour(ts), ':', minute(ts), ':', second(ts), sep = ''), orders = c('Y!-m!*-d! H!:M!:S!'))]
flux.eddy[, date.2 := parse_date_time(paste('0000-', month(ts), '-', day(ts), sep = ''), orders = c('Y!-m!*-d!'))]
flux.eddy <- flux.eddy[, .(ts, ts.2, date, date.2, month, year, CO2_measured = FC, NEP = NEE_PI_F, Reco = RECO_PI_F,GEP = GPP_PI_F, CH4_measured = FCH4)]
flux.eddy[, NEP := NEP*12.0107*1800/1000000] # convert micromoles m-2 s-1 to g m-2 half hr-1
flux.eddy[, Reco := Reco*12.0107*1800/1000000]
flux.eddy[, GEP := GEP*-12.0107*1800/1000000] # switch sign and convert units
flux.eddy[, CH4_measured := CH4_measured/1000] # convert nanomoles m-2 s-1 to micromoles m-2 s-1


# Format recent co2 and ch4 data
# co2
co2 <- rbind(co2.2018.2019, co2.2019.2020, co2.2020.2021)
co2[, year := year(ts)]
co2[, month := month(ts)]
co2[, date := parse_date_time(paste(year(ts), month(ts), day(ts), sep = '-'), orders = c('Y!-m!*-d!'))]
co2[, ts.2 := parse_date_time(paste('0000-', month(ts), '-', day(ts), ' ', hour(ts), ':', minute(ts), ':', second(ts), sep = ''), orders = c('Y!-m!*-d! H!:M!:S!'))]
co2[, date.2 := parse_date_time(paste('0000-', month(ts), '-', day(ts), sep = ''), orders = c('Y!-m!*-d!'))]
co2 <- co2[, .(ts, ts.2, date, date.2, month, year, CO2_measured = nee1, NEP, Reco, GEP)]

# ch4
ch4 <- rbind(ch4.2018.2019, ch4.2019.2020, ch4.2020.2021)
ch4[, year := year(ts)]
ch4[, month := month(ts)]
ch4[, date := parse_date_time(paste(year(ts), month(ts), day(ts), sep = '-'), orders = c('Y!-m!*-d!'))]
ch4[, ts.2 := parse_date_time(paste('0000-', month(ts), '-', day(ts), ' ', hour(ts), ':', minute(ts), ':', second(ts), sep = ''), orders = c('Y!-m!*-d! H!:M!:S!'))]
ch4[, date.2 := parse_date_time(paste('0000-', month(ts), '-', day(ts), sep = ''), orders = c('Y!-m!*-d!'))]
ch4 <- ch4[, .(ts, ts.2, date, date.2, month, year, CH4_measured = ch4_flux_filter)]

# join co2 and ch4
flux.eddy.recent <- merge(co2, ch4, by = c('ts', 'ts.2', 'date', 'date.2', 'month', 'year'))

# join old and new
flux.eddy <- rbind(flux.eddy, flux.eddy.recent)

# create flux.eddy year variable
flux.eddy[, flux.year := ifelse(date.2 < as_date('0000-05-01'),
                           year - 1,
                           year)]
flux.eddy[, c.balance := factor(ifelse(NEP > 0,
                                  'release',
                                  'uptake'),
                           levels = c('release', 'uptake'))]
flux.eddy[, ch4.balance := factor(ifelse(CH4_measured >= 0,
                                    'release',
                                    'uptake'),
                             levels = c('release', 'uptake'))]

# Winter Sum
flux.eddy.winter <- flux.eddy[
  month %in% c(seq(1, 4), seq(10, 12)) & flux.year >= 2010
][
  ,
  year := NULL
  ]
flux.eddy.winter <- flux.eddy.winter[
  , 
                                     lapply(.SD, sum), 
                                     by = .(flux.year), 
                                     .SDcols = !c('ts', 'ts.2', 'date', 'date.2', 'month', 'c.balance', 'ch4.balance')
  ][
    ,
    c('CO2_measured', 'CH4_measured') := NULL]
rm(ch4, ch4.2018.2019, ch4.2019.2020, co2, co2.2018.2019, co2.2019.2020,
   flux.eddy, flux.eddy.recent)

# Add winter ec fluxes to growing season chamber fluxes
flux.seasonal <- flux.seasonal %>%
  full_join(flux.eddy.winter, by = c('flux.year'))

flux.seasonal <- flux.seasonal %>%
  mutate(nee.annual = nee.sum - NEP,
         reco.annual = reco.sum + Reco,
         gpp.annual = gpp.sum - GEP,
         treatment = factor(treatment,
                            levels = c('Control',
                                       'Air Warming',
                                       'Soil Warming',
                                       'Air + Soil Warming'))) %>%
  group_by(fence, plot) %>%
  mutate(sub.group = factor(case_when(min(subsidence.annual) > -20 ~ '<20 cm Subsidence',
                                      min(subsidence.annual) > -40 ~ '20-40 cm Subsidence',
                                      min(subsidence.annual) > -60 ~ '40-60 cm Subsidence',
                                      min(subsidence.annual) <= -60 ~ '>=60 cm Subsidence'),
                   levels = c('<20 cm Subsidence', '20-40 cm Subsidence', 
                              '40-60 cm Subsidence', '>=60 cm Subsidence')))

### Plot
### growing season
# NEE
ggplot(flux.seasonal,
       aes(x = flux.year, y = nee.sum, color = subsidence.annual)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  scale_color_viridis(name = 'Subsidence (cm)') +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  facet_wrap(~ treatment) +
  theme_bw()

ggplot(flux.seasonal,
       aes(x = flux.year, y = nee.sum, color = biomass.annual)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  scale_color_viridis(name = 'Biomass') +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  facet_wrap(~ treatment) +
  theme_bw()

ggplot(flux.seasonal,
       aes(x = flux.year, y = nee.sum, color = subsidence.annual)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_color_viridis(name = 'Subsidence (cm)') +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  facet_grid(fence ~ plot) +
  theme_bw()

# Reco
ggplot(flux.seasonal,
       aes(x = flux.year, y = reco.sum, color = subsidence.annual)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  scale_color_viridis(name = 'Subsidence (cm)') +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  facet_wrap(~ treatment) +
  theme_bw()

ggplot(flux.seasonal,
       aes(x = flux.year, y = reco.sum, color = biomass.annual)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  scale_color_viridis(name = 'Biomass') +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  facet_wrap(~ treatment) +
  theme_bw()

ggplot(flux.seasonal,
       aes(x = flux.year, y = reco.sum, color = subsidence.annual)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_color_viridis(name = 'Subsidence (cm)') +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  facet_grid(fence ~ plot) +
  theme_bw()

# GPP
ggplot(flux.seasonal,
       aes(x = flux.year, y = gpp.sum, color = subsidence.annual)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  scale_color_viridis(name = 'Subsidence (cm)') +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  facet_wrap(~ treatment) +
  theme_bw()

ggplot(flux.seasonal,
       aes(x = flux.year, y = gpp.sum, color = biomass.annual)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  scale_color_viridis(name = 'Biomass') +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  facet_wrap(~ treatment) +
  theme_bw()

ggplot(flux.seasonal,
       aes(x = flux.year, y = gpp.sum, color = subsidence.annual)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_color_viridis(name = 'Subsidence (cm)') +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  facet_grid(fence ~ plot) +
  theme_bw()

# annual estimate
# NEE
ggplot(flux.seasonal,
       aes(x = flux.year, y = nee.annual, color = subsidence.annual)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  scale_color_viridis(name = 'Subsidence (cm)') +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  facet_wrap(~ treatment) +
  theme_bw()

ggplot(flux.seasonal,
       aes(x = flux.year, y = nee.annual, color = biomass.annual)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  scale_color_viridis(name = 'Biomass') +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  facet_wrap(~ treatment) +
  theme_bw()

ggplot(flux.seasonal,
       aes(x = flux.year, y = nee.annual, color = subsidence.annual)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_color_viridis(name = 'Subsidence (cm)') +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  facet_grid(fence ~ plot) +
  theme_bw()

ggplot(flux.seasonal,
       aes(x = flux.year, y = nee.annual, color = biomass.annual)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_color_viridis(name = 'Biomass') +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  facet_grid(fence ~ plot) +
  theme_bw()

# Reco
ggplot(flux.seasonal,
       aes(x = flux.year, y = reco.annual, color = subsidence.annual)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  scale_color_viridis(name = 'Subsidence (cm)') +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  facet_wrap(~ treatment) +
  theme_bw()

ggplot(flux.seasonal,
       aes(x = flux.year, y = reco.annual, color = biomass.annual)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  scale_color_viridis(name = 'Biomass') +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  facet_wrap(~ treatment) +
  theme_bw()

ggplot(flux.seasonal,
       aes(x = flux.year, y = reco.annual, color = subsidence.annual)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_color_viridis(name = 'Subsidence (cm)') +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  facet_grid(fence ~ plot) +
  theme_bw()

ggplot(flux.seasonal,
       aes(x = flux.year, y = reco.annual, color = biomass.annual)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_color_viridis(name = 'Biomass') +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  facet_grid(fence ~ plot) +
  theme_bw()

# GPP
ggplot(flux.seasonal,
       aes(x = flux.year, y = gpp.annual, color = subsidence.annual)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  scale_color_viridis(name = 'Subsidence (cm)') +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  facet_wrap(~ treatment) +
  theme_bw()

ggplot(flux.seasonal,
       aes(x = flux.year, y = gpp.annual, color = biomass.annual)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  scale_color_viridis(name = 'Biomass') +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  facet_wrap(~ treatment) +
  theme_bw()

ggplot(flux.seasonal,
       aes(x = flux.year, y = gpp.annual, color = subsidence.annual)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_color_viridis(name = 'Subsidence (cm)') +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  facet_grid(fence ~ plot) +
  theme_bw()

ggplot(flux.seasonal,
       aes(x = flux.year, y = gpp.annual, color = biomass.annual)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_color_viridis(name = 'Biomass') +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  facet_grid(fence ~ plot) +
  theme_bw()

################################################################################