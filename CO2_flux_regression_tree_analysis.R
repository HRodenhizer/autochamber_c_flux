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
library(raster)
library(sf)
library(zoo)
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

### Function to Plot Partial Dependence Plots ###############################################################################
plot.pdp <- function(df1, df2, predictor, response, color.var, shape.var) {
  # print(paste('Running predictor.name <- xxx'))
  predictor.name <- case_when(predictor == 'tp.annual' ~ expression('Thaw Penetration (cm)'), 
                              predictor == 'subsidence.annual' ~ expression('Subsidence (cm)'),
                              predictor == 'alt.annual' ~ expression('ALT (cm)'), 
                              predictor == 'vwc.mean' ~ expression('Mean VWC (%)'), 
                              predictor == 'vwc.sd' ~ expression('SD VWC (%)'), 
                              predictor == 'gwc.mean' ~ expression('Mean GWC (%)'), 
                              predictor == 'gwc.sd' ~ expression('SD GWC (%)'), 
                              predictor == 'wtd.mean' ~ expression('Mean WTD (cm)'), 
                              predictor == 'wtd.sd' ~ expression('SD WTD (cm)'), 
                              predictor == 'precip.sum' ~ expression('Precipitation (mm)'), 
                              predictor == 'winter.snow.depth' ~ expression('Snow Depth (cm)'), 
                              predictor == 'winter.min.t10.min' ~ expression('Winter Min Soil Temp ('*degree*'C)'), 
                              predictor == 't10.mean' ~ expression('Mean Soil Temp ('*degree*'C)'), 
                              predictor == 't10.sd' ~ expression('SD Soil Temp ('*degree*'C)'), 
                              predictor == 'tair.mean' ~ expression('Mean Air Temp ('*degree*'C)'), 
                              predictor == 'tair.sd' ~ expression('SD Air Temp ('*degree*'C)'), 
                              predictor == 'biomass.annual' ~ expression('Biomass (g m'^-2*')'),
                              predictor == 'gdd.2m' ~ expression('2 Month GDD'))

  # print(paste('Running response.name <- xxx'))
  response.name <- case_when(response == 'nee.sum' ~ expression('NEE (gC m'^-2*')'),
                             response == 'gpp.sum' ~ expression('GPP (gC m'^-2*')'),
                             response == 'reco.sum' ~ expression('Reco (gC m'^-2*')'))
  # print(paste('Running color.name <- xxx'))
  color.name <- case_when(color.var == 'flux.year' ~ 'Year',
                          color.var == 'month' ~ 'Month')
  # print(paste('Running color.limits <- xxx'))
  color.breaks <- if (color.var == 'flux.year') {
    seq(2010, 2021)
  } else if (color.var == 'month') {
    seq(5, 9)
  }
  # print(paste('Running color.breaks <- xxx'))
  color.labels <- if (color.var == 'flux.year') {
    seq(2010, 2021)
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
                        direction = -1,
                        discrete = TRUE,
                        breaks = color.breaks,
                        labels = color.labels) +
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
#############################################################################################################################

### Gradient Boosted Regression Tree ########################################################################################
flux.seasonal[, flux.year := factor(flux.year)]
set.seed(21591)

### Seasonal
nee.seasonal <- flux.seasonal[!is.na(nee.sum), 
                              c('nee.sum',
                                'tp.annual',
                                'subsidence.annual',
                                'alt.annual', 
                                'vwc.mean', 'vwc.sd',
                                'gwc.mean', 'gwc.sd',
                                'wtd.mean', 'wtd.sd',
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
                                'wtd.mean', 'wtd.sd',
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
                                 'wtd.mean', 'wtd.sd',
                                 'precip.sum', 'winter.snow.depth',
                                 'winter.min.t10.min',
                                 't10.mean', 't10.sd',
                                 'tair.mean', 'tair.sd',
                                 'biomass.annual')]

# # set.seed doesn't seem to apply to sample
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
#                n.trees = 400,
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
  mutate(var = case_when(var == 'tp.annual' ~ 'Thaw Penetration', 
                         var == 'subsidence.annual' ~ 'Subsidence',
                         var == 'alt.annual' ~ 'ALT', 
                         var == 'vwc.mean' ~ 'Mean VWC', 
                         var == 'vwc.sd' ~ 'SD VWC', 
                         var == 'gwc.mean' ~ 'Mean GWC', 
                         var == 'gwc.sd' ~ 'SD GWC', 
                         var == 'wtd.mean' ~ 'Mean WTD', 
                         var == 'wtd.sd' ~ 'SD WTD', 
                         var == 'precip.sum' ~ 'Precipitation', 
                         var == 'winter.snow.depth' ~ 'Snow Depth', 
                         var == 'winter.min.t10.min' ~ 'Winter Min Soil Temp', 
                         var == 't10.mean' ~ 'Mean Soil Temp',  
                         var == 't10.sd' ~ 'SD Soil Temp', 
                         var == 'tair.mean' ~ 'Mean Air Temp', 
                         var == 'tair.sd' ~ 'SD Air Temp', 
                         var == 'biomass.annual' ~ 'Biomass')) %>%
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
                            n.trees = nee.seasonal.gbm$n.trees),
         nee.resid = nee.pred - nee.sum,
         response = 'nee',
         timescale = 'seasonal')
mean(nee.seasonal.pred$nee.resid^2)
nee.seasonal.lm <- lm(nee.pred ~ nee.sum,
                      data = nee.seasonal.pred)
summary(nee.seasonal.lm)
nee.seasonal.lm.reversed <- lm(nee.sum ~ nee.pred,
                      data = nee.seasonal.pred)
summary(nee.seasonal.lm.reversed)

nee.seasonal.loc.slope <- as.numeric(sqrt(nee.seasonal.lm$coefficients[2] * 
                                 nee.seasonal.lm.reversed$coefficients[2]))
nee.seasonal.loc.intercept <- as.numeric(((nee.seasonal.lm$coefficients[1] * 
                                 nee.seasonal.lm.reversed$coefficients[2] - 
                                 nee.seasonal.lm.reversed$coefficients[1] * 
                                 nee.seasonal.lm$coefficients[2]) / 
                                 (nee.seasonal.lm.reversed$coefficients[2] - 
                                    nee.seasonal.lm$coefficients[2])) - 
  nee.seasonal.loc.slope * ((nee.seasonal.lm.reversed$coefficients[1] - 
                               nee.seasonal.lm$coefficients[1]) / 
                              (nee.seasonal.lm$coefficients[2] -  
                                 nee.seasonal.lm.reversed$coefficients[2])))

ggplot(nee.seasonal.pred, aes(x = nee.sum, y = nee.pred)) +
  geom_point() +
  geom_abline(intercept = nee.seasonal.lm$coefficients[1], 
              slope = nee.seasonal.lm$coefficients[2], color = 'blue') +
  geom_abline(intercept = nee.seasonal.lm.reversed$coefficients[1], 
              slope = nee.seasonal.lm.reversed$coefficients[2], color = 'red') +
  geom_abline(intercept = nee.seasonal.loc.intercept, 
              slope = nee.seasonal.loc.slope, color = 'black')

nee.seasonal.r2 <- summary(nee.seasonal.lm)$r.squared
nee.seasonal.r2.label <- paste0(as.character(expression('R'^2 ~ ' = ')), 
                                ' ~ ', 
                                round(nee.seasonal.r2[1], 2))

nee.seasonal.fit.plot <- ggplot(nee.seasonal.pred, aes(x = nee.sum, y = nee.pred)) +
  geom_point() +
  # geom_smooth(method = 'lm', color = 'black') +
  geom_abline(intercept = nee.seasonal.loc.intercept, slope = nee.seasonal.loc.slope, color = 'black') +
  geom_text(x = -65, y = 200, label = nee.seasonal.r2.label,
            hjust = 0,
            vjust = 0,
            parse = TRUE) +
  scale_x_continuous(name = expression('Measured NEE (gC m'^-2*')')) +
  scale_y_continuous(name = expression('Predicted NEE (gC m'^-2*')')) +
  theme_bw()
nee.seasonal.fit.plot

# plot partial dependence plots of top predictors (with real data points underneath)
example.plots.seasonal <- flux.seasonal %>%
  filter(fence == 4 & plot == 1 | fence == 4 & plot == 6 | fence == 1 & plot == 5)

nee.seasonal.pd.biomass <- nee.seasonal.gbm %>%
  pdp::partial(pred.var = "biomass.annual", n.trees = nee.seasonal.gbm$n.trees,
               grid.resolution = 100)
nee.seasonal.plot.1 <- plot.pdp(df1 = flux.seasonal, df2 = nee.seasonal.pd.biomass,
                                predictor = 'biomass.annual', response = 'nee.sum',
                                color.var = 'flux.year', shape.var = 'treatment')# +
  # geom_point(data = example.plots.seasonal,
  #            aes(x = biomass.annual, y = nee.sum),
  #            inherit.aes = FALSE,
  #            shape = 1, size = 3) +
  # geom_text(data = example.plots.seasonal,
  #           aes(x = biomass.annual, y = nee.sum, label = plot.id),
  #           inherit.aes = FALSE, size = 3, nudge_y = -7) +
  # geom_text(data = example.plots.seasonal,
  #           aes(x = biomass.annual, y = nee.sum, label = ID),
  #           inherit.aes = FALSE, size = 3, nudge_y = -15)
nee.seasonal.plot.1

nee.seasonal.pd.vwc.mean <- nee.seasonal.gbm %>%
  pdp::partial(pred.var = "vwc.mean", n.trees = nee.seasonal.gbm$n.trees,
               grid.resolution = 100)
nee.seasonal.plot.2 <- plot.pdp(df1 = flux.seasonal, df2 = nee.seasonal.pd.vwc.mean,
                                predictor = 'vwc.mean', response = 'nee.sum',
                                color.var = 'flux.year', shape.var = 'treatment')# +
  # geom_point(data = example.plots.seasonal,
  #            aes(x = vwc.mean, y = nee.sum),
  #            inherit.aes = FALSE,
  #            shape = 1, size = 3) +
  # geom_text(data = example.plots.seasonal,
  #           aes(x = vwc.mean, y = nee.sum, label = plot.id),
  #           inherit.aes = FALSE, size = 3, nudge_y = -7) +
  # geom_text(data = example.plots.seasonal,
  #           aes(x = vwc.mean, y = nee.sum, label = ID),
  #           inherit.aes = FALSE, size = 3, nudge_y = -15)
nee.seasonal.plot.2

nee.seasonal.pd.wtd.sd <- nee.seasonal.gbm %>%
  pdp::partial(pred.var = "wtd.sd", n.trees = nee.seasonal.gbm$n.trees,
               grid.resolution = 100)
nee.seasonal.plot.3 <- plot.pdp(df1 = flux.seasonal, df2 = nee.seasonal.pd.wtd.sd,
                                predictor = 'wtd.sd', response = 'nee.sum',
                                color.var = 'flux.year', shape.var = 'treatment')# +
  # geom_point(data = example.plots.seasonal,
  #            aes(x = gwc.sd, y = nee.sum),
  #            inherit.aes = FALSE,
  #            shape = 1, size = 3) +
  # geom_text(data = example.plots.seasonal,
  #           aes(x = gwc.sd, y = nee.sum, label = plot.id),
  #           inherit.aes = FALSE, size = 3, nudge_y = -7) +
  # geom_text(data = example.plots.seasonal,
  #           aes(x = gwc.sd, y = nee.sum, label = ID),
  #           inherit.aes = FALSE, size = 3, nudge_y = -15)
nee.seasonal.plot.3

nee.seasonal.pd.gwc.mean <- nee.seasonal.gbm %>%
  pdp::partial(pred.var = "gwc.mean", n.trees = nee.seasonal.gbm$n.trees,
               grid.resolution = 100)
nee.seasonal.plot.4 <- plot.pdp(df1 = flux.seasonal, df2 = nee.seasonal.pd.gwc.mean,
                                predictor = 'gwc.mean', response = 'nee.sum',
                                color.var = 'flux.year', shape.var = 'treatment')# +
  # geom_point(data = example.plots.seasonal,
  #            aes(x = gwc.mean, y = nee.sum),
  #            inherit.aes = FALSE,
  #            shape = 1, size = 3) +
  # geom_text(data = example.plots.seasonal,
  #           aes(x = gwc.mean, y = nee.sum, label = plot.id),
  #           inherit.aes = FALSE, size = 3, nudge_y = -7) +
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
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/gbm_nee_seasonal_top_4.jpg',
#        nee.seasonal.pd.plot,
#        height = 6.5,
#        width = 6.5,
#        bg = 'white')
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/gbm_nee_seasonal_top_4.pdf',
#        nee.seasonal.pd.plot,
#        height = 6.5,
#        width = 6.5,
#        bg = 'white')

# # explore a few points using LIME
# # I'd love to do this, but the example I'm working from online, 
# # http://uc-r.github.io/gbm_regression, doesn't even work...
# lime.points <- flux.seasonal[-train.nee.seasonal, ][(fence == 1 & plot == 5 | 
#                                                        fence == 4 & plot %in% c(1, 6)) &
#                                                       flux.year == 2019,]
# explainer <- lime(nee.seasonal[train.nee.seasonal,], nee.seasonal.gbm)
# explanation <- lime::explain(lime.points, explainer, n_features = 5)

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
#                          shrinkage = 0.01,
#                          interaction.depth = 6,
#                          n.minobsinnode = 10)
# summary(reco.seasonal.gbm)
# saveRDS(reco.seasonal.gbm, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/reco_seasonal_gbm.rds')
reco.seasonal.gbm <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/reco_seasonal_gbm.rds')
reco.seasonal.influence <- reco.seasonal.gbm %>%
  summary() %>%
  as.data.frame() %>%
  arrange(rel.inf) %>%
  mutate(var = case_when(var == 'tp.annual' ~ 'Thaw Penetration', 
                         var == 'subsidence.annual' ~ 'Subsidence',
                         var == 'alt.annual' ~ 'ALT', 
                         var == 'vwc.mean' ~ 'Mean VWC', 
                         var == 'vwc.sd' ~ 'SD VWC', 
                         var == 'gwc.mean' ~ 'Mean GWC', 
                         var == 'gwc.sd' ~ 'SD GWC', 
                         var == 'wtd.mean' ~ 'Mean WTD', 
                         var == 'wtd.sd' ~ 'SD WTD', 
                         var == 'precip.sum' ~ 'Precipitation', 
                         var == 'winter.snow.depth' ~ 'Snow Depth', 
                         var == 'winter.min.t10.min' ~ 'Winter Min Soil Temp', 
                         var == 't10.mean' ~ 'Mean Soil Temp',  
                         var == 't10.sd' ~ 'SD Soil Temp', 
                         var == 'tair.mean' ~ 'Mean Air Temp', 
                         var == 'tair.sd' ~ 'SD Air Temp', 
                         var == 'biomass.annual' ~ 'Biomass')) %>%
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
                            n.trees = reco.seasonal.gbm$n.trees),
         reco.resid = reco.pred - reco.sum,
         response = 'reco',
         timescale = 'seasonal')
mean(reco.seasonal.pred$reco.resid^2)

reco.seasonal.lm <- lm(reco.pred ~ reco.sum,
                      data = reco.seasonal.pred)
summary(reco.seasonal.lm)
reco.seasonal.lm.reversed <- lm(reco.sum ~ reco.pred,
                               data = reco.seasonal.pred)
summary(reco.seasonal.lm.reversed)

reco.seasonal.loc.slope <- as.numeric(sqrt(reco.seasonal.lm$coefficients[2] * 
                                 reco.seasonal.lm.reversed$coefficients[2]))
reco.seasonal.loc.intercept <- as.numeric(((reco.seasonal.lm$coefficients[1] * 
                                  reco.seasonal.lm.reversed$coefficients[2] - 
                                  reco.seasonal.lm.reversed$coefficients[1] * 
                                  reco.seasonal.lm$coefficients[2]) / 
                                 (reco.seasonal.lm.reversed$coefficients[2] - 
                                    reco.seasonal.lm$coefficients[2])) - 
  reco.seasonal.loc.slope * ((reco.seasonal.lm.reversed$coefficients[1] - 
                               reco.seasonal.lm$coefficients[1]) / 
                              (reco.seasonal.lm$coefficients[2] -  
                                 reco.seasonal.lm.reversed$coefficients[2])))

reco.seasonal.r2 <- summary(reco.seasonal.lm)$r.squared
reco.seasonal.r2.label <- paste0(as.character(expression('R'^2 ~ ' = ')), 
                                 ' ~ ', 
                                 round(reco.seasonal.r2[1], 2))

reco.seasonal.fit.plot <- ggplot(reco.seasonal.pred, aes(x = reco.sum, y = reco.pred)) +
  geom_point() +
  # geom_smooth(method = 'lm', color = 'black') +
  geom_abline(intercept = reco.seasonal.loc.intercept, slope = reco.seasonal.loc.slope, color = 'black') +
  geom_text(x = 150, y = 525, label = reco.seasonal.r2.label,
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
                                color.var = 'flux.year', shape.var = 'treatment')# +
  # geom_point(data = example.plots.seasonal,
  #            aes(x = biomass.annual, y = reco.sum),
  #            inherit.aes = FALSE,
  #            shape = 1, size = 3) +
  # geom_text(data = example.plots.seasonal,
  #           aes(x = biomass.annual, y = reco.sum, label = plot.id),
  #           inherit.aes = FALSE, size = 3, nudge_y = -7)# +
  # geom_text(data = example.plots.seasonal,
  #           aes(x = biomass.annual, y = reco.sum, label = ID),
  #           inherit.aes = FALSE, size = 3, nudge_y = -15)
reco.seasonal.plot.1

reco.seasonal.pd.alt <- reco.seasonal.gbm %>%
  pdp::partial(pred.var = "alt.annual", n.trees = reco.seasonal.gbm$n.trees,
               grid.resolution = 100)
reco.seasonal.plot.2 <- plot.pdp(df1 = flux.seasonal, df2 = reco.seasonal.pd.alt,
                                predictor = 'alt.annual', response = 'reco.sum',
                                color.var = 'flux.year', shape.var = 'treatment')# +
  # geom_point(data = example.plots.seasonal,
  #            aes(x = alt.annual, y = reco.sum),
  #            inherit.aes = FALSE,
  #            shape = 1, size = 3) +
  # geom_text(data = example.plots.seasonal,
  #           aes(x = alt.annual, y = reco.sum, label = plot.id),
  #           inherit.aes = FALSE, size = 3, nudge_y = -7) +
  # geom_text(data = example.plots.seasonal,
  #           aes(x = alt.annual, y = reco.sum, label = ID),
  #           inherit.aes = FALSE, size = 3, nudge_y = -15)
reco.seasonal.plot.2

reco.seasonal.pd.gwc.sd <- reco.seasonal.gbm %>%
  pdp::partial(pred.var = "gwc.sd", n.trees = reco.seasonal.gbm$n.trees,
               grid.resolution = 100)
reco.seasonal.plot.3 <- plot.pdp(df1 = flux.seasonal, df2 = reco.seasonal.pd.gwc.sd,
                                predictor = 'gwc.sd', response = 'reco.sum',
                                color.var = 'flux.year', shape.var = 'treatment')# +
  # geom_point(data = example.plots.seasonal,
  #            aes(x = tair.sd, y = reco.sum),
  #            inherit.aes = FALSE,
  #            shape = 1, size = 3) +
  # geom_text(data = example.plots.seasonal,
  #           aes(x = tair.sd, y = reco.sum, label = plot.id),
  #           inherit.aes = FALSE, size = 3, nudge_y = -7) +
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
  # geom_point(data = example.plots.seasonal,
  #            aes(x = subsidence.annual, y = reco.sum),
  #            inherit.aes = FALSE,
  #            shape = 1, size = 3) +
  # geom_text(data = example.plots.seasonal,
  #           aes(x = subsidence.annual, y = reco.sum, label = plot.id),
  #           inherit.aes = FALSE, size = 3, nudge_y = -7) +
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
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/gbm_reco_seasonal_top_4.jpg',
#        reco.seasonal.pd.plot,
#        height = 6.5,
#        width = 6.5,
#        bg = 'white')
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/gbm_reco_seasonal_top_4.pdf',
#        reco.seasonal.pd.plot,
#        height = 6.5,
#        width = 6.5,
#        bg = 'white')

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
#                 interaction.depth = 5,
#                 n.minobsinnode = 5)
# summary(gpp.seasonal.gbm)
# saveRDS(gpp.seasonal.gbm, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/gpp_seasonal_gbm.rds')
gpp.seasonal.gbm <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/gpp_seasonal_gbm.rds')
summary(gpp.seasonal.gbm)

# Variable Influence Plot
gpp.seasonal.influence <- gpp.seasonal.gbm %>%
  summary() %>%
  as.data.frame() %>%
  arrange(rel.inf) %>%
  mutate(var = case_when(var == 'tp.annual' ~ 'Thaw Penetration', 
                         var == 'subsidence.annual' ~ 'Subsidence',
                         var == 'alt.annual' ~ 'ALT', 
                         var == 'vwc.mean' ~ 'Mean VWC', 
                         var == 'vwc.sd' ~ 'SD VWC', 
                         var == 'gwc.mean' ~ 'Mean GWC', 
                         var == 'gwc.sd' ~ 'SD GWC', 
                         var == 'wtd.mean' ~ 'Mean WTD', 
                         var == 'wtd.sd' ~ 'SD WTD', 
                         var == 'precip.sum' ~ 'Precipitation', 
                         var == 'winter.snow.depth' ~ 'Snow Depth', 
                         var == 'winter.min.t10.min' ~ 'Winter Min Soil Temp', 
                         var == 't10.mean' ~ 'Mean Soil Temp',  
                         var == 't10.sd' ~ 'SD Soil Temp', 
                         var == 'tair.mean' ~ 'Mean Air Temp', 
                         var == 'tair.sd' ~ 'SD Air Temp', 
                         var == 'biomass.annual' ~ 'Biomass')) %>%
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
                             n.trees = gpp.seasonal.gbm$n.trees),
         gpp.resid = gpp.pred - gpp.sum,
         response = 'gpp',
         timescale = 'seasonal')
mean(gpp.seasonal.pred$gpp.resid^2)
gpp.seasonal.lm <- lm(gpp.pred ~ gpp.sum,
                      data = gpp.seasonal.pred)
summary(gpp.seasonal.lm)
gpp.seasonal.lm.reversed <- lm(gpp.sum ~ gpp.pred,
                               data = gpp.seasonal.pred)
summary(gpp.seasonal.lm.reversed)

gpp.seasonal.loc.slope <- as.numeric(sqrt(gpp.seasonal.lm$coefficients[2] * 
                                 gpp.seasonal.lm.reversed$coefficients[2]))
gpp.seasonal.loc.intercept <- as.numeric(((gpp.seasonal.lm$coefficients[1] * 
                                  gpp.seasonal.lm.reversed$coefficients[2] - 
                                  gpp.seasonal.lm.reversed$coefficients[1] * 
                                  gpp.seasonal.lm$coefficients[2]) / 
                                 (gpp.seasonal.lm.reversed$coefficients[2] - 
                                    gpp.seasonal.lm$coefficients[2])) - 
  gpp.seasonal.loc.slope * ((gpp.seasonal.lm.reversed$coefficients[1] - 
                               gpp.seasonal.lm$coefficients[1]) / 
                              (gpp.seasonal.lm$coefficients[2] -  
                                 gpp.seasonal.lm.reversed$coefficients[2])))

gpp.seasonal.r2 <- summary(gpp.seasonal.lm)$r.squared
gpp.seasonal.r2.label <- paste0(as.character(expression('R'^2 ~ ' = ')), 
                                ' ~ ', 
                                round(gpp.seasonal.r2[1], 2))

gpp.seasonal.fit.plot <- ggplot(gpp.seasonal.pred, aes(x = gpp.sum, y = gpp.pred)) +
  geom_point() +
  # geom_smooth(method = 'lm', color = 'black') +
  geom_abline(intercept = gpp.seasonal.loc.intercept, slope = gpp.seasonal.loc.slope, color = 'black') +
  geom_text(x = 50, y = 720, label = gpp.seasonal.r2.label,
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
                                 color.var = 'flux.year', shape.var = 'treatment')# +
  # geom_point(data = example.plots.seasonal,
  #            aes(x = biomass.annual, y = gpp.sum),
  #            inherit.aes = FALSE,
  #            shape = 1, size = 3) +
  # geom_text(data = example.plots.seasonal,
  #           aes(x = biomass.annual, y = gpp.sum, label = plot.id),
  #           inherit.aes = FALSE, size = 3, nudge_y = -7) +
  # geom_text(data = example.plots.seasonal,
  #           aes(x = biomass.annual, y = gpp.sum, label = ID),
  #           inherit.aes = FALSE, size = 3, nudge_y = -15)
gpp.seasonal.plot.1

gpp.seasonal.pd.gwc.sd <- gpp.seasonal.gbm %>%
  pdp::partial(pred.var = "gwc.sd", n.trees = gpp.seasonal.gbm$n.trees,
               grid.resolution = 100)
gpp.seasonal.plot.2 <- plot.pdp(df1 = flux.seasonal, df2 = gpp.seasonal.pd.gwc.sd,
                                 predictor = 'gwc.sd', response = 'gpp.sum',
                                 color.var = 'flux.year', shape.var = 'treatment')# +
  # geom_point(data = example.plots.seasonal,
  #            aes(x = alt.annual, y = gpp.sum),
  #            inherit.aes = FALSE,
  #            shape = 1, size = 3) +
  # geom_text(data = example.plots.seasonal,
  #           aes(x = alt.annual, y = gpp.sum, label = plot.id),
  #           inherit.aes = FALSE, size = 3, nudge_y = -7) +
  # geom_text(data = example.plots.seasonal,
  #           aes(x = alt.annual, y = gpp.sum, label = ID),
  #           inherit.aes = FALSE, size = 3, nudge_y = -15)
gpp.seasonal.plot.2

gpp.seasonal.pd.alt <- gpp.seasonal.gbm %>%
  pdp::partial(pred.var = "alt.annual", n.trees = gpp.seasonal.gbm$n.trees,
               grid.resolution = 100)
gpp.seasonal.plot.3 <- plot.pdp(df1 = flux.seasonal, df2 = gpp.seasonal.pd.alt,
                                 predictor = 'alt.annual', response = 'gpp.sum',
                                 color.var = 'flux.year', shape.var = 'treatment')# +
  # geom_point(data = example.plots.seasonal,
  #            aes(x = gwc.sd, y = gpp.sum),
  #            inherit.aes = FALSE,
  #            shape = 1, size = 3) +
  # geom_text(data = example.plots.seasonal,
  #           aes(x = gwc.sd, y = gpp.sum, label = plot.id),
  #           inherit.aes = FALSE, size = 3, nudge_y = -7) +
  # geom_text(data = example.plots.seasonal,
  #           aes(x = gwc.sd, y = gpp.sum, label = ID),
  #           inherit.aes = FALSE, size = 3, nudge_y = -15)
gpp.seasonal.plot.3

gpp.seasonal.pd.vwc.sd <- gpp.seasonal.gbm %>%
  pdp::partial(pred.var = "vwc.sd", n.trees = gpp.seasonal.gbm$n.trees,
               grid.resolution = 100)
gpp.seasonal.plot.4 <- plot.pdp(df1 = flux.seasonal, df2 = gpp.seasonal.pd.vwc.sd,
                                 predictor = 'vwc.sd', response = 'gpp.sum',
                                 color.var = 'flux.year', shape.var = 'treatment')# +
  # geom_point(data = example.plots.seasonal,
  #            aes(x = tair.sd, y = gpp.sum),
  #            inherit.aes = FALSE,
  #            shape = 1, size = 3) +
  # geom_text(data = example.plots.seasonal,
  #           aes(x = tair.sd, y = gpp.sum, label = plot.id),
  #           inherit.aes = FALSE, size = 3, nudge_y = -7) +
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
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/gbm_gpp_seasonal_top_4.jpg',
#        gpp.seasonal.pd.plot,
#        height = 6.5,
#        width = 6.5,
#        bg = 'white')
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/gbm_gpp_seasonal_top_4.pdf',
#        gpp.seasonal.pd.plot,
#        height = 6.5,
#        width = 6.5,
#        bg = 'white')


### Monthly
flux.monthly[, month := factor(month)]
# include ndvi?
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
# # run model
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
                            n.trees = nee.monthly.gbm$n.trees),
         nee.resid = nee.pred - nee.sum,
         response = 'nee',
         timescale = 'monthly')
mean(nee.monthly.pred$nee.resid^2)
nee.monthly.lm <- lm(nee.pred ~ nee.sum,
                      data = nee.monthly.pred)
summary(nee.monthly.lm)
nee.monthly.lm.reversed <- lm(nee.sum ~ nee.pred,
                               data = nee.monthly.pred)
summary(nee.monthly.lm.reversed)

nee.monthly.loc.slope <- as.numeric(sqrt(nee.monthly.lm$coefficients[2] * 
                                 nee.monthly.lm.reversed$coefficients[2]))
nee.monthly.loc.intercept <- as.numeric(((nee.monthly.lm$coefficients[1] * 
                                  nee.monthly.lm.reversed$coefficients[2] - 
                                  nee.monthly.lm.reversed$coefficients[1] * 
                                  nee.monthly.lm$coefficients[2]) / 
                                 (nee.monthly.lm.reversed$coefficients[2] - 
                                    nee.monthly.lm$coefficients[2])) - 
  nee.monthly.loc.slope * ((nee.monthly.lm.reversed$coefficients[1] - 
                               nee.monthly.lm$coefficients[1]) / 
                              (nee.monthly.lm$coefficients[2] -  
                                 nee.monthly.lm.reversed$coefficients[2])))

nee.monthly.r2 <- summary(nee.monthly.lm)$r.squared
nee.monthly.r2.label <- paste0(as.character(expression('R'^2 ~ ' = ')), 
                               ' ~ ', 
                               round(nee.monthly.r2[1], 2))

# plot model performance
nee.monthly.fit.plot <- ggplot(nee.monthly.pred, aes(x = nee.sum, y = nee.pred)) +
  geom_point() +
  # geom_smooth(method = 'lm', color = 'black') +
  geom_abline(intercept = nee.monthly.loc.intercept, slope = nee.monthly.loc.slope, color = 'black') +
  geom_text(x = -50, y = 110, label = nee.monthly.r2.label,
            hjust = 0,
            vjust = 1,
            parse = TRUE) +
  scale_x_continuous(name = expression('Measured NEE (gC m'^-2*')')) +
  scale_y_continuous(name = expression('Predicted NEE (gC m'^-2*')')) +
  theme_bw()
nee.monthly.fit.plot

nee.monthly.pd.tair.mean <- nee.monthly.gbm %>%
  pdp::partial(pred.var = "tair.mean", n.trees = nee.monthly.gbm$n.trees,
               grid.resolution = 100)
nee.monthly.plot.1 <- plot.pdp(df1 = flux.monthly, df2 = nee.monthly.pd.tair.mean,
                               predictor = 'tair.mean', response = 'nee.sum',
                               color.var = 'month', shape.var = 'treatment')# +
  # geom_point(data = example.plots.monthly,
  #            aes(x = tair.mean, y = nee.sum),
  #            inherit.aes = FALSE,
  #            shape = 1, size = 3) +
  # geom_text(data = example.plots.monthly,
  #           aes(x = tair.mean, y = nee.sum, label = plot.id),
  #           inherit.aes = FALSE, size = 3, nudge_y = -7) +
  # geom_text(data = example.plots.monthly,
  #           aes(x = tair.mean, y = nee.sum, label = ID),
  #           inherit.aes = FALSE, size = 3, nudge_y = -15)
nee.monthly.plot.1

nee.monthly.pd.biomass <- nee.monthly.gbm %>%
  pdp::partial(pred.var = "biomass.annual", n.trees = nee.monthly.gbm$n.trees,
               grid.resolution = 100)
nee.monthly.plot.2 <- plot.pdp(df1 = flux.monthly, df2 = nee.monthly.pd.biomass,
                                predictor = 'biomass.annual', response = 'nee.sum',
                                color.var = 'month', shape.var = 'treatment')# +
  # geom_point(data = example.plots.monthly,
  #            aes(x = biomass.annual, y = nee.sum),
  #            inherit.aes = FALSE,
  #            shape = 1, size = 3) +
  # geom_text(data = example.plots.monthly,
  #           aes(x = biomass.annual, y = nee.sum, label = plot.id),
  #           inherit.aes = FALSE, size = 3, nudge_y = -7) +
  # geom_text(data = example.plots.monthly,
  #           aes(x = biomass.annual, y = nee.sum, label = ID),
  #           inherit.aes = FALSE, size = 3, nudge_y = -15)
nee.monthly.plot.2

nee.monthly.pd.vwc.mean <- nee.monthly.gbm %>%
  pdp::partial(pred.var = "vwc.mean", n.trees = nee.monthly.gbm$n.trees,
               grid.resolution = 100)
nee.monthly.plot.3 <- plot.pdp(df1 = flux.monthly, df2 = nee.monthly.pd.vwc.mean,
                                predictor = 'vwc.mean', response = 'nee.sum',
                                color.var = 'month', shape.var = 'treatment')# +
  # geom_point(data = example.plots.monthly,
  #            aes(x = vwc.mean, y = nee.sum),
  #            inherit.aes = FALSE,
  #            shape = 1, size = 3) +
  # geom_text(data = example.plots.monthly,
  #           aes(x = vwc.mean, y = nee.sum, label = plot.id),
  #           inherit.aes = FALSE, size = 3, nudge_y = -7) +
  # geom_text(data = example.plots.monthly,
  #           aes(x = vwc.mean, y = nee.sum, label = ID),
  #           inherit.aes = FALSE, size = 3, nudge_y = -15)
nee.monthly.plot.3

nee.monthly.pd.gdd.2m <- nee.monthly.gbm %>%
  pdp::partial(pred.var = "gdd.2m", n.trees = nee.monthly.gbm$n.trees,
               grid.resolution = 100)
nee.monthly.plot.4 <- plot.pdp(df1 = flux.monthly, df2 = nee.monthly.pd.gdd.2m,
                                predictor = 'gdd.2m', response = 'nee.sum',
                                color.var = 'month', shape.var = 'treatment')# +
  # geom_point(data = example.plots.monthly,
  #            aes(x = gwc.sd, y = nee.sum),
  #            inherit.aes = FALSE,
  #            shape = 1, size = 3) +
  # geom_text(data = example.plots.monthly,
  #           aes(x = gwc.sd, y = nee.sum, label = plot.id),
  #           inherit.aes = FALSE, size = 3, nudge_y = -7) +
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
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/gbm_nee_monthly_top_4.jpg',
#        nee.monthly.pd.plot,
#        height = 6.5,
#        width = 6.5,
#        bg = 'white')
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/gbm_nee_monthly_top_4.pdf',
#        nee.monthly.pd.plot,
#        height = 6.5,
#        width = 6.5,
#        bg = 'white')


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
#                        n.minobsinnode = 10)
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
                            n.trees = reco.monthly.gbm$n.trees),
         reco.resid = reco.pred - reco.sum,
         response = 'reco',
         timescale = 'monthly')
mean(reco.monthly.pred$reco.resid^2)
reco.monthly.lm <- lm(reco.pred ~ reco.sum,
                      data = reco.monthly.pred)
summary(reco.monthly.lm)
reco.monthly.lm.reversed <- lm(reco.sum ~ reco.pred,
                              data = reco.monthly.pred)
summary(reco.monthly.lm.reversed)

reco.monthly.loc.slope <- as.numeric(sqrt(reco.monthly.lm$coefficients[2] * 
                                reco.monthly.lm.reversed$coefficients[2]))
reco.monthly.loc.intercept <- as.numeric(((reco.monthly.lm$coefficients[1] * 
                                 reco.monthly.lm.reversed$coefficients[2] - 
                                 reco.monthly.lm.reversed$coefficients[1] * 
                                 reco.monthly.lm$coefficients[2]) / 
                                (reco.monthly.lm.reversed$coefficients[2] - 
                                   reco.monthly.lm$coefficients[2])) - 
  reco.monthly.loc.slope * ((reco.monthly.lm.reversed$coefficients[1] - 
                              reco.monthly.lm$coefficients[1]) / 
                             (reco.monthly.lm$coefficients[2] -  
                                reco.monthly.lm.reversed$coefficients[2])))

reco.monthly.r2 <- summary(reco.monthly.lm)$r.squared
reco.monthly.r2.label <- paste0(as.character(expression('R'^2 ~ ' = ')), ' ~ ', round(reco.monthly.r2[1], 2))

reco.monthly.fit.plot <- ggplot(reco.monthly.pred, aes(x = reco.sum, y = reco.pred)) +
  geom_point() +
  # geom_smooth(method = 'lm', color = 'black') +
  geom_abline(intercept = reco.monthly.loc.intercept, slope = reco.monthly.loc.slope, color = 'black') +
  geom_text(x = 10, y = 200, label = reco.monthly.r2.label,
            hjust = 0,
            vjust = 0,
            parse = TRUE) +
  scale_x_continuous(name = expression('Measured Reco (gC m'^-2*')')) +
  scale_y_continuous(name = expression('Predicted Reco (gC m'^-2*')')) +
  theme_bw()
reco.monthly.fit.plot

# plot partial dependence plots with data points
reco.monthly.pd.t10.mean <- reco.monthly.gbm %>%
  pdp::partial(pred.var = "t10.mean", n.trees = reco.monthly.gbm$n.trees,
               grid.resolution = 100)
reco.monthly.plot.1 <- plot.pdp(df1 = flux.monthly, df2 = reco.monthly.pd.t10.mean,
                               predictor = 't10.mean', response = 'reco.sum',
                               color.var = 'month', shape.var = 'treatment')# +
  # geom_point(data = example.plots.monthly,
  #            aes(x = t10.mean, y = reco.sum),
  #            inherit.aes = FALSE,
  #            shape = 1, size = 3) +
  # geom_text(data = example.plots.monthly,
  #           aes(x = t10.mean, y = reco.sum, label = plot.id),
  #           inherit.aes = FALSE, size = 3, nudge_y = -7) +
  # geom_text(data = example.plots.monthly,
  #           aes(x = t10.mean, y = reco.sum, label = ID),
  #           inherit.aes = FALSE, size = 3, nudge_y = -15)
reco.monthly.plot.1

reco.monthly.pd.biomass <- reco.monthly.gbm %>%
  pdp::partial(pred.var = "biomass.annual", n.trees = reco.monthly.gbm$n.trees,
               grid.resolution = 100)
reco.monthly.plot.2 <- plot.pdp(df1 = flux.monthly, df2 = reco.monthly.pd.biomass,
                               predictor = 'biomass.annual', response = 'reco.sum',
                               color.var = 'month', shape.var = 'treatment')# +
  # geom_point(data = example.plots.monthly,
  #            aes(x = biomass.annual, y = reco.sum),
  #            inherit.aes = FALSE,
  #            shape = 1, size = 3) +
  # geom_text(data = example.plots.monthly,
  #           aes(x = biomass.annual, y = reco.sum, label = plot.id),
  #           inherit.aes = FALSE, size = 3, nudge_y = -7) +
  # geom_text(data = example.plots.monthly,
  #           aes(x = biomass.annual, y = reco.sum, label = ID),
  #           inherit.aes = FALSE, size = 3, nudge_y = -15)
reco.monthly.plot.2

reco.monthly.pd.tair <- reco.monthly.gbm %>%
  pdp::partial(pred.var = "tair.mean", n.trees = reco.monthly.gbm$n.trees,
               grid.resolution = 100)
reco.monthly.plot.3 <- plot.pdp(df1 = flux.monthly, df2 = reco.monthly.pd.tair,
                               predictor = 'tair.mean', response = 'reco.sum',
                               color.var = 'month', shape.var = 'treatment')# +
  # geom_point(data = example.plots.monthly,
  #            aes(x = gwc.sd, y = reco.sum),
  #            inherit.aes = FALSE,
  #            shape = 1, size = 3) +
  # geom_text(data = example.plots.monthly,
  #           aes(x = gwc.sd, y = reco.sum, label = plot.id),
  #           inherit.aes = FALSE, size = 3, nudge_y = -7) +
  # geom_text(data = example.plots.monthly,
  #           aes(x = gwc.sd, y = reco.sum, label = ID),
  #           inherit.aes = FALSE, size = 3, nudge_y = -15)
reco.monthly.plot.3

reco.monthly.pd.subsidence <- reco.monthly.gbm %>%
  pdp::partial(pred.var = "subsidence.annual", n.trees = reco.monthly.gbm$n.trees,
               grid.resolution = 100)
reco.monthly.plot.4 <- plot.pdp(df1 = flux.monthly, df2 = reco.monthly.pd.subsidence,
                                predictor = 'subsidence.annual', response = 'reco.sum',
                                color.var = 'month', shape.var = 'treatment') +
  # geom_point(data = example.plots.monthly,
  #            aes(x = subsidence.annual, y = reco.sum),
  #            inherit.aes = FALSE,
  #            shape = 1, size = 3) +
  # geom_text(data = example.plots.monthly,
  #           aes(x = subsidence.annual, y = reco.sum, label = plot.id),
  #           inherit.aes = FALSE, size = 3, nudge_y = -7) +
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
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/gbm_reco_monthly_top_4.jpg',
#        reco.monthly.pd.plot,
#        height = 6.5,
#        width = 6.5,
#        bg = 'white')
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/gbm_reco_monthly_top_4.pdf',
#        reco.monthly.pd.plot,
#        height = 6.5,
#        width = 6.5,
#        bg = 'white')


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
#                        interaction.depth = 6,
#                        n.minobsinnode = 7)
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
                             n.trees = gpp.monthly.gbm$n.trees),
         gpp.resid = gpp.pred - gpp.sum,
         response = 'gpp',
         timescale = 'monthly')
mean(gpp.monthly.pred$gpp.resid^2)
gpp.monthly.lm <- lm(gpp.pred ~ gpp.sum,
                      data = gpp.monthly.pred)
summary(gpp.monthly.lm)
gpp.monthly.lm.reversed <- lm(gpp.sum ~ gpp.pred,
                              data = gpp.monthly.pred)
summary(gpp.monthly.lm.reversed)

gpp.monthly.loc.slope <- as.numeric(sqrt(gpp.monthly.lm$coefficients[2] * 
                                gpp.monthly.lm.reversed$coefficients[2]))
gpp.monthly.loc.intercept <- as.numeric(((gpp.monthly.lm$coefficients[1] * 
                                 gpp.monthly.lm.reversed$coefficients[2] - 
                                 gpp.monthly.lm.reversed$coefficients[1] * 
                                 gpp.monthly.lm$coefficients[2]) / 
                                (gpp.monthly.lm.reversed$coefficients[2] - 
                                   gpp.monthly.lm$coefficients[2])) - 
  gpp.monthly.loc.slope * ((gpp.monthly.lm.reversed$coefficients[1] - 
                              gpp.monthly.lm$coefficients[1]) / 
                             (gpp.monthly.lm$coefficients[2] -  
                                gpp.monthly.lm.reversed$coefficients[2])))

gpp.monthly.r2 <- summary(gpp.monthly.lm)$r.squared
gpp.monthly.r2.label <- paste0(as.character(expression('R'^2 ~ ' = ')), ' ~ ', round(gpp.monthly.r2[1], 2))

gpp.monthly.fit.plot <- ggplot(gpp.monthly.pred, aes(x = gpp.sum, y = gpp.pred)) +
  geom_point() +
  # geom_smooth(method = 'lm', color = 'black') +
  geom_abline(intercept = gpp.monthly.loc.intercept, slope = gpp.monthly.loc.slope, color = 'black') +
  geom_text(x = 0, y = 275, label = gpp.monthly.r2.label,
            hjust = 0,
            vjust = 1,
            parse = TRUE) +
  scale_x_continuous(name = expression('Measured GPP (gC m'^-2*')')) +
  scale_y_continuous(name = expression('Predicted GPP (gC m'^-2*')')) +
  theme_bw()
gpp.monthly.fit.plot

# plot partial dependence plots with data points
gpp.monthly.pd.tair.mean <- gpp.monthly.gbm %>%
  pdp::partial(pred.var = "tair.mean", n.trees = gpp.monthly.gbm$n.trees,
               grid.resolution = 100)
gpp.monthly.plot.1 <- plot.pdp(df1 = flux.monthly, df2 = gpp.monthly.pd.tair.mean,
                               predictor = 'tair.mean', response = 'gpp.sum',
                               color.var = 'month', shape.var = 'treatment')# +
# geom_point(data = example.plots.monthly,
#            aes(x = tair.mean, y = gpp.sum),
#            inherit.aes = FALSE,
#            shape = 1, size = 3) +
# geom_text(data = example.plots.monthly,
#           aes(x = tair.mean, y = gpp.sum, label = plot.id),
#           inherit.aes = FALSE, size = 3, nudge_y = -7) +
# geom_text(data = example.plots.monthly,
#           aes(x = tair.mean, y = gpp.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
gpp.monthly.plot.1

gpp.monthly.pd.biomass <- gpp.monthly.gbm %>%
  pdp::partial(pred.var = "biomass.annual", n.trees = gpp.monthly.gbm$n.trees,
               grid.resolution = 100)
gpp.monthly.plot.2 <- plot.pdp(df1 = flux.monthly, df2 = gpp.monthly.pd.biomass,
                                predictor = 'biomass.annual', response = 'gpp.sum',
                                color.var = 'month', shape.var = 'treatment')# +
  # geom_point(data = example.plots.monthly,
  #            aes(x = biomass.annual, y = gpp.sum),
  #            inherit.aes = FALSE,
  #            shape = 1, size = 3) +
  # geom_text(data = example.plots.monthly,
  #           aes(x = biomass.annual, y = gpp.sum, label = plot.id),
  #           inherit.aes = FALSE, size = 3, nudge_y = -7) +
  # geom_text(data = example.plots.monthly,
  #           aes(x = biomass.annual, y = gpp.sum, label = ID),
  #           inherit.aes = FALSE, size = 3, nudge_y = -15)
gpp.monthly.plot.2

gpp.monthly.pd.t10.mean <- gpp.monthly.gbm %>%
  pdp::partial(pred.var = "t10.mean", n.trees = gpp.monthly.gbm$n.trees,
               grid.resolution = 100)
gpp.monthly.plot.3 <- plot.pdp(df1 = flux.monthly, df2 = gpp.monthly.pd.t10.mean,
                               predictor = 't10.mean', response = 'gpp.sum',
                               color.var = 'month', shape.var = 'treatment')# +
# geom_point(data = example.plots.monthly,
#            aes(x = t10.mean, y = gpp.sum),
#            inherit.aes = FALSE,
#            shape = 1, size = 3) +
# geom_text(data = example.plots.monthly,
#           aes(x = t10.mean, y = gpp.sum, label = plot.id),
#           inherit.aes = FALSE, size = 3, nudge_y = -7) +
# geom_text(data = example.plots.monthly,
#           aes(x = t10.mean, y = gpp.sum, label = ID),
#           inherit.aes = FALSE, size = 3, nudge_y = -15)
gpp.monthly.plot.3

gpp.monthly.pd.vwc.sd <- gpp.monthly.gbm %>%
  pdp::partial(pred.var = "vwc.sd", n.trees = gpp.monthly.gbm$n.trees,
               grid.resolution = 100)
gpp.monthly.plot.4 <- plot.pdp(df1 = flux.monthly, df2 = gpp.monthly.pd.vwc.sd,
                               predictor = 'vwc.sd', response = 'gpp.sum',
                               color.var = 'month', shape.var = 'treatment')# +
  # geom_point(data = example.plots.monthly,
  #            aes(x = vwc.sd, y = gpp.sum),
  #            inherit.aes = FALSE,
  #            shape = 1, size = 3) +
  # geom_text(data = example.plots.monthly,
  #           aes(x = vwc.sd, y = gpp.sum, label = plot.id),
  #           inherit.aes = FALSE, size = 3, nudge_y = -7) +
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
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/gbm_gpp_monthly_top_4.jpg',
#        gpp.monthly.pd.plot,
#        height = 6.5,
#        width = 6.5,
#        bg = 'white')
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/gbm_gpp_monthly_top_4.pdf',
#        gpp.monthly.pd.plot,
#        height = 6.5,
#        width = 6.5,
#        bg = 'white')


### Plot Variable Importance and Model Performance
### Functions
# A function to plot the inset 
get_inset <- function(data.df, fit.df){
  p <- ggplot(data.df, aes(x = flux.measured, y = flux.pred)) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
    geom_point(color = 'gray20') +
    # geom_smooth(method = 'lm', color = 'black') +
    # geom_abline(data = fit.df, aes(intercept = intercept, slope = slope)) +
    geom_segment(data = fit.df, aes(x = xmin,
                                    y = intercept + slope * xmin,
                                    xend = xmax,
                                    yend = intercept + slope * xmax)) +
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
         intercept = c(gpp.seasonal.loc.intercept,
                       nee.seasonal.loc.intercept,
                       reco.seasonal.loc.intercept),
         slope = c(gpp.seasonal.loc.slope,
                   nee.seasonal.loc.slope,
                   reco.seasonal.loc.slope),
         xmin = c(min(gpp.seasonal.pred$gpp.sum),
                  min(nee.seasonal.pred$nee.sum),
                  min(reco.seasonal.pred$reco.sum)),
         xmax = c(max(gpp.seasonal.pred$gpp.sum),
                  max(nee.seasonal.pred$nee.sum),
                  max(reco.seasonal.pred$reco.sum)),
         x = c(50, -70, 100),
         y = c(760, 265, 635))

seasonal.grob.dimensions <- data.frame(xmin = 8,
                                      xmax = 27.5,
                                      ymin = 1,
                                      ymax = 14.5)

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
         intercept = c(gpp.monthly.loc.intercept,
                       nee.monthly.loc.intercept,
                       reco.monthly.loc.intercept),
         slope = c(gpp.monthly.loc.slope,
                   nee.monthly.loc.slope,
                   reco.monthly.loc.slope),
         xmin = c(min(gpp.monthly.pred$gpp.sum),
                  min(nee.monthly.pred$nee.sum),
                  min(reco.monthly.pred$reco.sum)),
         xmax = c(max(gpp.monthly.pred$gpp.sum),
                  max(nee.monthly.pred$nee.sum),
                  max(reco.monthly.pred$reco.sum)),
         x = c(0, -60, 10),
         y = c(315, 115, 285))

monthly.grob.dimensions <- data.frame(xmin = 10,
                                      xmax = 40,
                                      ymin = 1.25,
                                      ymax = 16)


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
                     minor_breaks = seq(0, 28, by = 1),
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
#        height = 7,
#        width = 5.5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/gbm_influence_plot_seasonal.pdf',
#        seasonal.influence.plot,
#        height = 7,
#        width = 5.5)

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
#        width = 5.5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/gbm_influence_plot_monthly.pdf',
#        monthly.influence.plot,
#        height = 7,
#        width = 5.5)


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

### Plot PDP plots all together
seasonal.pdp <- ggarrange(gpp.seasonal.plot.1 +
                            theme(axis.title.y = element_text(margin = margin(r = 6.5, unit = 'pt'))) +
                            facet_grid(. ~ 1), 
                          gpp.seasonal.plot.2 +
                            scale_x_continuous(name = expression('SD GWC (%)'),
                                               breaks = seq(0.5, 1, by = 0.25)) +
                            theme(axis.title.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank(),
                                  axis.title.x = element_text(margin = margin(t = 5.75, unit = 'pt'))) +
                            facet_grid(. ~ 2),
                          gpp.seasonal.plot.3 +
                            theme(axis.title.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank(),
                                  axis.title.x = element_text(margin = margin(t = 5.75, unit = 'pt'))) +
                            facet_grid(. ~ 3), 
                          gpp.seasonal.plot.4 +
                            theme(axis.title.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank(),
                                  axis.title.x = element_text(margin = margin(t = 5.75, unit = 'pt'))) +
                            facet_grid("GPP" ~ 4),
                          nee.seasonal.plot.1 +
                            theme(axis.title.y = element_text(margin = margin(r = 0, unit = 'pt'))), 
                          nee.seasonal.plot.2 +
                            theme(axis.title.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank(),
                                  axis.title.x = element_text(margin = margin(t = 5.75, unit = 'pt'))),
                          nee.seasonal.plot.3 +
                            theme(axis.title.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank(),
                                  axis.title.x = element_text(margin = margin(t = 5.75, unit = 'pt'))),
                          nee.seasonal.plot.4 +
                            theme(axis.title.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank(),
                                  axis.title.x = element_text(margin = margin(t = 5.75, unit = 'pt'))) +
                            facet_grid("NEE" ~ .),
                          reco.seasonal.plot.1 +
                            theme(axis.title.y = element_text(margin = margin(r = 6.5, unit = 'pt'))), 
                          reco.seasonal.plot.2 +
                            theme(axis.title.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank(),
                                  axis.title.x = element_text(margin = margin(t = 5.75, unit = 'pt'))),
                          reco.seasonal.plot.3 +
                            scale_x_continuous(name = expression('SD GWC (%)'),
                                               breaks = seq(0.5, 1, by = 0.25)) +
                            theme(axis.title.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank(),
                                  axis.title.x = element_text(margin = margin(t = 5.75, unit = 'pt'))), 
                          reco.seasonal.plot.4 +
                            theme(axis.title.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank(),
                                  axis.title.x = element_text(margin = margin(t = 5.75, unit = 'pt'))) +
                            facet_grid("Reco" ~ .),
                          ncol = 4, nrow = 3,
                          common.legend = TRUE, legend = 'right',
                          heights = c(1, 0.93, 0.93),
                          widths = c(1, 0.77, 0.77, 0.85))
seasonal.pdp
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/seasonal_pdp.jpg',
#        seasonal.pdp,
#        height = 10,
#        width = 10,
#        bg = 'white')
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/seasonal_pdp.pdf',
#        seasonal.pdp,
#        height = 10,
#        width = 10,
#        bg = 'white')

monthly.pdp <- ggarrange(gpp.monthly.plot.1 +
                           theme(axis.title.x = element_text(margin = margin(t = 5.75, unit = 'pt'))) +
                           facet_grid(. ~ 1), 
                          gpp.monthly.plot.2 +
                            theme(axis.title.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank()) +
                            facet_grid(. ~ 2),
                          gpp.monthly.plot.3 +
                            theme(axis.title.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank(),
                                  axis.title.x = element_text(margin = margin(t = 5.75, unit = 'pt'))) +
                            facet_grid(. ~ 3), 
                          gpp.monthly.plot.4 +
                            theme(axis.title.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank(),
                                  axis.title.x = element_text(margin = margin(t = 5.75, unit = 'pt'))) +
                            facet_grid("GPP" ~ 4),
                          nee.monthly.plot.1 +
                           theme(axis.title.x = element_text(margin = margin(t = 5.75, unit = 'pt'))), 
                          nee.monthly.plot.2 +
                            theme(axis.title.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank()),
                          nee.monthly.plot.3 +
                            theme(axis.title.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank(),
                                  axis.title.x = element_text(margin = margin(t = 5.75, unit = 'pt'))), 
                          nee.monthly.plot.4 +
                            theme(axis.title.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank(),
                                  axis.title.x = element_text(margin = margin(t = 5.75, unit = 'pt'))) +
                            facet_grid("NEE" ~ .),
                          reco.monthly.plot.1 +
                           theme(axis.title.x = element_text(margin = margin(t = 5.75, unit = 'pt'))), 
                          reco.monthly.plot.2 +
                            theme(axis.title.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank()),
                          reco.monthly.plot.3 +
                            theme(axis.title.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank(),
                                  axis.title.x = element_text(margin = margin(t = 5.75, unit = 'pt'))), 
                          reco.monthly.plot.4 +
                            theme(axis.title.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank(),
                                  axis.title.x = element_text(margin = margin(t = 5.75, unit = 'pt'))) +
                            facet_grid("Reco" ~ .),
                          ncol = 4, nrow = 3,
                          common.legend = TRUE, legend = 'right',
                          heights = c(1, 0.93, 0.93),
                          widths = c(1, 0.77, 0.77, 0.85))
monthly.pdp
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/monthly_pdp.jpg',
#        monthly.pdp,
#        height = 10,
#        width = 10,
#        bg = 'white')
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/monthly_pdp.pdf',
#        monthly.pdp,
#        height = 10,
#        width = 10,
#        bg = 'white')
################################################################################

### Time Series Analysis #######################################################
flux.seasonal.filled <- fread('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_annual_filled_2019.csv')

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
flux.eddy.2018.2019 <- fread('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2018-2019/US-EML_HH_201804302330_201904302330.csv',
                             na.strings = c('-9999'))
flux.eddy.2019.2020 <- fread('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2019-2020/US-EML_HH_201904302330_202004292330.csv',
                             na.strings = c('-9999'))
flux.eddy <- rbind(flux.eddy, flux.eddy.2018.2019, flux.eddy.2019.2020,
                   use.names = TRUE, fill = TRUE)
co2.2015.2016 <- loadRData("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2015-2016/AK15_Carbon_new_30Apr2019.Rdata")
load("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2016-2017/AK16_Carbon_new_30Apr2019.Rdata")
co2.2016.2017 <- export
rm(export)
co2.2017.2018 <- loadRData("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2017-2018/AK17_Carbon_new_30Apr2019.Rdata")
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
flux.soil.eddy <- flux.eddy[, .(ts, ts.2, date, date.2, month, year,
                                CO2_measured = FC, TA, PPFD_IN,
                                TS_1_1_1, TS_1_2_1, TS_1_3_1, TS_1_4_1,
                                TS_2_1_1, TS_2_2_1, TS_2_3_1, TS_2_4_1)]
ggplot(flux.soil.eddy[month %in% c(10, 11, 12, 1, 2, 3, 4)], aes(x = ts)) +
  geom_line(aes(y = TS_1_1_1, color = "Sensor 1")) +
  geom_line(aes(y = TS_2_1_1, color = "Sensor 2")) +
  facet_wrap(~ year, scales = "free", ncol = 2)
ggplot(flux.soil.eddy[month %in% c(10, 11, 12, 1, 2, 3, 4) & CO2_measured > 0 & PPFD_IN < 5], 
       aes(x = TS_2_1_1, y = CO2_measured, color = month)) +
  geom_point()
ggplot(flux.soil.eddy[month %in% c(10, 11, 12, 1, 2, 3, 4) & CO2_measured > 0 & PPFD_IN < 5], 
       aes(x = TS_1_1_1, y = CO2_measured, color = month)) +
  geom_point()

flux.eddy <- flux.eddy[, .(ts, ts.2, date, date.2, month, year,
                           CO2_measured = FC, NEP = NEE_PI_F,
                           Reco = RECO_PI_F,GEP = GPP_PI_F,
                           CH4_measured = FCH4)]
flux.eddy[, NEP := NEP*12.0107*1800/1000000] # convert micromoles m-2 s-1 to g m-2 half hr-1
flux.eddy[, Reco := Reco*12.0107*1800/1000000]
flux.eddy[, GEP := GEP*-12.0107*1800/1000000] # switch sign and convert units
flux.eddy[, CH4_measured := CH4_measured/1000] # convert nanomoles m-2 s-1 to micromoles m-2 s-1


# Format recent co2 and ch4 data
# co2
co2 <- rbind(co2.2015.2016, co2.2016.2017, co2.2017.2018, co2.2018.2019, 
             co2.2019.2020, co2.2020.2021)
co2[, year := year(ts)]
co2[, month := month(ts)]
co2[, date := parse_date_time(paste(year(ts), month(ts), day(ts), sep = '-'), orders = c('Y!-m!*-d!'))]
co2[, ts.2 := parse_date_time(paste('0000-', month(ts), '-', day(ts), ' ', hour(ts), ':', minute(ts), ':', second(ts), sep = ''), orders = c('Y!-m!*-d! H!:M!:S!'))]
co2[, date.2 := parse_date_time(paste('0000-', month(ts), '-', day(ts), sep = ''), orders = c('Y!-m!*-d!'))]
co2 <- co2[, .(ts, ts.2, date, date.2, month, year, ppfd, tsoil,
               CO2_measured = nee1, NEP, Reco, GEP)]

# ch4
ch4 <- rbind(ch4.2018.2019, ch4.2019.2020, ch4.2020.2021)
ch4[, year := year(ts)]
ch4[, month := month(ts)]
ch4[, date := parse_date_time(paste(year(ts), month(ts), day(ts), sep = '-'), orders = c('Y!-m!*-d!'))]
ch4[, ts.2 := parse_date_time(paste('0000-', month(ts), '-', day(ts), ' ', hour(ts), ':', minute(ts), ':', second(ts), sep = ''), orders = c('Y!-m!*-d! H!:M!:S!'))]
ch4[, date.2 := parse_date_time(paste('0000-', month(ts), '-', day(ts), sep = ''), orders = c('Y!-m!*-d!'))]
ch4 <- ch4[, .(ts, ts.2, date, date.2, month, year, CH4_measured = ch4_flux_filter)]

# join co2 and ch4
flux.eddy.recent <- merge(co2[ts >= as_date('2018-05-01'), 
                              .(ts, ts.2, date, date.2, month, year, 
                                CO2_measured , NEP, Reco, GEP)], ch4, 
                          by = c('ts', 'ts.2', 'date', 'date.2', 'month', 'year'),
                          all = TRUE)

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

# Estimate winter fluxes using soil temperature model
winter.flux <- flux.soil.eddy[month %in% c(10, 11, 12, 1, 2, 3, 4) & 
                                CO2_measured >= 0 & PPFD_IN < 5]
# convert from micromol/m2/s to g/m2/half hour
winter.flux[, CO2_measured := CO2_measured * 12.0107 * 1800 / 1000000]

# Model EC winter respiration using soil temperature response
# TS_1_1_1 (first sensor, depth = 5 cm)
ggplot(winter.flux, 
       aes(x = TS_1_1_1, y = CO2_measured, color = factor(month))) +
  geom_point() +
  scale_color_viridis(discrete = TRUE)
ggplot(winter.flux, 
       aes(x = TS_1_1_1, y = CO2_measured, color = factor(year))) +
  geom_point() +
  scale_color_viridis(discrete = TRUE)

# TS_2_1_1 (second sensor, depth = 5) 
# has a wider range of temperatures to constrain model - will use this one
ggplot(winter.flux, 
       aes(x = TS_2_1_1, y = CO2_measured, color = factor(month))) +
  geom_point() +
  scale_color_viridis(discrete = TRUE)
ggplot(winter.flux, 
       aes(x = TS_2_1_1, y = CO2_measured, color = factor(year))) +
  geom_point() +
  scale_color_viridis(discrete = TRUE)

m <- function(df){
  mod <- nls(CO2_measured ~ a * exp( b * TS_2_1_1),
             start = list(a=0.1, b=0.1),
             control = nls.control(maxiter = 1000,
                                   tol = 1e-05,
                                   minFactor = 1/3024,
                                   printEval = FALSE,
                                   warnOnly = TRUE),
             data = df)
  
  as.list(c(coef(mod),
            RSS.p =sum(residuals(mod)^2),
            TSS = sum((df$CO2_measured - mean(df$CO2_measured^2)))))
}

winter.tsoil.curve <- m(winter.flux)

# estimate winter fluxes
### Need to run the model with 10 cm depths from the 2nd eddy tower or use a spline to estimate 10 cm soil temp at the eddy tower
load('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/input_data/flux_all.RData')
flux[,
     flux.year := fifelse(month >= 10,
                         year + 1,
                         year)]
ggplot(flux, aes(x = t5, y = nee)) +
  geom_point()

# Should I only include the winter dates in this data.table and then add NGS
# sums to the GS sums separately?
flux[month %in% c(10, 11, 12, 1, 2, 3, 4),
     ':=' (nee = (winter.tsoil.curve$a * exp(winter.tsoil.curve$b * t5.filled))*-1,
           reco = winter.tsoil.curve$a * exp(winter.tsoil.curve$b * t5.filled),
           gpp = 0,
           winter.filled = 1)]
ggplot(flux[month %in% c(10, 11, 12, 1, 2, 3, 4)], 
       aes(x = t5, y = nee)) +
  geom_point()

flux.winter.filled.monthly <- flux[month %in% c(10, 11, 12, 1, 2, 3, 4),
                                  .(nee.sum = sum(nee),
                                    reco.sum = sum(reco),
                                    gpp.sum = sum(gpp)),
                                  by = .(flux.year, month, fence, plot, treatment, plot.id)]

# fill in 2018 October here, by interpolation between Sept. and Nov.
flux.winter.filled.monthly[, ':=' (nee.sum = na.approx(nee.sum, maxgap = 1),
                                   reco.sum = na.approx(reco.sum, maxgap = 1),
                                   gpp.sum = na.approx(gpp.sum, maxgap = 1)),
                           by = c('fence', 'plot')]
flux.winter.filled.monthly[is.na(nee.sum), .N]
flux.winter.filled.monthly[is.na(reco.sum), .N]
flux.winter.filled.monthly[is.na(gpp.sum), .N]

flux.winter.filled.seasonal <- flux.winter.filled.monthly[,
                                  .(nee.sum.winter = sum(nee.sum, na.rm = TRUE),
                                    reco.sum.winter = sum(reco.sum, na.rm = TRUE),
                                    gpp.sum.winter = sum(gpp.sum, na.rm = TRUE)),
                                  by = .(flux.year, fence, plot, treatment, plot.id)]
flux.winter.filled.seasonal[is.na(nee.sum.winter), .N, by = .(fence, plot)]
flux.winter.filled.seasonal[is.na(reco.sum.winter), .N, by = .(fence, plot)]
flux.winter.filled.seasonal[is.na(gpp.sum.winter), .N, by = .(fence, plot)]

ggplot(flux.winter.filled.seasonal[flux.year >= 2010], aes(x = flux.year, y = nee.sum)) +
  geom_point() +
  facet_grid(. ~ treatment)
ggplot(flux.winter.filled.seasonal[flux.year >= 2010], aes(x = flux.year, y = reco.sum)) +
  geom_point() +
  facet_grid(. ~ treatment)
ggplot(flux.winter.filled.seasonal[flux.year >= 2010], aes(x = flux.year, y = gpp.sum)) +
  geom_point() +
  facet_grid(. ~ treatment)

# # Add winter ec fluxes to growing season chamber fluxes
flux.winter.filled.seasonal[, season := 0]
flux.seasonal.filled.winter <- flux.seasonal.filled[, .(flux.year, fence, plot, treatment, plot.id, season,
                         nee.sum, reco.sum, gpp.sum)]
flux.seasonal.filled.winter <- merge(flux.seasonal.filled.winter, flux.winter.filled.seasonal,
                              by = c('flux.year', 'fence', 'plot', 'treatment',
                                     'plot.id', 'season'),
                              all = TRUE)

flux.seasonal.filled.winter[,
                     ':=' (nee.sum = fifelse(is.na(nee.sum),
                                             nee.sum.winter,
                                             nee.sum),
                           reco.sum = fifelse(is.na(reco.sum),
                                              reco.sum.winter,
                                              reco.sum),
                           gpp.sum = fifelse(is.na(gpp.sum),
                                             gpp.sum.winter,
                                             gpp.sum))]

flux.seasonal.filled.winter[is.na(nee.sum), .N]
flux.seasonal.filled.winter[is.na(reco.sum), .N]
flux.seasonal.filled.winter[is.na(gpp.sum), .N]

flux.annual.filled <- flux.seasonal.filled.winter[,
                                           .(nee.sum.annual = sum(nee.sum),
                                             reco.sum.annual = sum(reco.sum),
                                             gpp.sum.annual = sum(gpp.sum)),
                                           by = c('flux.year', 'fence', 'plot', 
                                                  'treatment',  'plot.id')]

flux.seasonal.filled <- flux.seasonal.filled[,
                                             ':=' (nee.sum.gs = nee.sum,
                                                   reco.sum.gs = reco.sum,
                                                   gpp.sum.gs = gpp.sum)]
flux.seasonal.filled <- flux.seasonal.filled[,
                                             ':=' (nee.sum = NULL,
                                                   reco.sum = NULL,
                                                   gpp.sum = NULL)]
flux.annual.filled.plotting <- merge(flux.annual.filled, flux.seasonal.filled,
                                       by = c('flux.year', 'fence', 'plot', 
                                              'treatment',  'plot.id'))
flux.annual.filled.plotting <- flux.annual.filled.plotting[flux.year >= 2010]
flux.annual.filled.plotting[,
                             ':=' (treatment = factor(treatment, 
                                                      levels = c('Control', 'Air Warming',
                                                                 'Soil Warming', 'Air + Soil Warming')),
                                   nee.sum.ngs = nee.sum.annual - nee.sum.gs,
                                   reco.sum.ngs = reco.sum.annual - reco.sum.gs,
                                   gpp.sum.ngs = gpp.sum.annual - gpp.sum.gs)]

### Create a table with GS, NGS, and annual values
flux.summary.winter <- copy(flux.seasonal.filled.winter)
flux.summary.winter[, ':=' (nee.sum.winter = NULL,
                            reco.sum.winter = NULL,
                            gpp.sum.winter = NULL)]
flux.summary.winter <- flux.summary.winter[, .(nee.sum = mean(nee.sum),
                        reco.sum = mean(reco.sum),
                        gpp.sum = mean(gpp.sum)),
                    by = .(flux.year, season, treatment)]
flux.summary.winter <- dcast(melt(flux.summary.winter, 
                                  id.vars = c('flux.year', 'season', 'treatment'),
                                  value.vars = c('nee.sum', 'reco.sum', 'gpp.sum'),
                                  variable.name = 'type',
                                  value.name = 'flux'),
                             flux.year + treatment ~ type + season, 
                             value.var = 'flux',
                             sep = '.')
flux.summary.winter[,
                    ':=' (nee.sum.annual = nee.sum.0 + nee.sum.1,
                          reco.sum.annual = reco.sum.0 + reco.sum.1,
                          gpp.sum.annual = gpp.sum.0 + gpp.sum.1,
                          treatment = factor(treatment,
                                              levels = c('Control', 'Air Warming',
                                                         'Soil Warming', 'Air + Soil Warming')))]
flux.summary.winter <- flux.summary.winter[,
                    lapply(.SD, round, 0),
                    .SD = c('nee.sum.1', 'nee.sum.0', 'nee.sum.annual', 
                            'reco.sum.1', 'reco.sum.0', 'reco.sum.annual',
                            'gpp.sum.1', 'gpp.sum.0', 'gpp.sum.annual'),
                    by = .(flux.year, treatment)]
flux.summary.winter <- flux.summary.winter[order(flux.year, treatment)]
flux.summary.winter <- flux.summary.winter[, .(Year = flux.year, Treatment = treatment, 
                        `GS NEE` = nee.sum.1, `NGS NEE` = nee.sum.0,
                        `Annual NEE` = nee.sum.annual,
                        `GS Reco` = reco.sum.1, `NGS Reco` = reco.sum.0,
                        `Annual Reco` = reco.sum.annual,
                        `GS GPP` = gpp.sum.1, `NGS GPP` = gpp.sum.0,
                        `Annual GPP` = gpp.sum.annual)]
# write.csv(flux.summary.winter,
#           '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/tables/flux_summary.csv',
#           row.names = FALSE)

### Plot
### growing season
# NEE
# 2019 filled
ggplot(flux.annual.filled.plotting,
       aes(x = flux.year, y = nee.sum.gs, color = subsidence.annual, shape = factor(filled.gbm))) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), color = 'black') +
  scale_color_viridis(name = 'Subsidence (cm)') +
  scale_shape_manual(name = '',
                     labels = c('Gap Filled Data', 'Modeled Only'),
                     values = c(16, 1)) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  scale_y_continuous(name = expression('GS NEE (gC m'^-2*')')) +
  facet_wrap(~ treatment, ncol = 4) +
  theme_bw() +
  theme(axis.title.x = element_blank())

nee.trajectory.gs <- ggplot(flux.annual.filled.plotting,
       aes(x = flux.year, y = nee.sum.gs, color = biomass.annual, shape = factor(filled.gbm))) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), color = 'black') +
  scale_color_viridis(name = expression('Biomass (g m'^-2*')')) +
  scale_shape_manual(name = '',
                     labels = c('Gap Filled Data', 'Modeled Only'),
                     values = c(16, 1)) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  scale_y_continuous(name = expression('GS NEE (gC m'^-2*')')) +
  facet_grid('NEE' ~ treatment) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90))
nee.trajectory.gs

nee.trajectory <- ggplot(flux.annual.filled.plotting,
                            aes(x = flux.year, y = nee.sum.annual, color = biomass.annual, shape = factor(filled.gbm))) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), color = 'black') +
  scale_color_viridis(name = expression('Biomass (g m'^-2*')')) +
  scale_shape_manual(name = '',
                     labels = c('Gap Filled Data', 'Modeled Only'),
                     values = c(16, 1)) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  scale_y_continuous(name = expression('Annual NEE (gC m'^-2*')')) +
  facet_grid('NEE' ~ treatment) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90))
nee.trajectory

ggplot(flux.annual.filled.plotting,
       aes(x = flux.year, y = nee.sum.gs, color = subsidence.annual, shape = factor(filled.gbm))) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), color = 'black') +
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_color_viridis(name = 'Subsidence (cm)') +
  scale_shape_manual(name = '',
                     labels = c('Gap Filled Data', 'Modeled Only'),
                     values = c(16, 1)) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  scale_y_continuous(name = expression('GS NEE (gC m'^-2*')')) +
  facet_grid(fence ~ plot) +
  theme_bw() +
  theme(axis.title.x = element_blank())

# Reco
ggplot(flux.annual.filled.plotting,
       aes(x = flux.year, y = reco.sum.gs, color = subsidence.annual, shape = factor(filled.gbm))) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), color = 'black') +
  scale_color_viridis(name = 'Subsidence (cm)') +
  scale_shape_manual(name = '',
                     labels = c('Gap Filled Data', 'Modeled Only'),
                     values = c(16, 1)) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  scale_y_continuous(name = expression('GS Reco (gC m'^-2*')')) +
  facet_wrap(~ treatment) +
  theme_bw() +
  theme(axis.title.x = element_blank())

reco.trajectory.gs <- ggplot(flux.annual.filled.plotting,
       aes(x = flux.year, y = reco.sum.gs, color = biomass.annual, shape = factor(filled.gbm))) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), color = 'black') +
  scale_color_viridis(name = expression('Biomass (g m'^-2*')')) +
  scale_shape_manual(name = '',
                     labels = c('Gap Filled Data', 'Modeled Only'),
                     values = c(16, 1)) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  scale_y_continuous(name = expression('GS Reco (gC m'^-2*')')) +
  facet_grid('Reco' ~ treatment) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 7)),
        axis.text.x = element_text(angle = 90))
reco.trajectory.gs

reco.trajectory <- ggplot(flux.annual.filled.plotting,
                          aes(x = flux.year, y = reco.sum.annual, color = biomass.annual, shape = factor(filled.gbm))) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), color = 'black') +
  scale_color_viridis(name = expression('Biomass (g m'^-2*')')) +
  scale_shape_manual(name = '',
                     labels = c('Gap Filled Data', 'Modeled Only'),
                     values = c(16, 1)) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  scale_y_continuous(name = expression('Annual Reco (gC m'^-2*')')) +
  facet_grid('Reco' ~ treatment) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 7)),
        axis.text.x = element_text(angle = 90))
reco.trajectory

ggplot(flux.annual.filled.plotting,
       aes(x = flux.year, y = reco.sum.gs, color = subsidence.annual, shape = factor(filled.gbm))) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), color = 'black') +
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_color_viridis(name = 'Subsidence (cm)') +
  scale_shape_manual(name = '',
                     labels = c('Gap Filled Data', 'Modeled Only'),
                     values = c(16, 1)) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  scale_y_continuous(name = expression('GS Reco (gC m'^-2*')')) +
  facet_grid(fence ~ plot) +
  theme_bw() +
  theme(axis.title.x = element_blank())

# GPP
ggplot(flux.annual.filled.plotting,
       aes(x = flux.year, y = gpp.sum.gs, color = subsidence.annual, shape = factor(filled.gbm))) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), color = 'black') +
  scale_color_viridis(name = 'Subsidence (cm)') +
  scale_shape_manual(name = '',
                     labels = c('Gap Filled Data', 'Modeled Only'),
                     values = c(16, 1)) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  scale_y_continuous(name = expression('GS GPP (gC m'^-2*')')) +
  facet_wrap(~ treatment) +
  theme_bw() +
  theme(axis.title.x = element_blank())

gpp.trajectory.gs <- ggplot(flux.annual.filled.plotting,
       aes(x = flux.year, y = gpp.sum.gs, color = biomass.annual, shape = factor(filled.gbm))) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), color = 'black') +
  scale_color_viridis(name = expression('Biomass (g m'^-2*')')) +
  scale_shape_manual(name = '',
                     labels = c('Gap Filled Data', 'Modeled Only'),
                     values = c(16, 1)) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  scale_y_continuous(name = expression('GS GPP (gC m'^-2*')')) +
  facet_grid('GPP' ~ treatment) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 7)),
        axis.text.x = element_text(angle = 90))
gpp.trajectory.gs

gpp.trajectory <- ggplot(flux.annual.filled.plotting,
                            aes(x = flux.year, y = gpp.sum.annual, color = biomass.annual, shape = factor(filled.gbm))) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), color = 'black') +
  scale_color_viridis(name = expression('Biomass (g m'^-2*')')) +
  scale_shape_manual(name = '',
                     labels = c('Gap Filled Data', 'Modeled Only'),
                     values = c(16, 1)) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  scale_y_continuous(name = expression('Annual GPP (gC m'^-2*')')) +
  facet_grid('GPP' ~ treatment) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 7)),
        axis.text.x = element_text(angle = 90))
gpp.trajectory

ggplot(flux.annual.filled.plotting,
       aes(x = flux.year, y = gpp.sum.gs, color = subsidence.annual, shape = factor(filled.gbm))) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), color = 'black') +
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_color_viridis(name = 'Subsidence (cm)') +
  scale_shape_manual(name = '',
                     labels = c('Gap Filled Data', 'Modeled Only'),
                     values = c(16, 1)) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  scale_y_continuous(name = expression('GS GPP (gC m'^-2*')')) +
  facet_grid(fence ~ plot) +
  theme_bw() +
  theme(axis.title.x = element_blank())


### All fluxes in same plot
flux.trajectory.gs <- ggarrange(gpp.trajectory.gs +
                               theme(axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank()),
                             nee.trajectory.gs +
                               theme(strip.background.x = element_blank(),
                                     strip.text.x = element_blank(),
                                     axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank()),
                             reco.trajectory.gs +
                               theme(strip.background.x = element_blank(),
                                     strip.text.x = element_blank()),
                             ncol = 1,
                             heights = c(1, 0.9, 0.9),
                             widths = c(1, 0.85, 0.85, 0.88),
                             common.legend = TRUE,
                             legend = 'right')
flux.trajectory.gs
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/flux_trajectory_gs.jpg',
#        flux.trajectory.gs,
#        height = 7,
#        width = 7,
#        bg = 'white')
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/flux_trajectory_gs.pdf',
#        flux.trajectory.gs,
#        height = 7,
#        width = 7,
#        bg = 'white')

flux.trajectory <- ggarrange(gpp.trajectory +
                               theme(axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank()),
                             nee.trajectory +
                               theme(strip.background.x = element_blank(),
                                     strip.text.x = element_blank(),
                                     axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank()),
                             reco.trajectory +
                               theme(strip.background.x = element_blank(),
                                     strip.text.x = element_blank()),
                             ncol = 1,
                             heights = c(1, 0.9, 0.9),
                             widths = c(1, 0.85, 0.85, 0.88),
                             common.legend = TRUE,
                             legend = 'right')
flux.trajectory
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/flux_trajectory_annual.jpg',
#        flux.trajectory,
#        height = 7,
#        width = 7,
#        bg = 'white')
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/flux_trajectory_annual.pdf',
#        flux.trajectory,
#        height = 7,
#        width = 7,
#        bg = 'white')

### Flux Treatment Difference Plot
flux.annual.treat.diff <- flux.annual.filled.plotting[, .(flux.year, block, fence, plot, plot.id, treatment,
                                                          nee.sum.gs, reco.sum.gs, gpp.sum.gs)]
flux.annual.treat.diff <- melt(flux.annual.treat.diff,
                               id.vars = c('flux.year', 'treatment', 'fence', 'plot'),
                               measure.vars = c('nee.sum.gs', 'reco.sum.gs', 'gpp.sum.gs'),
                               value.name = 'flux')
flux.annual.treat.diff <- flux.annual.treat.diff[,
                                                 .(flux = mean(flux)),
                                                 by = c('flux.year', 'treatment', 'variable')]
flux.annual.treat.diff <- dcast(flux.annual.treat.diff,
                                flux.year + variable ~ treatment,
                                value.var = 'flux')
flux.annual.treat.diff[,
                       ':=' (flux.diff = `Soil Warming` - Control,
                             variable = factor(fifelse(variable == 'nee.sum.gs',
                                                       'NEE',
                                                       fifelse(variable == 'reco.sum.gs',
                                                               'Reco',
                                                               'GPP')),
                                               levels = c('NEE', 'Reco', 'GPP')))]

flux.colors <- c('NEE' = '#00CCFF', 'GPP' = '#009933', 'Reco' = '#663300')

# fluxes match Marguerite's graph through 2018, but error bars are a bit smaller
flux.treat.plot.2019.filled <- ggplot(flux.annual.treat.diff,
       aes(x = variable, y = flux.diff, fill = variable)) +
  geom_col(width = 1) +
  # geom_errorbar(aes(ymax = flux.diff + flux.diff.se, ymin = flux.diff - flux.diff.se),
  #               width = 0.25) +
  geom_hline(yintercept = 0, size = 0.5) +
  facet_grid(. ~ flux.year) +
  scale_y_continuous(name = expression(Delta ~ 'Growing Season C Flux (Soil Warming - Control; g C ' ~ m^2 ~ ')')) +
  scale_fill_manual(values = flux.colors, 
                    breaks = c('NEE', 'Reco', 'GPP')) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(0.001, 0.999),
        legend.justification = c(0, 1),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.spacing.x = unit(0, 'cm'))
flux.treat.plot.2019.filled

# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/flux_treatment_difference_plot_2019_filled.jpg',
#        flux.treat.plot.2019.filled,
#        height = 9,
#        width = 12)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/flux_treatment_difference_plot_2019_filled.pdf',
#        flux.treat.plot.2019.filled,
#        height = 9,
#        width = 12)
################################################################################

### Fluxes and PCA Results #####################################################
pca.scores <- fread('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/env_pca_output.csv')

flux.annual.filled.plotting <- merge(flux.annual.filled.plotting, 
                                     pca.scores[, .(flux.year, block, fence, plot, plot.id, treatment,
                                                    nee.sum.gs = nee.sum, reco.sum.gs = reco.sum, gpp.sum.gs = gpp.sum,
                                                    PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10,
                                                    PC11, PC12, PC13, PC14, PC15, PC16, PC17, PC18, PC19, PC20,
                                                    PC21, PC22, PC23)],
                                     by = c('flux.year', 'block', 'fence', 'plot', 'plot.id', 'treatment',
                                            'nee.sum.gs', 'reco.sum.gs', 'gpp.sum.gs'))

# plot nee by principle components
nee.pc1.plot <- ggplot(flux.annual.filled.plotting, aes(x = PC1, y = nee.sum.gs, color = factor(flux.year))) +
  geom_point() +
  scale_y_continuous(name = expression('GS NEE (gC m'^-2*')')) +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  theme_bw() +
  theme(legend.title = element_blank())
nee.pc1.plot

ggplot(flux.annual.filled.plotting, aes(x = PC2, y = nee.sum.gs, color = factor(flux.year))) +
  geom_point() +
  scale_y_continuous(name = expression('GS NEE (gC m'^-2*')')) +
scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  theme_bw() +
  theme(legend.title = element_blank())

# plot reco by principle components
reco.pc1.plot <- ggplot(flux.annual.filled.plotting, aes(x = PC1, y = reco.sum.gs, color = factor(flux.year))) +
  geom_point() +
  scale_y_continuous(name = expression('GS Reco (gC m'^-2*')')) +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  theme_bw() +
  theme(legend.title = element_blank())
reco.pc1.plot

ggplot(flux.annual.filled.plotting, aes(x = PC2, y = reco.sum.gs, color = factor(flux.year))) +
  geom_point() +
  scale_y_continuous(name = expression('GS Reco (gC m'^-2*')')) +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  theme_bw() +
  theme(legend.title = element_blank())

# plot gpp by principle components
gpp.pc1.plot <- ggplot(flux.annual.filled.plotting, aes(x = PC1, y = gpp.sum.gs, color = factor(flux.year))) +
  geom_point() +
  scale_y_continuous(name = expression('GS GPP (gC m'^-2*')')) +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  theme_bw() +
  theme(legend.title = element_blank())
gpp.pc1.plot

ggplot(flux.annual.filled.plotting, aes(x = PC2, y = gpp.sum.gs, color = factor(flux.year))) +
  geom_point() +
  scale_y_continuous(name = expression('GS GPP (gC m'^-2*')')) +
  scale_color_viridis(discrete = TRUE,
                      direction = -1) +
  theme_bw() +
  theme(legend.title = element_blank())

# combine
flux.pc1.plot <- ggarrange(gpp.pc1.plot,
                           nee.pc1.plot,
                           reco.pc1.plot,
                           ncol = 1,
                           legend = 'right',
                           common.legend = TRUE)
flux.pc1.plot
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/flux_pc1.jpg',
#        flux.pc1.plot,
#        height = 7,
#        width = 4,
#        bg = 'white')
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/flux_pc1.pdf',
#        flux.pc1.plot,
#        height = 7,
#        width = 4,
#        bg = 'white')
################################################################################

### Investigate relationship between soil moisture variables and biomass #######
biomass.hydrology.plot.1 <- ggplot(flux.seasonal, 
       aes(x = subsidence.annual*-1, biomass.annual,
           color = flux.year)) +
  geom_point(aes(shape = treatment)) +
  # geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), color = 'black') +
  scale_x_continuous(name = 'Subsidence (cm)') +
  scale_y_continuous(name = expression('Biomass (gC' ~ m^-2*')')) +
  scale_shape_manual(values = c(1, 0, 16, 15),
                     guide = guide_legend(order = 1)) +
  scale_color_viridis(breaks = seq(2010, 2020),
                      direction = -1,
                      guide = guide_legend(order = 2)) +
  theme_bw() +
  theme(legend.title = element_blank())
biomass.hydrology.plot.2 <- ggplot(flux.seasonal, 
       aes(x = wtd.mean*-1, biomass.annual, 
           color = flux.year)) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_point(aes(shape = treatment)) +
  # geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), color = 'black') +
  scale_x_continuous(name = 'WTD (cm)') +
  scale_y_continuous(name = expression('Biomass (gC' ~ m^-2*')')) +
  scale_shape_manual(values = c(1, 0, 16, 15),
                     guide = guide_legend(order = 1)) +
  scale_color_viridis(breaks = seq(2010, 2020),
                      direction = -1,
                      guide = guide_legend(order = 2)) +
  theme_bw() +
  theme(legend.title = element_blank())
biomass.hydrology.plot.3 <- ggplot(flux.seasonal, 
       aes(x = vwc.mean, biomass.annual, 
           color = flux.year)) +
  geom_point(aes(shape = treatment)) +
  # geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), color = 'black') +
  scale_x_continuous(name = 'VWC (%)') +
  scale_y_continuous(name = expression('Biomass (gC' ~ m^-2*')')) +
  scale_shape_manual(values = c(1, 0, 16, 15),
                     guide = guide_legend(order = 1)) +
  scale_color_viridis(breaks = seq(2010, 2020),
                      direction = -1,
                      guide = guide_legend(order = 2)) +
  theme_bw() +
  theme(legend.title = element_blank())
biomass.hydrology.plot.4 <- ggplot(flux.seasonal, 
       aes(x = gwc.mean, biomass.annual, 
           color = flux.year)) +
  geom_point(aes(shape = treatment)) +
  # geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), color = 'black') +
  scale_x_continuous(name = 'GWC (%)') +
  scale_y_continuous(name = expression('Biomass (gC' ~ m^-2*')')) +
  scale_shape_manual(values = c(1, 0, 16, 15),
                     guide = guide_legend(order = 1)) +
  scale_color_viridis(breaks = seq(2010, 2020),
                      direction = -1,
                      guide = guide_legend(order = 2)) +
  theme_bw() +
  theme(legend.title = element_blank())

biomass.hydrology.plot.5 <- ggplot(flux.seasonal, 
                                   aes(x = wtd.sd, biomass.annual, 
                                       color = flux.year)) +
  geom_point(aes(shape = treatment)) +
  # geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), color = 'black') +
  scale_x_continuous(name = 'SD WTD (cm)') +
  scale_y_continuous(name = expression('Biomass (gC' ~ m^-2*')')) +
  scale_shape_manual(values = c(1, 0, 16, 15),
                     guide = guide_legend(order = 1)) +
  scale_color_viridis(breaks = seq(2010, 2020),
                      direction = -1,
                      guide = guide_legend(order = 2)) +
  theme_bw() +
  theme(legend.title = element_blank())
biomass.hydrology.plot.6 <- ggplot(flux.seasonal, 
                                   aes(x = vwc.sd, biomass.annual, 
                                       color = flux.year)) +
  geom_point(aes(shape = treatment)) +
  # geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), color = 'black') +
  scale_x_continuous(name = 'SD VWC (%)') +
  scale_y_continuous(name = expression('Biomass (gC' ~ m^-2*')')) +
  scale_shape_manual(values = c(1, 0, 16, 15),
                     guide = guide_legend(order = 1)) +
  scale_color_viridis(breaks = seq(2010, 2020),
                      direction = -1,
                      guide = guide_legend(order = 2)) +
  theme_bw() +
  theme(legend.title = element_blank())
biomass.hydrology.plot.7 <- ggplot(flux.seasonal, 
                                   aes(x = gwc.sd, biomass.annual, 
                                       color = flux.year)) +
  geom_point(aes(shape = treatment)) +
  # geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), color = 'black') +
  scale_x_continuous(name = 'SD GWC (%)',
                     breaks = seq(0.5, 1, by = 0.25)) +
  scale_y_continuous(name = expression('Biomass (gC' ~ m^-2*')')) +
  scale_shape_manual(values = c(1, 0, 16, 15),
                     guide = guide_legend(order = 1)) +
  scale_color_viridis(breaks = seq(2010, 2020),
                      direction = -1,
                      guide = guide_legend(order = 2)) +
  theme_bw() +
  theme(legend.title = element_blank())

biomass.hydrology.plot <- ggarrange(biomass.hydrology.plot.4,
                                    biomass.hydrology.plot.7,
                                    biomass.hydrology.plot.3,
                                    biomass.hydrology.plot.6,
                                    biomass.hydrology.plot.2,
                                    biomass.hydrology.plot.5,
                                    # biomass.hydrology.plot.1,
                                    ncol = 2,
                                    nrow = 3,
                                    common.legend = TRUE,
                                    legend = "right",
                                    labels = LETTERS[1:6])
biomass.hydrology.plot

# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/biomass_moisture.jpg',
#        biomass.hydrology.plot,
#        height = 7,
#        width = 6.5,
#        bg = 'white') # As of 9/24/21, with no updates to R, R packages, or OS, this started plotting with a black background... I have no idea what might have changed
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/biomass_moisture.pdf',
#        biomass.hydrology.plot,
#        height = 7,
#        width = 6.5)
################################################################################

### Impact of TK Classification on 2018 fluxes #################################
cip.bnd <- st_read('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/CiPEHR_bnd_NAD83.shp')
tk.edges <- brick(stack(raster('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/karst_edges_1.tif'),
                        raster('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/karst_edges_2.tif'),
                        raster('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/karst_edges_3.tif')))
plots <- st_read('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/plot_coordinates_from_2017.shp')

cip.bnd <- st_transform(cip.bnd, st_crs(tk.edges))
plots <- plots %>%
  filter(!is.na(as.numeric(plot))) %>%
  mutate(plot = as.numeric(plot)) %>%
  st_transform(st_crs(tk.edges))

# crop thermokarst edges to cipehr
tk.edges.cip <- crop(tk.edges, cip.bnd)
# adjust edges to be both the cells immediately outside the thermokarst depression
# and immediately inside
tk.centers.cip <- tk.edges.cip
edges.inner.cip <- tk.edges.cip
for (years.n in 1:nlayers(tk.edges.cip)) {
  tk.centers.cip[[years.n]][tk.centers.cip[[years.n]] != 1] <- NA
  plot(tk.centers.cip[[years.n]])
  edges.inner.cip[[years.n]] <- boundaries(tk.centers.cip[[years.n]], directions = 4)
  edges.inner.cip[[years.n]][is.na(edges.inner.cip[[years.n]])] <- 0
  plot(edges.inner.cip[[years.n]])
}

tk.edges.cip <- tk.edges.cip + edges.inner.cip
plot(tk.edges.cip)

ggplot(tk.edges.cip %>%
         as.data.frame(xy = TRUE),
       aes(x = x, y = y, color = karst_edges_1, fill = karst_edges_1)) +
  geom_tile() +
  geom_sf(data = plots, inherit.aes = FALSE) +
  scale_fill_viridis() +
  scale_color_viridis()

plots.tk.class <- raster::extract(tk.edges.cip, as(plots, 'Spatial'), df = TRUE) %>%
  as.data.frame() %>%
  rename(tk.class.2017 = 2,
         tk.class.2018 = 3,
         tk.class.2019 = 4) %>%
  cbind.data.frame(plots) %>%
  pivot_longer(tk.class.2017:tk.class.2019, names_to = 'flux.year', values_to = 'tk.class') %>%
  mutate(flux.year = as.integer(str_sub(flux.year, start = 10)),
         tk.class = factor(case_when(tk.class == 1 ~ 'TK Center',
                                     tk.class == 2 ~ 'TK Edge',
                                     tk.class == 0 ~ 'Non-TK'),
                           levels = c('Pre-Thaw', 'Non-TK', 'TK Edge', 'TK Center'))) %>%
  select(flux.year, fence, plot, tk.class) %>%
  rbind.data.frame(plots %>%
                     st_drop_geometry() %>%
                     mutate(flux.year = as.integer(2010),
                            tk.class = factor('Pre-Thaw')) %>%
                     select(flux.year, fence, plot, tk.class)) %>%
  arrange(fence, plot, flux.year) %>%
  as.data.table()

# PLots 1-5 and 1-6 were edge, center, edge in 2017, 2018, and 2019, respectively
# Will call them edge for all three years
plots.tk.class[fence == 1 & plot %in% c(5, 6) & flux.year == 2018,
               tk.class := factor('TK Edge')]
# plot 4-7 was center, edge, center in 2017, 2018, and 2019 respectively
# Will call it center
plots.tk.class[fence == 4 & plot == 7 & flux.year == 2018,
               tk.class := factor('TK Center')]
# plots 4-6 and 5-6 got classified as edges, but are really centers
plots.tk.class[fence %in% c(4, 5) & plot == 6,
               tk.class := factor('TK Center')]


flux.tk <- merge(flux.annual.filled.plotting[flux.year == 2010 | 
                                               flux.year >= 2017 & flux.year <= 2019], 
                 plots.tk.class,
                 by = c('flux.year', 'fence', 'plot'))
flux.tk.mean <- flux.tk[, .(nee.sum.gs = mean(nee.sum.gs, na.rm = TRUE),
                            nee.se.gs = sd(nee.sum.gs, na.rm = TRUE)/sqrt(.N),
                            gpp.sum.gs = mean(gpp.sum.gs, na.rm = TRUE),
                            gpp.se.gs = sd(gpp.sum.gs, na.rm = TRUE)/sqrt(.N),
                            reco.sum.gs = mean(reco.sum.gs, na.rm = TRUE),
                            reco.se.gs = sd(reco.sum.gs, na.rm = TRUE)/sqrt(.N),
                            nee.sum.ngs = mean(nee.sum.ngs, na.rm = TRUE),
                            nee.se.ngs = sd(nee.sum.ngs, na.rm = TRUE)/sqrt(.N),
                            gpp.sum.ngs = mean(gpp.sum.ngs, na.rm = TRUE),
                            gpp.se.ngs = sd(gpp.sum.ngs, na.rm = TRUE)/sqrt(.N),
                            reco.sum.ngs = mean(reco.sum.ngs, na.rm = TRUE),
                            reco.se.ngs = sd(reco.sum.ngs, na.rm = TRUE)/sqrt(.N),
                            nee.sum.annual = mean(nee.sum.annual, na.rm = TRUE),
                            nee.se.annual = sd(nee.sum.annual, na.rm = TRUE)/sqrt(.N),
                            gpp.sum.annual = mean(gpp.sum.annual, na.rm = TRUE),
                            gpp.se.annual = sd(gpp.sum.annual, na.rm = TRUE)/sqrt(.N),
                            reco.sum.annual = mean(reco.sum.annual, na.rm = TRUE),
                            reco.se.annual = sd(reco.sum.annual, na.rm = TRUE)/sqrt(.N),
                            wtd.mean = mean(wtd.mean),
                            wtd.se = sd(wtd.mean)/sqrt(.N),
                            alt.mean = mean(alt.annual),
                            alt.se = sd(alt.annual)/sqrt(.N),
                            tp.mean = mean(tp.annual),
                            tp.se = sd(tp.annual)/sqrt(.N)),
                        by = c('tk.class')]

flux.tk[, .N, by = c('tk.class')]
histogram(flux.tk[tk.class == 'Pre-Thaw']$nee.sum.gs)
histogram(flux.tk[tk.class == 'Non-TK']$nee.sum.gs)
histogram(flux.tk[tk.class == 'TK Edge']$nee.sum.gs)
histogram(flux.tk[tk.class == 'TK Center']$nee.sum.gs)

histogram(flux.tk[tk.class == 'Pre-Thaw']$gpp.sum.gs)
histogram(flux.tk[tk.class == 'Non-TK']$gpp.sum.gs)
histogram(flux.tk[tk.class == 'TK Edge']$gpp.sum.gs)
histogram(flux.tk[tk.class == 'TK Center']$gpp.sum.gs)

histogram(flux.tk[tk.class == 'Pre-Thaw']$reco.sum.gs)
histogram(flux.tk[tk.class == 'Non-TK']$reco.sum.gs)
histogram(flux.tk[tk.class == 'TK Edge']$reco.sum.gs)
histogram(flux.tk[tk.class == 'TK Center']$reco.sum.gs)

# will use kruskall wallis test for small sample size and perhaps non-normal distribution
# NEE
kruskal.test(nee.sum.gs ~ tk.class, data = flux.tk)
pairwise.wilcox.test(flux.tk$nee.sum, flux.tk$tk.class,
                     p.adjust.method = "BH")

# GPP
kruskal.test(gpp.sum.gs ~ tk.class, data = flux.tk)
pairwise.wilcox.test(flux.tk$gpp.sum, flux.tk$tk.class,
                     p.adjust.method = "BH")

# Reco
kruskal.test(reco.sum.gs ~ tk.class, data = flux.tk)
# Wilcoxon Rank Sum test with p-value adjustment
pairwise.wilcox.test(flux.tk$reco.sum, flux.tk$tk.class,
                     p.adjust.method = "BH")

# Plot GS differences
nee.tk.class.plot <- ggplot(flux.tk.mean, 
                            aes (x = tk.class)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_point(data = flux.tk, aes(x = tk.class, y = nee.sum.gs), 
             inherit.aes = FALSE, color = 'gray50', size = 1, alpha = 0.5) +
  geom_point(aes(y = nee.sum.gs), 
             size = 2) +
  geom_errorbar(aes(ymin = nee.sum.gs - nee.se.gs, 
                    ymax = nee.sum.gs + nee.se.gs),
                width = 0.2) +
  geom_text(aes(x = c(1, 2, 3, 4), y = rep(-150, 4), label = c('a', 'b', 'bc', 'ac')),
            inherit.aes = FALSE,
            size = 3) +
  scale_y_continuous(name = expression('GS NEE (gC' ~ m^-2*')')) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  facet_grid('NEE' ~ .)

gpp.tk.class.plot <- ggplot(flux.tk.mean, aes (x = tk.class, y = gpp.sum.gs), size = 2) +
  geom_point(data = flux.tk, aes(x = tk.class, y = gpp.sum.gs), 
             inherit.aes = FALSE, color = 'gray50', size = 1, alpha = 0.5) +
  geom_point() +
  geom_errorbar(aes(ymin = gpp.sum.gs - gpp.se.gs, ymax = gpp.sum.gs + gpp.se.gs),
                width = 0.2) +
  geom_text(aes(x = c(1, 2, 3, 4), y = rep(0, 4), label = c('a', 'b', 'c', 'b')),
            inherit.aes = FALSE,
            size = 3) +
  scale_y_continuous(name = expression('GS GPP (gC' ~ m^-2*')')) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 7.5, unit = 'pt'))) +
  facet_grid('GPP' ~ .)

reco.tk.class.plot <- ggplot(flux.tk.mean, aes (x = tk.class, y = reco.sum.gs), size = 2) +
  geom_point(data = flux.tk, aes(x = tk.class, y = reco.sum.gs), 
             inherit.aes = FALSE, color = 'gray50', size = 1, alpha = 0.5) +
  geom_point() +
  geom_errorbar(aes(ymin = reco.sum.gs - reco.se.gs, ymax = reco.sum.gs + reco.se.gs),
                width = 0.2) +
  geom_text(aes(x = seq(1, 4), y = rep(50, 4), label = c('a', 'b', 'c', 'b')),
            inherit.aes = FALSE,
            size = 3) +
  scale_y_continuous(name = expression('GS Reco (gC' ~ m^-2*')')) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 7.5, unit = 'pt'))) +
  facet_grid('Reco' ~ .)

# Plot NGS differences
nee.tk.class.plot.ngs <- ggplot(flux.tk.mean, 
                            aes (x = tk.class)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_point(data = flux.tk, aes(x = tk.class, y = nee.sum.ngs), 
             inherit.aes = FALSE, color = 'gray50', size = 1, alpha = 0.5) +
  geom_point(aes(y = nee.sum.ngs), 
             size = 2) +
  geom_errorbar(aes(ymin = nee.sum.ngs - nee.se.ngs, 
                    ymax = nee.sum.ngs + nee.se.ngs),
                width = 0.2) +
  # geom_text(aes(x = c(1, 2, 3, 4), y = rep(-150, 4), label = c('a', 'b', 'bc', 'ac')),
  #           inherit.aes = FALSE,
  #           size = 3) +
  scale_y_continuous(name = expression('NGS NEE (gC' ~ m^-2*')')) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  facet_grid('NEE' ~ .)

gpp.tk.class.plot.ngs <- ggplot(flux.tk.mean, aes (x = tk.class, y = gpp.sum.ngs), size = 2) +
  geom_point(data = flux.tk, aes(x = tk.class, y = gpp.sum.ngs), 
             inherit.aes = FALSE, color = 'gray50', size = 1, alpha = 0.5) +
  geom_point() +
  geom_errorbar(aes(ymin = gpp.sum.ngs - gpp.se.ngs, ymax = gpp.sum.ngs + gpp.se.ngs),
                width = 0.2) +
  # geom_text(aes(x = c(1, 2, 3, 4), y = rep(0, 4), label = c('a', 'b', 'c', 'b')),
  #           inherit.aes = FALSE,
  #           size = 3) +
  scale_y_continuous(name = expression('NGS GPP (gC' ~ m^-2*')')) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 7.5, unit = 'pt'))) +
  facet_grid('GPP' ~ .)

reco.tk.class.plot.ngs <- ggplot(flux.tk.mean, aes (x = tk.class, y = reco.sum.gs), size = 2) +
  geom_point(data = flux.tk, aes(x = tk.class, y = reco.sum.gs), 
             inherit.aes = FALSE, color = 'gray50', size = 1, alpha = 0.5) +
  geom_point() +
  geom_errorbar(aes(ymin = reco.sum.gs - reco.se.gs, ymax = reco.sum.gs + reco.se.gs),
                width = 0.2) +
  # geom_text(aes(x = seq(1, 4), y = rep(50, 4), label = c('a', 'b', 'c', 'b')),
  #           inherit.aes = FALSE,
  #           size = 3) +
  scale_y_continuous(name = expression('NGS Reco (gC' ~ m^-2*')')) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 7.5, unit = 'pt'))) +
  facet_grid('Reco' ~ .)

# Plot Annual differences
nee.tk.class.plot.annual <- ggplot(flux.tk.mean, 
                                aes (x = tk.class)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_point(data = flux.tk, aes(x = tk.class, y = nee.sum.annual), 
             inherit.aes = FALSE, color = 'gray50', size = 1, alpha = 0.5) +
  geom_point(aes(y = nee.sum.annual), 
             size = 2) +
  geom_errorbar(aes(ymin = nee.sum.annual - nee.se.annual, 
                    ymax = nee.sum.annual + nee.se.annual),
                width = 0.2) +
  # geom_text(aes(x = c(1, 2, 3, 4), y = rep(-150, 4), label = c('a', 'b', 'bc', 'ac')),
  #           inherit.aes = FALSE,
  #           size = 3) +
  scale_y_continuous(name = expression('Annual NEE (gC' ~ m^-2*')')) +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  facet_grid('NEE' ~ .)

# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/annual_nee_tk_class.jpg',
#        nee.tk.class.plot.annual,
#        height = 3,
#        width = 3.5,
#        bg = 'white') # As of 9/24/21, with no updates to R, R packages, or OS, this started plotting with a black background... I have no idea what might have changed
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/annual_nee_tk_class.pdf',
#        nee.tk.class.plot.annual,
#        height = 3,
#        width = 3.5)

gpp.tk.class.plot.annual <- ggplot(flux.tk.mean, aes (x = tk.class, y = gpp.sum.annual), size = 2) +
  geom_point(data = flux.tk, aes(x = tk.class, y = gpp.sum.annual), 
             inherit.aes = FALSE, color = 'gray50', size = 1, alpha = 0.5) +
  geom_point() +
  geom_errorbar(aes(ymin = gpp.sum.annual - gpp.se.annual, ymax = gpp.sum.annual + gpp.se.annual),
                width = 0.2) +
  # geom_text(aes(x = c(1, 2, 3, 4), y = rep(0, 4), label = c('a', 'b', 'c', 'b')),
  #           inherit.aes = FALSE,
  #           size = 3) +
  scale_y_continuous(name = expression('Annual GPP (gC' ~ m^-2*')')) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 7.5, unit = 'pt'))) +
  facet_grid('GPP' ~ .)

reco.tk.class.plot.annual <- ggplot(flux.tk.mean, aes (x = tk.class, y = reco.sum.gs), size = 2) +
  geom_point(data = flux.tk, aes(x = tk.class, y = reco.sum.gs), 
             inherit.aes = FALSE, color = 'gray50', size = 1, alpha = 0.5) +
  geom_point() +
  geom_errorbar(aes(ymin = reco.sum.gs - reco.se.gs, ymax = reco.sum.gs + reco.se.gs),
                width = 0.2) +
  # geom_text(aes(x = seq(1, 4), y = rep(50, 4), label = c('a', 'b', 'c', 'b')),
  #           inherit.aes = FALSE,
  #           size = 3) +
  scale_y_continuous(name = expression('Annual Reco (gC' ~ m^-2*')')) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 7.5, unit = 'pt'))) +
  facet_grid('Reco' ~ .)

# should facet plots and then have the y-axis label be for flux
tk.class.plot <- ggarrange(gpp.tk.class.plot,
                           nee.tk.class.plot,
                           reco.tk.class.plot,
                           ncol = 1,
                           heights = c(0.95, 0.95, 1))
tk.class.plot

# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/flux_tk_class.jpg',
#        tk.class.plot,
#        height = 5.5,
#        width = 3.5,
#        bg = 'white') # As of 9/24/21, with no updates to R, R packages, or OS, this started plotting with a black background... I have no idea what might have changed
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/flux_tk_class.pdf',
#        tk.class.plot,
#        height = 5.5,
#        width = 3.5)

# plot WTD
wtd.tk.class <- ggplot(flux.tk.mean, 
       aes (x = tk.class, y = wtd.mean*-1), 
       size = 2) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_point(data = flux.tk, aes(x = tk.class, y = wtd.mean*-1), 
             inherit.aes = FALSE, color = 'gray50', size = 1, alpha = 0.5) +
  geom_point() +
  geom_errorbar(aes(ymin = wtd.mean*-1 - wtd.se, ymax = wtd.mean*-1 + wtd.se),
                width = 0.2) +
  scale_y_continuous(name = 'WTD (cm)') +
  theme_bw() +
  theme(axis.title.x = element_blank())
wtd.tk.class
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/wtd_tk_class.jpg',
#        wtd.tk.class,
#        height = 3.5,
#        width = 3.5) # As of 9/24/21, with no updates to R, R packages, or OS, this started plotting with a black background... I have no idea what might have changed
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/figures/wtd_tk_class.pdf',
#        wtd.tk.class,
#        height = 3.5,
#        width = 3.5)

# plot ALT
ggplot(flux.tk.mean, aes (x = tk.class, y = alt.mean), size = 2) +
  geom_point(data = flux.tk, aes(x = tk.class, y = alt.annual), 
             inherit.aes = FALSE, color = 'gray50', size = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = alt.mean - alt.se, ymax = alt.mean + alt.se),
                width = 0.2) +
  scale_y_continuous(name = '') +
  theme_bw()
  
ggplot(flux.tk.mean, aes (x = tk.class, y = tp.mean), size = 2) +
  geom_point(data = flux.tk, aes(x = tk.class, y = tp.annual), 
             inherit.aes = FALSE, color = 'gray50', size = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = tp.mean - tp.se, ymax = tp.mean + tp.se),
                width = 0.2) +
  scale_y_continuous(name = '') +
  theme_bw()
################################################################################
