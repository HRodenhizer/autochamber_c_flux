#############################################################################################################################
###                              Model Growing Season CO2 Fluxes using a Regression Tree                                  ###
###                                                code by HGR 2/2020                                                     ###
#############################################################################################################################

### Load Libraries ##########################################################################################################
library(gbm)
library(tidyverse)
#############################################################################################################################

### Load Data ###############################################################################################################
flux.daily <- read.csv("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/flux_daily_neat.csv")
flux.weekly <- read.csv("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/flux_weekly_neat.csv")
flux.annual <- read.csv("/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/flux_annual.csv")
#############################################################################################################################

### Gradient Boosted Regression Tree ########################################################################################
set.seed(21591)
train.weekly <- sample(1:nrow(flux.daily), 20000)
train.annual <- sample(1:nrow(flux.daily), 1100)

nee.daily <- flux.daily[!is.na(nee.sum), .SD, .SDcols = c(13, seq(16, 50))]
gpp.daily <- flux.daily[!is.na(gpp.sum), .SD, .SDcols = c(15, seq(16, 50))]
reco.daily <- flux.daily[!is.na(reco.sum), .SD, .SDcols = c(14, seq(16, 50))]
train.nee.daily <- sample(1:nrow(nee.daily), 55000)
train.gpp.daily <- sample(1:nrow(gpp.daily), 55000)
train.reco.daily <- sample(1:nrow(reco.daily), 55000)

nee.weekly <- flux.weekly[, .SD, .SDcols = c(12, seq(15, 135))][train.weekly,]
gpp.weekly <- flux.weekly[, .SD, .SDcols = c(14, seq(15, 135))][train.weekly,]
reco.weekly <- flux.weekly[, .SD, .SDcols = c(13, seq(15, 135))][train.weekly,]

nee.annual <- flux.annual[, .SD, .SDcols = c(8, seq(11, 93))][train.annual,]
gpp.annual <- flux.annual[, .SD, .SDcols = c(10, seq(11, 93))][train.annual,]
reco.annual <- flux.annual[, .SD, .SDcols = c(9, seq(11, 93))][train.annual,]

### Test out regression tree for stability (it would be nice to see the tree)
library(tree)
for (i in 1:20) {
  set.seed(i)
  train <- sample(1:nrow(nee.daily))
  print(train[1:10])
  test <- tree(nee.sum ~ .,
               nee.daily,
               subset = train)
  plot(test)
  text(test)
}

### Daily
### Can switch to using a regression tree!
# NEE 
nee.daily.gbm <- gbm(nee.sum~.,
                     data = nee.daily[train.nee.daily,],
                     distribution = "gaussian",
                     n.trees = 10000,
                     shrinkage = 0.01,
                     interaction.depth = 4)
summary(nee.daily.gbm)
nee.pred <- nee.daily[-train.nee.daily,
                      nee.pred := predict(nee.daily.gbm,
                                          newdata = nee.daily[-train.nee.daily,],
                                          n.trees = 10000,
                                          single.tree = TRUE)]
ggplot(nee.pred, aes(x = nee.sum, y = nee.pred)) +
  geom_point()
plot(nee.daily.gbm, i = 't5.mean')
pretty.gbm.tree(nee.daily.gbm, i.tree = 10000)
# saveRDS(nee.daily.gbm, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/nee_daily_gbm.rds')

# Reco
reco.daily.gbm <- gbm(reco.sum~.,
                     data = reco.daily[train.reco.daily,],
                     distribution = "gaussian",
                     n.trees = 10000,
                     shrinkage = 0.01,
                     interaction.depth = 4)
summary(reco.daily.gbm)
reco.pred <- reco.daily[-train.reco.daily,
                      reco.pred := predict(reco.daily.gbm,
                                          newdata = reco.daily[-train.reco.daily,],
                                          n.trees = 10000,
                                          single.tree = TRUE)]
ggplot(reco.pred, aes(x = reco.sum, y = reco.pred)) +
  geom_point()
plot(reco.daily.gbm, i = 't10.min')
pretty.gbm.tree(reco.daily.gbm, i.tree = 10000)
# saveRDS(reco.daily.gbm, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/reco_daily_gbm.rds')

# GPP
gpp.daily.gbm <- gbm(gpp.sum~.,
                      data = gpp.daily[train.gpp.daily,],
                      distribution = "gaussian",
                      n.trees = 10000,
                      shrinkage = 0.01,
                      interaction.depth = 4)
summary(gpp.daily.gbm)
gpp.pred <- gpp.daily[-train.gpp.daily,
                        gpp.pred := predict(gpp.daily.gbm,
                                             newdata = gpp.daily[-train.gpp.daily,],
                                             n.trees = 10000,
                                             single.tree = TRUE)]
ggplot(gpp.pred, aes(x = gpp.sum, y = gpp.pred)) +
  geom_point()
plot(gpp.daily.gbm, i = 't10.min')
pretty.gbm.tree(gpp.daily.gbm, i.tree = 10000)
# saveRDS(gpp.daily.gbm, '/home/heidi/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux/model_output/gpp_daily_gbm.rds')
