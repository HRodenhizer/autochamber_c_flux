#############################################################################################################################
###                              Model Growing Season CO2 Fluxes using a Regression Tree                                  ###
###                                                code by HGR 2/2020                                                     ###
#############################################################################################################################

### Load Libraries ##########################################################################################################
library(gbm)
library(tidyverse)
#############################################################################################################################

### Load Data ###############################################################################################################
compiled_data <- read.csv("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Autochamber/autochamber_c_flux_input_data/annual_environmental_data_compiled.csv") %>%
  filter(!is.na(NEE.sum) & !is.na(Reco.sum) & !is.na(GPP.sum))
#############################################################################################################################

### Gradient Boosted Regression Tree ########################################################################################
set.seed(21591)
train <- sample(1:nrow(compiled_data), 300)

# NEE
nee <- compiled_data %>%
  select(-c(seq(1,6), 8, 9))
nee.gbm <- gbm(NEE.sum~., data = nee[train,], distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
summary(nee.gbm)
nee.pred <- compiled_data %>%
  slice(-1*train) %>%
  select(c(seq(1,9))) %>%
  mutate(nee.pred = predict(nee.gbm, newdata = nee[-train,], n.trees = 10000, single.tree = TRUE))
ggplot(nee.pred, aes(x = NEE.sum, y = nee.pred)) +
  geom_point()
plot(nee.gbm, i = 'biomass.filled')
pretty(nee.gbm)

# Reco
reco <- compiled_data %>%
  select(-c(seq(1,7), 9))
reco.gbm <- gbm(Reco.sum~., data = reco[train,], distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
summary(reco.gbm)

# GPP
gpp <- compiled_data %>%
  select(-c(seq(1,8)))
gpp.gbm <- gbm(GPP.sum~., data = gpp[train,], distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
summary(gpp.gbm)
