library(neuralnet)
library(tidyverse)
library(ggplot2)
library(dplyr)


setwd("C:/Users/Brajamusthi/OneDrive/Master of Energy Research/Github/ArchEnSys")
training_source   <- source("demand_forecast_historical_data_source.R")
projection_source <- source("demand_forecast_prediction_data_source.R")

# select which province / area to predict
province_to_predict = "INDONESIA"


# Prepare training dataset ------------------------------------------------
  # open historical data
  # use everything for training
  nn_train_dataset <- historical_data_source[historical_data_source$Province == province_to_predict,]

  # create Max-min scaling
  max_dist_gwh   <- max(nn_train_dataset$dist_gwh)
  min_dist_gwh   <- min(nn_train_dataset$dist_gwh)
  scale_dist_gwh <- max_dist_gwh - min_dist_gwh
  
  # max-min normalization
  normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
  }
  
  maxmindf_train  <- as.data.frame(sapply(nn_train_dataset[,c(3:12),], normalize))
  #    ori_value <- as.data.frame(t((t(maxmindf_train)*scale_val) + min_val))
  
  # Create training & testing data set
  # 100% of the data will be used
  train_obs_size = ceiling(nrow(maxmindf_train) * 1)
  test_obs_size  = nrow(maxmindf_train) - train_obs_size
  
  train_dataset   <- maxmindf_train[1:train_obs_size, ]
  test_dataset    <- maxmindf_train[(train_obs_size+1):nrow(maxmindf_train),]


# Neural Network Training -------------------------------------------------
  #fit neural network
  nn <- neuralnet(dist_gwh ~ year + gdpr_billion_idr + gdpr_growth + population + kwh_dem_percap + intensity_biased,
                  data     = train_dataset,
                  hidden   = c(ncol(train_dataset), ncol(train_dataset), ncol(train_dataset)),
                  stepmax  = 1e+5,
                  act.fct  = "logistic",
                  linear.output = TRUE)
  
  plot(nn, rep      = "best",
       arrow.length = 0.15,
       col.entry    ="blue",
       col.hidden   ="red",
       col.out      ="blue",
       show.weights = TRUE,
       information  = TRUE,
       fontsize     = 10)
  
  #try to create prediction by fitting trainned neural network to training data
  
  Predict_2 = neuralnet::compute(nn, train_dataset)
  predict_value_2  <- (Predict_2$net.result * scale_dist_gwh) + min_dist_gwh
  actual_value_2   <- (train_dataset$dist_gwh * scale_dist_gwh) + min_dist_gwh
  error_percent_2  <- ((predict_value_2 - actual_value_2)/actual_value_2)*100
  results_denorm_2 <- data.frame(actual_value_2, predict_value_2, error_percent_2)
  average_error_2  = mean(error_percent_2)
  average_error_2
  
# Prepare projection dataset ----------------------------------------------
  nn_proj_dataset <- projection_dataset[projection_dataset$Province == province_to_predict,]
  
  # Call max min scaling function for projection dataset
  maxmindf_proj  <- as.data.frame(sapply(nn_proj_dataset[,c(2:7),], normalize))
  proj_dataset   <- maxmindf_proj

# Create projection -------------------------------------------------------
  Predict_1        = neuralnet::compute(nn, proj_dataset)
  predict_value_1  <- (Predict_1$net.result  * scale_dist_gwh) + min_dist_gwh
  actual_value_1   <- nn_proj_dataset$electricity_demand_twh * 1000
  error_percent_1  <- ((predict_value_1 - actual_value_1)/actual_value_1)*100
  results_denorm_1 <- data.frame(nn_proj_dataset$year, actual_value_1, predict_value_1, error_percent_1)
  average_error_1  = mean(error_percent_1)
  average_error_1
  
  
  #write_csv(results_denorm_1, "results.csv")
  