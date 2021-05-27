library(neuralnet)
library(tidyverse)
source("demand_forecast_data_source.R")

#main steps
# from: https://www.datacamp.com/community/tutorials/neural-network-models-r
# nice to read: https://datascienceplus.com/fitting-neural-network-in-r/
# 1. Create training data set
# 2. FIt neural network
# 3. Create test set
# 4. Prediction using Neural Network
# 5. Call the trained netowrk


# Create training data set
# 80% of the data will be used
training_data_percent = 0.8 #80% of the data will be used for training
test_data_percent = 1 - training_data_percent #the rest of the data is for testing
row_number = nrow(historical_data_source)
training_variables = ceiling(row_number * training_data_percent)

year         <- historical_data_source$year[1:training_variables]
gdpr         <- historical_data_source$gdpr_billion_idr[1:training_variables]
gdpr_growth  <- historical_data_source$`gdpr_growth_%`[1:training_variables]
population   <- historical_data_source$population[1:training_variables]
intensity    <- historical_data_source$intensity_biased[1:training_variables]
gwh_dist     <- historical_data_source$dist_gwh[1:training_variables]
training_dataset   <- data.frame(year,
                                 gdpr,
                                 gdpr_growth,
                                 population,
                                 intensity,
                                 gwh_dist)

# fit neural network
number_of_variables <- ncol(training_dataset)
neural_network = neuralnet(gwh_dist ~ year + gdpr + gdpr_growth + population + intensity,
                           data = training_dataset,
                           hidden = c(number_of_variables, number_of_variables - 1),
                           act.fct = "logistic",
                           linear.output = FALSE) 
plot(neural_network)


# Crete test set
year         <- historical_data_source$year[training_variables:row_number]
gdpr         <- historical_data_source$gdpr_billion_idr[training_variables:row_number]
gdpr_growth  <- historical_data_source$`gdpr_growth_%`[training_variables:row_number]
population   <- historical_data_source$population[training_variables:row_number]
intensity    <- historical_data_source$intensity_biased[training_variables:row_number]
gwh_dist     <- historical_data_source$dist_gwh[training_variables:row_number]
testing_dataset   <- data.frame(year,
                                gdpr,
                                gdpr_growth,
                                population,
                                intensity,
                                gwh_dist)

#prediction using NN
Predict = compute(neural_network, testing_dataset)
Predict$net.result



#______________________________________________________________________

#coba create training data set
TKS = c(20,10,30,20,80,30)
CSS = c(90,20,40,50,50,80)
Placed = c(1,0,0,0,1,1)
df = data.frame(TKS, CSS, Placed)

#fit neural netowrk
nn = neuralnet(Placed~TKS+CSS, data = df, hidden = 3, act.fct = "logistic", linear.output = FALSE)
plot(nn)

# create test set
TKS = c(30,40,85)
CSS = c(85,50,40)
test = data.frame(TKS, CSS)

#Prediction using neural network
Predict = compute(nn, test)
Predict$net.result

#convert probabilities into binary classes setting threshold level 0.5
prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)
pred
