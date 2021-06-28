library(neuralnet)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(normalr)
library(gradDescent)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to this script's location
source("demand_forecast_historical_data_source.R")

# max-min normalization
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# percent of the data will be used
percent_data   = 1


# INDONESIA ---------------------------------------------------------------

            ind_dataset     <- his_dat[his_dat$region =="INDONESIA",]
            
            # call min and max value for each column
            ind_max_dist_gwh <- max(ind_dataset$dist_gwh)
            ind_min_dist_gwh <- min(ind_dataset$dist_gwh)
            ind_scale_dist_gwh <- ind_max_dist_gwh - ind_min_dist_gwh
            ind_maxmindf  <- as.data.frame(sapply(ind_dataset[,c(2:11),], normalize))
            
            # Create training & testing data set
            ind_train_dataset   <- ind_maxmindf[1:(ceiling(nrow(ind_maxmindf) * percent_data)), ]
            ind_test_dataset    <- ind_maxmindf[((ceiling(nrow(ind_maxmindf) * percent_data))+1):nrow(ind_maxmindf),]

            
            # fit neural network
            ind_nn <- neuralnet(dist_gwh ~ year + gdpr_billion_idr + gdpr_growth + population + kwh_dem_percap + intensity_biased,
                            data = ind_train_dataset,
                            hidden = c(ncol(ind_train_dataset), ncol(ind_train_dataset), ncol(ind_train_dataset)),
                            stepmax = 1e+5,
                            act.fct = "logistic",
                            linear.output = TRUE)
            plot(ind_nn, rep = "best",
                 arrow.length = 0.15,
                 col.entry  ="blue",
                 col.hidden ="red",
                 col.out    ="blue",
                 show.weights = TRUE,
                 information  = TRUE,
                 fontsize = 10)
            
            #test the model to the training data
            ind_predict = neuralnet::compute(ind_nn, ind_train_dataset)
            ind_predict_value  <- (ind_predict$net.result * ind_scale_dist_gwh) + ind_min_dist_gwh
            ind_ref_value   <- (ind_train_dataset$dist_gwh * ind_scale_dist_gwh) + ind_min_dist_gwh
            ind_error_percent  <- ((ind_predict_value - ind_ref_value)/ind_ref_value) * 100
            ind_results_denorm <- data.frame(ind_ref_value, ind_predict_value, ind_error_percent)
            ind_results_denorm$region = "INDONESIA"
            ind_results_denorm$year   = ind_dataset$year
            setnames(ind_results_denorm, old = colnames(ind_results_denorm), new = c('ref_value',
                                                                                           'predict_value',
                                                                                           'error_percent',
                                                                                           'region',
                                                                                           'year'))
            
            #write.table(ind_results_denorm, "clipboard", sep="\t", row.names=FALSE) # copy to clipboard
            

# Sumatera ----------------------------------------------------------------

            suma_dataset      <- his_dat[his_dat$region =="SUMATERA",]
            
            # call min and max value for each column
            suma_max_dist_gwh <- max(suma_dataset$dist_gwh)
            suma_min_dist_gwh <- min(suma_dataset$dist_gwh)
            suma_scale_dist_gwh <- suma_max_dist_gwh - suma_min_dist_gwh
            suma_maxmindf  <- as.data.frame(sapply(suma_dataset[,c(2:11),], normalize))
            
            # Create training & testing data set
            suma_train_dataset   <- suma_maxmindf[1:(ceiling(nrow(suma_maxmindf) * percent_data)), ]
            suma_test_dataset    <- suma_maxmindf[((ceiling(nrow(suma_maxmindf) * percent_data))+1):nrow(suma_maxmindf),]
            
            
            # fit neural network
            suma_nn <- neuralnet(dist_gwh ~ year + gdpr_billion_idr + gdpr_growth + population + kwh_dem_percap + intensity_biased,
                                data = suma_train_dataset,
                                hidden = c(ncol(suma_train_dataset), ncol(suma_train_dataset), ncol(suma_train_dataset)),
                                stepmax = 1e+5,
                                act.fct = "logistic",
                                linear.output = TRUE)
            # plot(suma_nn, rep = "best",
            #      arrow.length = 0.15,
            #      col.entry  ="blue",
            #      col.hidden ="red",
            #      col.out    ="blue",
            #      show.weights = TRUE,
            #      information  = TRUE,
            #      fontsize = 10)
            
            #test the model to the training data
            suma_predict = neuralnet::compute(suma_nn, suma_train_dataset)
            suma_predict_value  <- (suma_predict$net.result * suma_scale_dist_gwh) + suma_min_dist_gwh
            suma_ref_value   <- (suma_train_dataset$dist_gwh * suma_scale_dist_gwh) + suma_min_dist_gwh
            suma_error_percent  <- ((suma_predict_value - suma_ref_value)/suma_ref_value) * 100
            suma_results_denorm <- data.frame(suma_ref_value, suma_predict_value, suma_error_percent)
            suma_results_denorm$region = "SUMATERA"
            suma_results_denorm$year   = suma_dataset$year
            setnames(suma_results_denorm, old = colnames(suma_results_denorm), new = c('ref_value',
                                                                                           'predict_value',
                                                                                           'error_percent',
                                                                                           'region',
                                                                                           'year'))
            
            
            
# JAWA ---------------------------------------------------------------
            
            jawa_dataset     <- his_dat[his_dat$region =="JAWA",]
            
            # call min and max value for each column
            jawa_max_dist_gwh <- max(jawa_dataset$dist_gwh)
            jawa_min_dist_gwh <- min(jawa_dataset$dist_gwh)
            jawa_scale_dist_gwh <- jawa_max_dist_gwh - jawa_min_dist_gwh
            jawa_maxmindf  <- as.data.frame(sapply(jawa_dataset[,c(2:11),], normalize))
            
            # Create training & testing data set
            jawa_train_dataset   <- jawa_maxmindf[1:(ceiling(nrow(jawa_maxmindf) * percent_data)), ]
            jawa_test_dataset    <- jawa_maxmindf[((ceiling(nrow(jawa_maxmindf) * percent_data))+1):nrow(jawa_maxmindf),]
            
            
            # fit neural network
            jawa_nn <- neuralnet(dist_gwh ~ year + gdpr_billion_idr + gdpr_growth + population + kwh_dem_percap + intensity_biased,
                                data = jawa_train_dataset,
                                hidden = c(ncol(jawa_train_dataset), ncol(jawa_train_dataset), ncol(jawa_train_dataset)),
                                stepmax = 1e+5,
                                act.fct = "logistic",
                                linear.output = TRUE)
            # plot(jawa_nn, rep = "best",
            #      arrow.length = 0.15,
            #      col.entry  ="blue",
            #      col.hidden ="red",
            #      col.out    ="blue",
            #      show.weights = TRUE,
            #      information  = TRUE,
            #      fontsize = 10)
            
            #test the model to the training data
            jawa_predict = neuralnet::compute(jawa_nn, jawa_train_dataset)
            jawa_predict_value  <- (jawa_predict$net.result * jawa_scale_dist_gwh) + jawa_min_dist_gwh
            jawa_ref_value   <- (jawa_train_dataset$dist_gwh * jawa_scale_dist_gwh) + jawa_min_dist_gwh
            jawa_error_percent  <- ((jawa_predict_value - jawa_ref_value)/jawa_ref_value) * 100
            jawa_results_denorm <- data.frame(jawa_ref_value, jawa_predict_value, jawa_error_percent)
            jawa_results_denorm$region = "JAWA"
            jawa_results_denorm$year   = jawa_dataset$year
            setnames(jawa_results_denorm, old = colnames(jawa_results_denorm), new = c('ref_value',
                                                                                           'predict_value',
                                                                                           'error_percent',
                                                                                           'region',
                                                                                           'year'))
            
            
# BALI & NUSA TENGGARA ---------------------------------------------------------------
            
            bali_dataset     <- his_dat[his_dat$region =="BALI_NT",]
            
            # call min and max value for each column
            bali_max_dist_gwh <- max(bali_dataset$dist_gwh)
            bali_min_dist_gwh <- min(bali_dataset$dist_gwh)
            bali_scale_dist_gwh <- bali_max_dist_gwh - bali_min_dist_gwh
            bali_maxmindf  <- as.data.frame(sapply(bali_dataset[,c(2:11),], normalize))
            
            # Create training & testing data set
            bali_train_dataset   <- bali_maxmindf[1:(ceiling(nrow(bali_maxmindf) * percent_data)), ]
            bali_test_dataset    <- bali_maxmindf[((ceiling(nrow(bali_maxmindf) * percent_data))+1):nrow(bali_maxmindf),]
            
            
            # fit neural network
            bali_nn <- neuralnet(dist_gwh ~ year + gdpr_billion_idr + gdpr_growth + population + kwh_dem_percap + intensity_biased,
                                data = bali_train_dataset,
                                hidden = c(ncol(bali_train_dataset), ncol(bali_train_dataset), ncol(bali_train_dataset)),
                                stepmax = 1e+5,
                                act.fct = "logistic",
                                linear.output = TRUE)
            # plot(bali_nn, rep = "best",
            #      arrow.length = 0.15,
            #      col.entry  ="blue",
            #      col.hidden ="red",
            #      col.out    ="blue",
            #      show.weights = TRUE,
            #      information  = TRUE,
            #      fontsize = 10)
            
            #test the model to the training data
            bali_predict = neuralnet::compute(bali_nn, bali_train_dataset)
            bali_predict_value  <- (bali_predict$net.result * bali_scale_dist_gwh) + bali_min_dist_gwh
            bali_ref_value   <- (bali_train_dataset$dist_gwh * bali_scale_dist_gwh) + bali_min_dist_gwh
            bali_error_percent  <- ((bali_predict_value - bali_ref_value)/bali_ref_value) * 100
            bali_results_denorm <- data.frame(bali_ref_value, bali_predict_value, bali_error_percent)
            bali_results_denorm$region = "BALI_NT"
            bali_results_denorm$year   = bali_dataset$year
            setnames(bali_results_denorm, old = colnames(bali_results_denorm), new = c('ref_value',
                                                                                           'predict_value',
                                                                                           'error_percent',
                                                                                           'region',
                                                                                           'year'))
            

            
# KALIMANTAN ---------------------------------------------------------------
            
            kali_dataset     <- his_dat[his_dat$region =="KALIMANTAN",]
            
            # call min and max value for each column
            kali_max_dist_gwh <- max(kali_dataset$dist_gwh)
            kali_min_dist_gwh <- min(kali_dataset$dist_gwh)
            kali_scale_dist_gwh <- kali_max_dist_gwh - kali_min_dist_gwh
            kali_maxmindf  <- as.data.frame(sapply(kali_dataset[,c(2:11),], normalize))
            
            # Create training & testing data set
            kali_train_dataset   <- kali_maxmindf[1:(ceiling(nrow(kali_maxmindf) * percent_data)), ]
            kali_test_dataset    <- kali_maxmindf[((ceiling(nrow(kali_maxmindf) * percent_data))+1):nrow(kali_maxmindf),]
            
            
            # fit neural network
            kali_nn <- neuralnet(dist_gwh ~ year + gdpr_billion_idr + gdpr_growth + population + kwh_dem_percap + intensity_biased,
                                data = kali_train_dataset,
                                hidden = c(ncol(kali_train_dataset), ncol(kali_train_dataset), ncol(kali_train_dataset)),
                                stepmax = 1e+5,
                                act.fct = "logistic",
                                linear.output = TRUE)
            # plot(kali_nn, rep = "best",
            #      arrow.length = 0.15,
            #      col.entry  ="blue",
            #      col.hidden ="red",
            #      col.out    ="blue",
            #      show.weights = TRUE,
            #      information  = TRUE,
            #      fontsize = 10)
            
            #test the model to the training data
            kali_predict = neuralnet::compute(kali_nn, kali_train_dataset)
            kali_predict_value  <- (kali_predict$net.result * kali_scale_dist_gwh) + kali_min_dist_gwh
            kali_ref_value   <- (kali_train_dataset$dist_gwh * kali_scale_dist_gwh) + kali_min_dist_gwh
            kali_error_percent  <- ((kali_predict_value - kali_ref_value)/kali_ref_value) * 100
            kali_results_denorm <- data.frame(kali_ref_value, kali_predict_value, kali_error_percent)
            kali_results_denorm$region = "KALIMANTAN"
            kali_results_denorm$year   = kali_dataset$year
            setnames(kali_results_denorm, old = colnames(kali_results_denorm), new = c('ref_value',
                                                                                           'predict_value',
                                                                                           'error_percent',
                                                                                           'region',
                                                                                           'year'))
            
            
# SULAWESI ---------------------------------------------------------------
            
            sula_dataset     <- his_dat[his_dat$region =="SULAWESI",]
            
            # call min and max value for each column
            sula_max_dist_gwh <- max(sula_dataset$dist_gwh)
            sula_min_dist_gwh <- min(sula_dataset$dist_gwh)
            sula_scale_dist_gwh <- sula_max_dist_gwh - sula_min_dist_gwh
            sula_maxmindf  <- as.data.frame(sapply(sula_dataset[,c(2:11),], normalize))
            
            # Create training & testing data set
            sula_train_dataset   <- sula_maxmindf[1:(ceiling(nrow(sula_maxmindf) * percent_data)), ]
            sula_test_dataset    <- sula_maxmindf[((ceiling(nrow(sula_maxmindf) * percent_data))+1):nrow(sula_maxmindf),]
            
            
            # fit neural network
            sula_nn <- neuralnet(dist_gwh ~ year + gdpr_billion_idr + gdpr_growth + population + kwh_dem_percap + intensity_biased,
                                data = sula_train_dataset,
                                hidden = c(ncol(sula_train_dataset), ncol(sula_train_dataset), ncol(sula_train_dataset)),
                                stepmax = 1e+5,
                                act.fct = "logistic",
                                linear.output = TRUE)
            # plot(sula_nn, rep = "best",
            #      arrow.length = 0.15,
            #      col.entry  ="blue",
            #      col.hidden ="red",
            #      col.out    ="blue",
            #      show.weights = TRUE,
            #      information  = TRUE,
            #      fontsize = 10)
            
            #test the model to the training data
            sula_predict = neuralnet::compute(sula_nn, sula_train_dataset)
            sula_predict_value  <- (sula_predict$net.result * sula_scale_dist_gwh) + sula_min_dist_gwh
            sula_ref_value   <- (sula_train_dataset$dist_gwh * sula_scale_dist_gwh) + sula_min_dist_gwh
            sula_error_percent  <- ((sula_predict_value - sula_ref_value)/sula_ref_value) * 100
            sula_results_denorm <- data.frame(sula_ref_value, sula_predict_value, sula_error_percent)
            sula_results_denorm$region = "SULAWESI"
            sula_results_denorm$year   = sula_dataset$year
            setnames(sula_results_denorm, old = colnames(sula_results_denorm), new = c('ref_value',
                                                                                           'predict_value',
                                                                                           'error_percent',
                                                                                           'region',
                                                                                           'year'))
            
            
# MALUKU & PAPUA ---------------------------------------------------------------
            
            malupa_dataset     <- his_dat[his_dat$region =="MALUKU_PAPUA",]
            
            # call min and max value for each column
            malupa_max_dist_gwh <- max(malupa_dataset$dist_gwh)
            malupa_min_dist_gwh <- min(malupa_dataset$dist_gwh)
            malupa_scale_dist_gwh <- malupa_max_dist_gwh - malupa_min_dist_gwh
            malupa_maxmindf  <- as.data.frame(sapply(malupa_dataset[,c(2:11),], normalize))
            
            # Create training & testing data set
            malupa_train_dataset   <- malupa_maxmindf[1:(ceiling(nrow(malupa_maxmindf) * percent_data)), ]
            malupa_test_dataset    <- malupa_maxmindf[((ceiling(nrow(malupa_maxmindf) * percent_data))+1):nrow(malupa_maxmindf),]
            
            
            # fit neural network
            malupa_nn <- neuralnet(dist_gwh ~ year + gdpr_billion_idr + gdpr_growth + population + kwh_dem_percap + intensity_biased,
                                data = malupa_train_dataset,
                                hidden = c(ncol(malupa_train_dataset), ncol(malupa_train_dataset), ncol(malupa_train_dataset)),
                                stepmax = 1e+5,
                                act.fct = "logistic",
                                linear.output = TRUE)
            # plot(malupa_nn, rep = "best",
            #      arrow.length = 0.15,
            #      col.entry  ="blue",
            #      col.hidden ="red",
            #      col.out    ="blue",
            #      show.weights = TRUE,
            #      information  = TRUE,
            #      fontsize = 10)
            
            #test the model to the training data
            malupa_predict = neuralnet::compute(malupa_nn, malupa_train_dataset)
            malupa_predict_value  <- (malupa_predict$net.result * malupa_scale_dist_gwh) + malupa_min_dist_gwh
            malupa_ref_value   <- (malupa_train_dataset$dist_gwh * malupa_scale_dist_gwh) + malupa_min_dist_gwh
            malupa_error_percent  <- ((malupa_predict_value - malupa_ref_value)/malupa_ref_value) * 100
            malupa_results_denorm <- data.frame(malupa_ref_value, 
                                                malupa_predict_value, 
                                                malupa_error_percent)
            malupa_results_denorm$region = "MALUKU_PAPUA"
            malupa_results_denorm$year   = malupa_dataset$year
            setnames(malupa_results_denorm, old = colnames(malupa_results_denorm), new = c('ref_value',
                                                                                           'predict_value',
                                                                                           'error_percent',
                                                                                           'region',
                                                                                           'year'))


# bind all training results -----------------------------------------------

            nn_train_results <- rbind(ind_results_denorm,
                                      suma_results_denorm,
                                      jawa_results_denorm,
                                      bali_results_denorm,
                                      kali_results_denorm,
                                      sula_results_denorm,
                                      malupa_results_denorm)

            
# ******************************************************************************
# prediction
# ******************************************************************************
source("demand_forecast_prediction_data_source.R")


# INDONESIA - Projection --------------------------------------------------
          ind_proj_dataset     <- proj_dat[proj_dat$region =="INDONESIA",]
          
          # call min and max value for each column
          ind_proj_maxmindf  <- as.data.frame(sapply(ind_proj_dataset[,c(2:8),], normalize))
          
          #test the model to the training data
          ind_proj_predict = neuralnet::compute(ind_nn, ind_proj_maxmindf)
          ind_proj_predict_value  <- (ind_proj_predict$net.result * ind_scale_dist_gwh) + ind_max_dist_gwh
          
          ind_proj_ref_value      <- ind_proj_dataset$electricity_demand_gwh
          ind_proj_error_percent  <- ((ind_proj_predict_value - ind_proj_ref_value)/ind_proj_ref_value) * 100
          ind_proj_results_denorm <- data.frame(ind_proj_ref_value, ind_proj_predict_value, ind_proj_error_percent)
          ind_proj_results_denorm$region = "INDONESIA"
          ind_proj_results_denorm$year   = ind_proj_dataset$year
          setnames(ind_proj_results_denorm, old = colnames(ind_proj_results_denorm), new = c('ref_value',
                                                                                             'predict_value',
                                                                                             'error_percent',
                                                                                             'region',
                                                                                             'year'))
          
          
# SUMATERA - Projection --------------------------------------------------
          suma_proj_dataset     <- proj_dat[proj_dat$region =="SUMATERA",]
          
          # call min and max value for each column
          suma_proj_maxmindf  <- as.data.frame(sapply(suma_proj_dataset[,c(2:8),], normalize))
          
          #test the model to the training data
          suma_proj_predict = neuralnet::compute(suma_nn, suma_proj_maxmindf)
          suma_proj_predict_value  <- (suma_proj_predict$net.result * suma_scale_dist_gwh) + suma_max_dist_gwh
          suma_proj_ref_value      <- suma_proj_dataset$electricity_demand_gwh
          suma_proj_error_percent  <- ((suma_proj_predict_value - suma_proj_ref_value)/suma_proj_ref_value) * 100
          suma_proj_results_denorm <- data.frame(suma_proj_ref_value, suma_proj_predict_value, suma_proj_error_percent)
          suma_proj_results_denorm$region = "SUMATERA"
          suma_proj_results_denorm$year   = suma_proj_dataset$year
          setnames(suma_proj_results_denorm, old = colnames(suma_proj_results_denorm), new = c('ref_value',
                                                                                             'predict_value',
                                                                                             'error_percent',
                                                                                             'region',
                                                                                             'year'))
          
# JAWA - Projection --------------------------------------------------
          jawa_proj_dataset     <- proj_dat[proj_dat$region =="JAWA",]
          
          # call min and max value for each column
          jawa_proj_maxmindf  <- as.data.frame(sapply(jawa_proj_dataset[,c(2:8),], normalize))
          
          #test the model to the training data
          jawa_proj_predict = neuralnet::compute(jawa_nn, jawa_proj_maxmindf)
          jawa_proj_predict_value  <- (jawa_proj_predict$net.result * jawa_scale_dist_gwh) + jawa_max_dist_gwh
          jawa_proj_ref_value      <- jawa_proj_dataset$electricity_demand_gwh
          jawa_proj_error_percent  <- ((jawa_proj_predict_value - jawa_proj_ref_value)/jawa_proj_ref_value) * 100
          jawa_proj_results_denorm <- data.frame(jawa_proj_ref_value, jawa_proj_predict_value, jawa_proj_error_percent)
          jawa_proj_results_denorm$region = "JAWA"
          jawa_proj_results_denorm$year   = jawa_proj_dataset$year
          setnames(jawa_proj_results_denorm, old = colnames(jawa_proj_results_denorm), new = c('ref_value',
                                                                                             'predict_value',
                                                                                             'error_percent',
                                                                                             'region',
                                                                                             'year'))
          
# BALI_NT - Projection --------------------------------------------------
          bali_proj_dataset     <- proj_dat[proj_dat$region =="BALI_NT",]
          
          # call min and max value for each column
          bali_proj_maxmindf  <- as.data.frame(sapply(bali_proj_dataset[,c(2:8),], normalize))
          
          #test the model to the training data
          bali_proj_predict = neuralnet::compute(bali_nn, bali_proj_maxmindf)
          bali_proj_predict_value  <- (bali_proj_predict$net.result * bali_scale_dist_gwh) + bali_max_dist_gwh
          bali_proj_ref_value      <- bali_proj_dataset$electricity_demand_gwh
          bali_proj_error_percent  <- ((bali_proj_predict_value - bali_proj_ref_value)/bali_proj_ref_value) * 100
          bali_proj_results_denorm <- data.frame(bali_proj_ref_value, bali_proj_predict_value, bali_proj_error_percent)
          bali_proj_results_denorm$region = "BALI_NT"
          bali_proj_results_denorm$year   = bali_proj_dataset$year
          setnames(bali_proj_results_denorm, old = colnames(bali_proj_results_denorm), new = c('ref_value',
                                                                                             'predict_value',
                                                                                             'error_percent',
                                                                                             'region',
                                                                                             'year'))
          
# KALIMANTAN - Projection --------------------------------------------------
          kali_proj_dataset     <- proj_dat[proj_dat$region =="KALIMANTAN",]
          
          # call min and max value for each column
          kali_proj_maxmindf  <- as.data.frame(sapply(kali_proj_dataset[,c(2:8),], normalize))
          
          #test the model to the training data
          kali_proj_predict = neuralnet::compute(kali_nn, kali_proj_maxmindf)
          kali_proj_predict_value  <- (kali_proj_predict$net.result * kali_scale_dist_gwh) + kali_max_dist_gwh
          kali_proj_ref_value      <- kali_proj_dataset$electricity_demand_gwh
          kali_proj_error_percent  <- ((kali_proj_predict_value - kali_proj_ref_value)/kali_proj_ref_value) * 100
          kali_proj_results_denorm <- data.frame(kali_proj_ref_value, kali_proj_predict_value, kali_proj_error_percent)
          kali_proj_results_denorm$region = "KALIMANTAN"
          kali_proj_results_denorm$year   = kali_proj_dataset$year
          setnames(kali_proj_results_denorm, old = colnames(kali_proj_results_denorm), new = c('ref_value',
                                                                                             'predict_value',
                                                                                             'error_percent',
                                                                                             'region',
                                                                                             'year'))

# SULAWESI - Projection --------------------------------------------------
          sula_proj_dataset     <- proj_dat[proj_dat$region =="SULAWESI",]
          
          # call min and max value for each column
          sula_proj_maxmindf  <- as.data.frame(sapply(sula_proj_dataset[,c(2:8),], normalize))
          
          #test the model to the training data
          sula_proj_predict = neuralnet::compute(sula_nn, sula_proj_maxmindf)
          sula_proj_predict_value  <- (sula_proj_predict$net.result * sula_scale_dist_gwh) + sula_max_dist_gwh
          sula_proj_ref_value      <- sula_proj_dataset$electricity_demand_gwh
          sula_proj_error_percent  <- ((sula_proj_predict_value - sula_proj_ref_value)/sula_proj_ref_value) * 100
          sula_proj_results_denorm <- data.frame(sula_proj_ref_value, sula_proj_predict_value, sula_proj_error_percent)
          sula_proj_results_denorm$region = "SULAWESI"
          sula_proj_results_denorm$year   = sula_proj_dataset$year
          setnames(sula_proj_results_denorm, old = colnames(sula_proj_results_denorm), new = c('ref_value',
                                                                                             'predict_value',
                                                                                             'error_percent',
                                                                                             'region',
                                                                                             'year'))
          
# MALUKU_PAPUA - Projection --------------------------------------------------
          malupa_proj_dataset     <- proj_dat[proj_dat$region =="MALUKU_PAPUA",]
          
          # call min and max value for each column
          malupa_proj_maxmindf  <- as.data.frame(sapply(malupa_proj_dataset[,c(2:8),], normalize))
          
          #test the model to the training data
          malupa_proj_predict = neuralnet::compute(malupa_nn, malupa_proj_maxmindf)
          malupa_proj_predict_value  <- (malupa_proj_predict$net.result * malupa_scale_dist_gwh) + malupa_max_dist_gwh
          malupa_proj_ref_value      <- malupa_proj_dataset$electricity_demand_gwh
          malupa_proj_error_percent  <- ((malupa_proj_predict_value - malupa_proj_ref_value)/malupa_proj_ref_value) * 100
          malupa_proj_results_denorm <- data.frame(malupa_proj_ref_value, malupa_proj_predict_value, malupa_proj_error_percent)
          malupa_proj_results_denorm$region = "MALUKU_PAPUA"
          malupa_proj_results_denorm$year   = malupa_proj_dataset$year
          setnames(malupa_proj_results_denorm, old = colnames(malupa_proj_results_denorm), new = c('ref_value',
                                                                                             'predict_value',
                                                                                             'error_percent',
                                                                                             'region',
                                                                                             'year'))
          
# bind all projection results -----------------------------------------------
          
          nn_proj_results <- rbind(ind_proj_results_denorm,
                                   suma_proj_results_denorm,
                                   jawa_proj_results_denorm,
                                   bali_proj_results_denorm,
                                   kali_proj_results_denorm,
                                   sula_proj_results_denorm,
                                   malupa_proj_results_denorm)
          # view(nn_proj_results)

# bind training + results
          nn_results <- rbind(nn_train_results,
                              nn_proj_results)
          
          # view(nn_results)
# bind all data
          his_dat_df <- his_dat[c('region',
                                  'year',
                                  'gdpr_growth',
                                  'intensity_biased',
                                  'kwh_dem_percap',
                                  'gdpr_billion_idr',
                                  'population',
                                  'gdpr_percap_thousand_idr')]
          proj_dat_df <- proj_dat[c('region',
                                  'year',
                                  'gdpr_growth',
                                  'intensity_biased',
                                  'kwh_dem_percap',
                                  'gdpr_billion_idr',
                                  'population',
                                  'gdpr_percap_thousand_idr')]
          all_data  <- rbind(his_dat_df,
                             proj_dat_df)
          

          
# show data
          # view(his_dat)     # show historical data - training data
          # view(proj_dat)    # show ANN prediction results data
          # view(nn_results)  # show all results combined
          # view(all_data)    # show all historical and prediction data
          

# End of line --------------------------------------------------------------

          # write data
          write_csv(all_data, "data_demand_forecast/training_prediction_data.csv")
          write_csv(nn_results, "data_demand_forecast/neural_network_results.csv")
          



            
            



