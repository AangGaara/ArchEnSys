library(readxl)
library(tidyverse)
library(reshape2)
library(data.table)
setwd("C:/Users/Brajamusthi/OneDrive/Master of Energy Research/Github/ArchEnSys")

province_code <- read_excel("data_demand_forecast/glossary.xlsx", 
                            sheet = "code")

# this script prepares prediction datasets
# dist_gwh = year + gdpr_billion_idr + gdpr_growth + population + gwh_cons_percap + intensity_biased,

# RUKN data read ______________________________________________________________________________
# read master data
  sheet           <- excel_sheets("data_demand_forecast/forecast/rukn_proyeksi_kebutuhan_listrik.xlsx" )
  rukn_projection = lapply(setNames(sheet, sheet), 
                           function(x) read_excel("data_demand_forecast/forecast/rukn_proyeksi_kebutuhan_listrik.xlsx", range = "A2:L15", col_names = TRUE, sheet = x))
  rukn_projection = bind_rows(rukn_projection, .id="Code")
  rukn_projection$Code      <- as.numeric(sub("page ","", rukn_projection$Code, fixed = TRUE))
  names(rukn_projection)[names(rukn_projection) == "URAIAN"] <- "old_var"
  rukn_projection           <- subset(rukn_projection, old_var!= "ASUMSI & TARGET" & old_var!= "HASIL PROYEKSI")
  rukn_projection$SATUAN    <- NULL
  rukn_projection           <- rukn_projection %>%  mutate(old_var = str_remove_all(old_var, "\\s"))
  
  #change variable names ***** does not work
 # new_variables <- read_excel("data_demand_forecast/glossary.xlsx", sheet = "variables")
 # projection_data <- merge(rukn_projection, unique(new_variables), all.x = TRUE, by = "old_var", sort = TRUE)
 # projection_data$old_var <- NULL
  
  # reshape the data
  projection_data <- reshape2:: melt(rukn_projection, id.vars = c("Code","old_var"))
  
  # assign name to each sheet
  projection_data           <- merge(province_code, projection_data, by = "Code") 
  projection_data$Code      <- NULL
  projection_data           <- na.omit(projection_data) #drop NA's
  names(projection_data)[names(projection_data) == "variable"] <- "year"
  
  #clean variable names
  projection_data  <- projection_data %>%  mutate(old_var = str_remove_all(old_var, "\\s"))
  new_variables    <- read_excel("data_demand_forecast/glossary.xlsx", range = "A1:B12", sheet = "variables")
  new_variables    <- new_variables %>% mutate(old_var = str_remove_all(old_var,"\\s"))
  projection_data  <- merge(projection_data, new_variables, by = 'old_var')
  projection_data$old_var <- NULL

    
  #change values to numerical
  projection_data$value  <- sub(".","", projection_data$value, fixed = TRUE)
  projection_data$value  <- sub(",",".", projection_data$value, fixed = TRUE)
  projection_data$value  <- sub("(","-", projection_data$value, fixed = TRUE)
  projection_data$value  <- sub(")","", projection_data$value, fixed = TRUE)
  projection_data$value  <- as.numeric(projection_data$value)
  projection_data$year   <- as.numeric(projection_data$year) + 2018 #set year to numeric
  

  # pivot to wider variable
  # https://seananderson.ca/2013/10/19/reshape/
  projection_data_source <- reshape2:: dcast(projection_data,
                                             Province + year ~ new_var,
                                             value.var = "value",
                                             fun.aggregate =  mean,
                                             na.rm = TRUE)
  
  
  # remove unneeded data
  projection_data_source[,c('inflation',
                            'add_capacity_cum_gw',
                            'add_capacity_gw',
                            'electrification_ratio',
                            'total_capacity_gw')] <- NULL


# Read historical data ----------------------------------------------------
  # read data from historical_data_source
  # to take data from last year of the historical, in this case: 2019 data
  source("demand_forecast_historical_data_source.R")
  first_year = 2019
  # will be calculated based on government targets
  # please check report chapter 2, in Indonesia's energy policy section
  # for long term the Energy intensity growth will be at -1% per year
  intensity_growth = -1

  #read 2019 data
  gdpr_billion_idr_2019 <- historical_data_source[historical_data_source$year == first_year, c('Province',
                                                                                               'year',
                                                                                               'gdpr_billion_idr')]
  population_2019       <- historical_data_source[historical_data_source$year == first_year, c('Province',
                                                                                               'year',
                                                                                               'population')]
  intensity_2019        <- historical_data_source[historical_data_source$year == first_year, c('Province',
                                                                                               'year',
                                                                                               'intensity_biased')]
  
  #input 2019 data to projection data source
  projection_data_source <- merge(projection_data_source, gdpr_billion_idr_2019, by = c("Province","year"), all.x = TRUE, sort = FALSE)
  projection_data_source <- merge(projection_data_source, population_2019, by = c("Province","year"), all.x = TRUE, sort = FALSE)
  projection_data_source <- merge(projection_data_source, intensity_2019, by = c("Province","year"), all.x = TRUE, sort = FALSE)
  
# Populate data with calculated values ------------------------------------
  aprojection_data_source <- projection_data_source # for trial and error
  
  for (i in unique(aprojection_data_source$Province)){
    for (j in aprojection_data_source$year[aprojection_data_source$year > first_year]){
      
      current_location  = aprojection_data_source$Province == i & aprojection_data_source$year == j
      previous_location = aprojection_data_source$Province == i & aprojection_data_source$year == j-1
      
            # GDRP in billion IDR
      aprojection_data_source$gdpr_billion_idr[current_location] <- 
        aprojection_data_source$gdpr_billion_idr[previous_location] * (1+(aprojection_data_source$gdpr_growth[current_location]/100))
      
      # population
      aprojection_data_source$population[current_location] <- 
        aprojection_data_source$population[previous_location] * (1+(aprojection_data_source$population_growth[current_location]/100))
      
      # energy intensity
      aprojection_data_source$intensity_biased[current_location] <- 
        aprojection_data_source$intensity_biased[previous_location] * (1+(intensity_growth/100))
      
    }
  }

# Create projection dataset -----------------------------------------------
  projection_dataset <- aprojection_data_source[c('Province',
                                                  'year',
                                                  'gdpr_billion_idr',
                                                  'gdpr_growth',
                                                  'population',
                                                  'kwh_dem_percap',
                                                  'intensity_biased',
                                                  'electricity_demand_twh')]


  



