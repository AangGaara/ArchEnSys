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
  

  # pivot to wider variable
  # https://seananderson.ca/2013/10/19/reshape/
  projection_data_source <- reshape2:: dcast(projection_data,
                                             Province + year ~ new_var,
                                             value.var = "value",
                                             fun.aggregate =  mean,
                                             na.rm = TRUE)
  
  # return the values to original units
  
  
  
  # remove unneeded data
  projection_data_source[,c('inflation',
                            'add_capacity_cum',
                            'add_capacity',
                            'electrification_ratio',
                            'total_capacity')] <- NULL

# GDPR --------------------------------------------------------------------
# calculated from gdpr growth and gdpr in historical data

  # GDPR from 2019
  # read data from historical_data_source
  source("demand_forecast_historical_data_source.R")
  #read 2019 GDPR data
  gdpr_billion_idr_2019 <- historical_data_source[historical_data_source$year =="2019", c('Province',
                                                                                           'year',
                                                                                           'gdpr_billion_idr')]
  #input 2019 gdpr data to projection data
  projection_data_source$gdpr_billion_idr_2019 <- 
  aprojection_data_source <- merge(projection_data_source, gdpr_billion_idr_2019, by = c("Province"), all.x = TRUE, sort = FALSE)
  
  # calculate GDPR for following years
  # gdpr(n) = gdpr (n-1) * (1+(gdpr_growth / 100))
  first_year = 2019
  
  aprojection_data_source$gdpr_billion_idr = aprojection_data_source$gdpr_billion_idr * (1+((aprojection_data_source$gdpr_growth)/100))
  
  

# Population growth -------------------------------------------------------
#taken from population growth data
  

# Population --------------------------------------------------------------
# calculated from population growth and population historical data


# Energy Intensity --------------------------------------------------------



# Electricity consumption per capita --------------------------------------

    

  



