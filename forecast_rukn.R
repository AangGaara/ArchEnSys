library(readxl)
library(tidyverse)
library(reshape2)
library(data.table)
library(dplyr)
setwd("C:/Users/Brajamusthi/OneDrive/Master of Energy Research/Github/ArchEnSys")

province_code   <- read_excel("data_demand_forecast/glossary.xlsx", 
                            sheet = "code")
province_region <- read_excel("data_demand_forecast/glossary.xlsx", 
                              sheet = "region")

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

# RUKN projection data
# only for demand (gwh) and production (gwh)

  projection_rukn <- projection_data_source[c('Province',
                                              'year',
                                              'electricity_demand_gwh',
                                              'net_electricity_production_gwh')]
  # add region
  projection_rukn <- merge(province_region, projection_rukn, all.x = TRUE, by = "Province", sort = FALSE)
  
  # change names
  names(projection_rukn)[names(projection_rukn) == "electricity_demand_gwh"]         <- "dem_gwh_tot"
  names(projection_rukn)[names(projection_rukn) == "net_electricity_production_gwh"] <- "sup_gwh_tot"

  
  #sum all data from same year within same
  sup <- projection_rukn %>% group_by(region, year) %>%  summarise(dem_gwh_tot = sum(dem_gwh_tot))
  dem <- projection_rukn %>% group_by(region, year) %>%  summarise(sup_gwh_tot = sum(sup_gwh_tot))
  projection_rukn <- merge(sup, dem, by = c("region","year"), all.x = TRUE, sort = FALSE)
  

  
  #write data to projection_rukn in data_source folder
  write_csv(projection_rukn, "data_source/projection_rukn.csv") #whole data


  



