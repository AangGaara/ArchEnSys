library(readxl)
library(dplyr)

setwd("C:/Users/Brajamusthi/OneDrive/Master of Energy Research/Github/ArchEnSys")
min_year = 2019
max_year = 2038

# Prepare data ------------------------------------------------------------
  
# look up data
  excel_sheets("data_source/data_source.xlsx")
  tech_data         <- read_excel("data_source/data_source.xlsx",
                                  sheet = "tech_data",
                                  range = "A1:O15")
  potential_mw      <- read_excel("data_source/data_source.xlsx",
                                  sheet = "potential",
                                  range = "A1:Q8")
  existing_capacity <- read_excel("data_source/data_source.xlsx",
                                  sheet = "existing_capacity",
                                  range = "A1:Q8")
  projection_data   <- read_excel("data_source/data_source.xlsx",
                                  sheet = "proj_rukn",
                                  range = "A1:D141") #in case RUKN projection data will be used
  region_year       <- projection_data[c('region','year')]
  
  
# data to make
  sup_gwh           <- read_excel("data_source/data_source.xlsx",
                                  sheet = "sup_gwh")
  sup_percent       <- read_excel("data_source/data_source.xlsx",
                                  sheet = "sup_percent")
  cap_mw            <- read_excel("data_source/data_source.xlsx",
                                  sheet = "cap_mw")
  add_mw            <- read_excel("data_source/data_source.xlsx",
                                  sheet = "add_mw")
  remain_pot        <- read_excel("data_source/data_source.xlsx",
                                  sheet = "remain_pot")
  prim_pj           <- read_excel("data_source/data_source.xlsx",
                                  sheet = "prim_pj")
  los_pj            <- read_excel("data_source/data_source.xlsx",
                                  sheet = "los_pj")
  cost_musd          <- read_excel("data_source/data_source.xlsx",
                                  sheet = "cost_musd")
  co2_ton            <- read_excel("data_source/data_source.xlsx",
                                  sheet = "co2_ton")
  
# input region and year to all data
  #sup_gwh           <- merge(sup_gwh,     region_year, all.x = TRUE, all.y = TRUE, sort = FALSE)
  #sup_percent       <- merge(sup_percent, region_year, all.x = TRUE, all.y = TRUE, sort = FALSE)
  #cap_mw            <- merge(cap_mw,      region_year, all.x = TRUE, all.y = TRUE, sort = FALSE)
  #add_mw            <- merge(add_mw,      region_year, all.x = TRUE, all.y = TRUE, sort = FALSE)  
  #remain_pot        <- merge(remain_pot,  region_year, all.x = TRUE, all.y = TRUE, sort = FALSE)
  #prim_gj           <- merge(prim_gj,     region_year, all.x = TRUE, all.y = TRUE, sort = FALSE)
  #los_gj            <- merge(los_gj,      region_year, all.x = TRUE, all.y = TRUE, sort = FALSE)
  #cost_usd          <- merge(cost_usd,    region_year, all.x = TRUE, all.y = TRUE, sort = FALSE)
  #co2_kg            <- merge(co2_kg,      region_year, all.x = TRUE, all.y = TRUE, sort = FALSE)  
  
# Initiation --------------------------------------------------------------
  # populate existing MW to capacity
  cap_mw <- rbind(cap_mw, existing_capacity)
  
  # set additional capacity to 0 in 2019
  add_mw <- rbind(add_mw, existing_capacity)
  add_mw[3:17] <- 0
  
  # calculate output mwh
  a <- (cap_mw[cap_mw$year == 2019, c(3:17)] + add_mw[add_mw$year == 2019, c(3:17)])
  
  for (i in cap_mw[]){
    print (i)
  }
  

  

# Decision variables ------------------------------------------------------



# Objective function ------------------------------------------------------


# Constraints -------------------------------------------------------------



# Definitions -------------------------------------------------------------


