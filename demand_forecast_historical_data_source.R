library(readxl)
library(tidyverse)
setwd("C:/Users/Brajamusthi/OneDrive/Master of Energy Research/Github/ArchEnSys")

# Real GDP data ------------------------------------------------------------
# all in billion rupiah

        gdpr_2010_2011 <- read_excel("data_demand_forecast/gdpr/gdpr_production_2010_2011.xlsx", range = "A3:E38", col_names = TRUE)
          gdpr_2010_2011[2:3] <- NULL
          names(gdpr_2010_2011)[1] <- "Province"
          names(gdpr_2010_2011)[2] <- "2010"
          names(gdpr_2010_2011)[3] <- "2011"
          gdpr_2010_2011 <- pivot_longer(gdpr_2010_2011, cols = 2:3, names_to = "year", values_to = "gdpr")
        
        gdpr_2012_2014 <- read_excel("data_demand_forecast/gdpr/gdpr_production_2012_2014.xlsx", range = "A3:G38", col_names = TRUE)
          gdpr_2012_2014[2:4] <- NULL
          names(gdpr_2012_2014)[1] <- "Province"
          names(gdpr_2012_2014)[2] <- "2012"
          names(gdpr_2012_2014)[3] <- "2013"
          names(gdpr_2012_2014)[4] <- "2014"
          gdpr_2012_2014 <- pivot_longer(gdpr_2012_2014, cols = 2:4, names_to = "year", values_to = "gdpr")
        
        gdpr_2015_2017 <- read_excel("data_demand_forecast/gdpr/gdpr_production_2015_2017.xlsx", range = "A3:G38", col_names = TRUE)
          gdpr_2015_2017[2:4] <- NULL
          names(gdpr_2015_2017)[1] <- "Province"
          names(gdpr_2015_2017)[2] <- "2015"
          names(gdpr_2015_2017)[3] <- "2016"
          names(gdpr_2015_2017)[4] <- "2017"
          gdpr_2015_2017 <- pivot_longer(gdpr_2015_2017, cols = 2:4, names_to = "year", values_to = "gdpr")  
          
        gdpr_2018_2020 <- read_excel("data_demand_forecast/gdpr/gdpr_production_2018_2020.xlsx", range = "A3:G38", col_names = TRUE)
          gdpr_2018_2020[2:4] <- NULL
          names(gdpr_2018_2020)[1] <- "Province"
          names(gdpr_2018_2020)[2] <- "2018"
          names(gdpr_2018_2020)[3] <- "2019"
          names(gdpr_2018_2020)[4] <- "2020"
          gdpr_2018_2020 <- pivot_longer(gdpr_2018_2020, cols = 2:4, names_to = "year", values_to = "gdpr")
        
        #merge data
        gdpr_2010_2020 <- rbind(gdpr_2010_2011,
                                gdpr_2012_2014,
                                gdpr_2015_2017,
                                gdpr_2018_2020)
        
        #remove all spaces, and convert comma to dot, replace "-" with 0
        gdpr_2010_2020$gdpr <- sub(",", ".", gdpr_2010_2020$gdpr, fixed = TRUE)
        gdpr_2010_2020$gdpr <- sub("-", "0", gdpr_2010_2020$gdpr, fixed = TRUE)
        gdpr_2010_2020      <- gdpr_2010_2020 %>% mutate(gdpr = str_remove(gdpr, "\\s"))
        gdpr_2010_2020      <- gdpr_2010_2020 %>% mutate(gdpr = str_remove(gdpr, "\\s"))
        gdpr_2010_2020$gdpr <- as.numeric(gdpr_2010_2020$gdpr)
        names(gdpr_2010_2020)[3] <- "gdpr_billion_idr"

# Real GDPR growth -------------------------------------------------------------
# based on 2010 constant price
        gdpr_growth_2010_2011 <- read_excel("data_demand_forecast/gdpr/gdpr_production_growth_2010_2011.xlsx", range = "A2:C37", col_names = TRUE)
          gdpr_growth_2010_2011[2] <- NULL
          names(gdpr_growth_2010_2011)[1] <- "Province"
          gdpr_growth_2010_2011 <- pivot_longer(gdpr_growth_2010_2011, cols = 2, names_to = "year", values_to = "gdpr_growth")
        
        gdpr_growth_2012_2014 <- read_excel("data_demand_forecast/gdpr/gdpr_production_growth_2012_2014.xlsx", range = "A2:D37", col_names = TRUE)
          names(gdpr_growth_2012_2014)[1] <- "Province"
          gdpr_growth_2012_2014 <- pivot_longer(gdpr_growth_2012_2014, cols = 2:4, names_to = "year", values_to = "gdpr_growth")     
          
        gdpr_growth_2015_2017 <- read_excel("data_demand_forecast/gdpr/gdpr_production_growth_2015_2017.xlsx", range = "A2:D37", col_names = TRUE)
          names(gdpr_growth_2015_2017)[1] <- "Province"
          gdpr_growth_2015_2017 <- pivot_longer(gdpr_growth_2015_2017, cols = 2:4, names_to = "year", values_to = "gdpr_growth") 
          
        gdpr_growth_2018_2020 <- read_excel("data_demand_forecast/gdpr/gdpr_production_growth_2018_2020.xlsx", range = "A2:D37", col_names = TRUE)
          names(gdpr_growth_2018_2020)[1] <- "Province"
          gdpr_growth_2018_2020 <- pivot_longer(gdpr_growth_2018_2020, cols = 2:4, names_to = "year", values_to = "gdpr_growth") 

# merge data
          gdpr_growth_2011_2020 <- rbind(gdpr_growth_2010_2011,
                                         gdpr_growth_2012_2014,
                                         gdpr_growth_2015_2017,
                                         gdpr_growth_2018_2020)
          
          gdpr_growth_2011_2020$`gdpr_growth` <- sub(",", ".", gdpr_growth_2011_2020$`gdpr_growth`, fixed = TRUE)
          gdpr_growth_2011_2020$`gdpr_growth` <- as.numeric(gdpr_growth_2011_2020$`gdpr_growth`)
                  

# GDPR per capita data ----------------------------------------------------
        gdpr_percap_2010_2011 <- read_excel("data_demand_forecast/gdpr/gdpr_per_capita_production_2010_2011.xlsx", range = "A3:E38", col_names = TRUE)
          gdpr_percap_2010_2011[2:3] <- NULL
          names(gdpr_percap_2010_2011)[1] <- "Province"
          names(gdpr_percap_2010_2011)[2] <- "2010"
          names(gdpr_percap_2010_2011)[3] <- "2011"
          gdpr_percap_2010_2011 <- pivot_longer(gdpr_percap_2010_2011, cols = 2:3, names_to = "year", values_to = "gdpr_percap")
        
        gdpr_percap_2012_2014 <- read_excel("data_demand_forecast/gdpr/gdpr_per_capita_production_2012_2014.xlsx", range = "A3:G38", col_names = TRUE)
          gdpr_percap_2012_2014[2:4] <- NULL
          names(gdpr_percap_2012_2014)[1] <- "Province"
          names(gdpr_percap_2012_2014)[2] <- "2012"
          names(gdpr_percap_2012_2014)[3] <- "2013"
          names(gdpr_percap_2012_2014)[4] <- "2014"
          gdpr_percap_2012_2014 <- pivot_longer(gdpr_percap_2012_2014, cols = 2:4, names_to = "year", values_to = "gdpr_percap")
        
        gdpr_percap_2015_2017 <- read_excel("data_demand_forecast/gdpr/gdpr_per_capita_production_2015_2017.xlsx", range = "A3:G38", col_names = TRUE)
          gdpr_percap_2015_2017[2:4] <- NULL
          names(gdpr_percap_2015_2017)[1] <- "Province"
          names(gdpr_percap_2015_2017)[2] <- "2015"
          names(gdpr_percap_2015_2017)[3] <- "2016"
          names(gdpr_percap_2015_2017)[4] <- "2017"
          gdpr_percap_2015_2017 <- pivot_longer(gdpr_percap_2015_2017, cols = 2:4, names_to = "year", values_to = "gdpr_percap")
          
        gdpr_percap_2018_2020 <- read_excel("data_demand_forecast/gdpr/gdpr_per_capita_production_2018_2020.xlsx", range = "A3:G38", col_names = TRUE)
          gdpr_percap_2018_2020[2:4] <- NULL
          names(gdpr_percap_2018_2020)[1] <- "Province"
          names(gdpr_percap_2018_2020)[2] <- "2018"
          names(gdpr_percap_2018_2020)[3] <- "2019"
          names(gdpr_percap_2018_2020)[4] <- "2020"
          gdpr_percap_2018_2020 <- pivot_longer(gdpr_percap_2018_2020, cols = 2:4, names_to = "year", values_to = "gdpr_percap")
        
        #merge data
        gdpr_percap_2010_2020 <- rbind(gdpr_percap_2010_2011,
                                       gdpr_percap_2012_2014,
                                       gdpr_percap_2015_2017,
                                       gdpr_percap_2018_2020)
        
        #remove all spaces, and convert comma to dot, replace "-" with 0
        gdpr_percap_2010_2020$gdpr_percap <- sub(",", ".", gdpr_percap_2010_2020$gdpr_percap, fixed = TRUE)
        gdpr_percap_2010_2020$gdpr_percap <- sub("-", "0", gdpr_percap_2010_2020$gdpr_percap, fixed = TRUE)
        gdpr_percap_2010_2020             <- gdpr_percap_2010_2020 %>% mutate(gdpr_percap = str_remove(gdpr_percap, "\\s"))
        gdpr_percap_2010_2020             <- gdpr_percap_2010_2020 %>% mutate(gdpr_percap = str_remove(gdpr_percap, "\\s"))
        gdpr_percap_2010_2020$gdpr_percap <- as.numeric(gdpr_percap_2010_2020$gdpr_percap)
        names(gdpr_percap_2010_2020)[3]   <- "gdpr_percap_thousand_idr" 
        

# Population data ---------------------------------------------------------
# calculated from GDPR and GPDR per capita
# population = 1e+6 * GDPR (billion IDR) / GDPR per capita (thousand IDR / person)
        population_data            <- merge(gdpr_2010_2020, gdpr_percap_2010_2020)
        population_data$population <- 1e+6 * population_data$gdpr_billion_idr / population_data$gdpr_percap_thousand_idr
        population_data[3:4]       <- NULL

# Energy intensity data ---------------------------------------------------
# extract primary energy (national) from ourworldindata.org 
        energy_intensity           <- read_csv("data_demand_forecast/energy/primary-energy-cons.csv", col_names = TRUE) #read energy consumption of Indonesia in TWh
        energy_intensity           <- subset(energy_intensity, energy_intensity$Entity == "Indonesia" & energy_intensity$Year > 2009)
        names(energy_intensity)[1] <- "Province"
        names(energy_intensity)[3] <- "year"
        names(energy_intensity)[4] <- "twh_cons"
        energy_intensity[2]        <- NULL
        energy_intensity$Province  <- toupper(energy_intensity$Province)
        energy_intensity$MJ        <- energy_intensity$twh_cons * 3600000000 # convert TWh to MJ
        
# extract national GDP
        # from GDPR (billion IDR)
        energy_intensity     <- merge(energy_intensity, gdpr_2010_2020, by = c("Province","year"), sort = FALSE)
        
        # nominal gdp from the world bank ($)
        gdp_national_nominal           <- read_excel("data_demand_forecast/gdpr/gdp_world_nominal.xls", range = "A4:BL268", sheet = "Data", col_names = TRUE)
        gdp_national_nominal           <- subset(gdp_national_nominal, gdp_national_nominal$'Country Name' == "Indonesia", col_names = TRUE)
        gdp_national_nominal[2:4]      <- NULL
        names(gdp_national_nominal)[1] <- "Province"
        gdp_national_nominal$Province  <- toupper(gdp_national_nominal$Province)
        gdp_national_nominal           <- pivot_longer(gdp_national_nominal, cols = 2:61, names_to = "year", values_to = "gdp_nominal_usd")
        gdp_national_nominal$year      <- as.numeric(gdp_national_nominal$year)
        energy_intensity <- merge(energy_intensity, gdp_national_nominal, by = c("Province","year"), sort = FALSE)
        
        # real gdp from the world bank, const 2010 ($)
        gdp_national_real              <- read_excel("data_demand_forecast/gdpr/gdp_world_real_2010.xls", range = "A4:BL268", sheet = "Data", col_names = TRUE)
        gdp_national_real              <- subset(gdp_national_real, gdp_national_real$'Country Name' == "Indonesia", col_names = TRUE)
        gdp_national_real[2:4]         <- NULL
        names(gdp_national_real)[1]    <- "Province"
        gdp_national_real$Province     <- toupper(gdp_national_real$Province)
        gdp_national_real              <- pivot_longer(gdp_national_real, cols = 2:61, names_to = "year", values_to = "gdp_real_2010_usd")
        energy_intensity <- merge(energy_intensity, gdp_national_real, by = c("Province","year"), sort = TRUE)
        
        # gdp PPP nominal value from world bank ($)
        gdp_ppp_nominal           <- read_excel("data_demand_forecast/gdpr/gdp_ppp_nominal.xls", range = "A4:BL268", sheet = "Data", col_names = TRUE)
        gdp_ppp_nominal           <- subset(gdp_ppp_nominal, gdp_ppp_nominal$`Country Name` == "Indonesia", col_names = TRUE)
        gdp_ppp_nominal[2:4]      <- NULL
        names(gdp_ppp_nominal)[1] <- "Province"
        gdp_ppp_nominal$Province  <- toupper(gdp_ppp_nominal$Province)
        gdp_ppp_nominal           <- pivot_longer(gdp_ppp_nominal, cols = 2:61, names_to = "year", values_to = "gdp_ppp_nom_usd")
        energy_intensity          <- merge(energy_intensity, gdp_ppp_nominal, by = c("Province", "year"), sort = TRUE)
        
        # gdp PPP real from the world bank, cosnt 2017 ($)
        gdp_ppp_real <- read_excel("data_demand_forecast/gdpr/gdp_ppp_real_2010.xls", range = "A4:BL268", sheet = "Data", col_names = TRUE)
        gdp_ppp_real <- subset(gdp_ppp_real, gdp_ppp_real$`Country Name` == "Indonesia", col_names = TRUE)
        gdp_ppp_real[2:4] <- NULL
        names(gdp_ppp_real)[1] <- "Province"
        gdp_ppp_real$Province <- toupper(gdp_ppp_real$Province)
        gdp_ppp_real <- pivot_longer(gdp_ppp_real, cols = 2:61, names_to = "year", values_to = "gdp_ppp_real_usd")
        energy_intensity <- merge(energy_intensity, gdp_ppp_real, by = c("Province","year"), sort = TRUE)
        
        # energy intensity = GDP / consumption (TWh)
        # intensity 1 - IDR                  ___ (BOE / Billion IDR)
        energy_intensity$intensity_gdpr     <- 588441 * energy_intensity$twh_cons / energy_intensity$gdpr_billion_idr
        # intensity 2 - nominal GDP ($)      ___ (MJ / USD)
        energy_intensity$intensity_nominal  <- energy_intensity$MJ / energy_intensity$gdp_nominal_usd
        # intensity 3 - real GDP ($)         ___ (MJ / USD)
        energy_intensity$intensity_real     <- energy_intensity$MJ / energy_intensity$gdp_real_2010_usd
        # intensity 4 - GDP PPP nominal ($)  ___ (MJ / USD)
        energy_intensity$intensity_ppp_nom  <- energy_intensity$MJ / energy_intensity$gdp_ppp_nom_usd
        # intensity 5 - GDP PPP real ($)     ___ (MJ / USD)
        energy_intensity$intensity_ppp_real <- energy_intensity$MJ / energy_intensity$gdp_ppp_real_usd
        
        # create biased energy intensity to be used for the historical data
        energy_intensity$intensity_biased   <- 1.0 * energy_intensity$intensity_gdpr
      
        # Why the GDP is below 3, while internet sources says it is 4 MJ in 2015 ???????????
        

# Electricity price data --------------------------------------------------
# this electricity price data is to be filled with national average electricity price from 2010 to 2019

# this data is hard to find, thus will be skipped


# generation (non-sectoral) gWh data ----------------------------------------------------------------
# generation data (GWh) from BPS, stored locally in github folder
        gen_gwh_2011_2012 <- read_excel("data_demand_forecast/gWh/generation_gwh_2011_2012.xlsx", range = "A2:C37", col_names = TRUE)
          names(gen_gwh_2011_2012)[1] <- "Province"
          gen_gwh_2011_2012 <- pivot_longer(gen_gwh_2011_2012, cols = 2:3, names_to = "year", values_to = "gen_gwh")
        
        gen_gwh_2013_2015 <- read_excel("data_demand_forecast/gWh/generation_gwh_2013_2015.xlsx", range = "A2:D37", col_names = TRUE)
          names(gen_gwh_2013_2015)[1] <- "Province"
          gen_gwh_2013_2015 <- pivot_longer(gen_gwh_2013_2015, cols = 2:4, names_to = "year", values_to = "gen_gwh")
        
        gen_gwh_2017_2019 <- read_excel("data_demand_forecast/gWh/generation_gwh_2017_2019.xlsx", range = "A2:D37", col_names = TRUE)
          names(gen_gwh_2017_2019)[1] <- "Province"
          gen_gwh_2017_2019 <- pivot_longer(gen_gwh_2017_2019, cols = 2:4, names_to = "year", values_to = "gen_gwh")
        
        #merge all data
        gen_gwh_2011_2019 <- rbind(gen_gwh_2011_2012, 
                                   gen_gwh_2013_2015, 
                                   gen_gwh_2017_2019)
        
        #remove all spaces, and convert comma to dot, replace "-" with 0
        gen_gwh_2011_2019[3] <- sub(",", ".", gen_gwh_2011_2019$gen_gwh, fixed = TRUE)
        gen_gwh_2011_2019[3] <- sub("-", "0", gen_gwh_2011_2019$gen_gwh, fixed = TRUE)
        gen_gwh_2011_2019    <- gen_gwh_2011_2019 %>%  mutate(gen_gwh = str_remove(gen_gwh, "\\s"))
        gen_gwh_2011_2019$gen_gwh    <- as.numeric(gen_gwh_2011_2019$gen_gwh)
        
# Distributed electricity gWh data ----------------------------------------
# from BPS, stored locally in github folder
# distributed electricity to each province in Indonesia
        dist_gwh_2011_2012 <- read_excel("data_demand_forecast/gWh/distributed_gwh_2011_2012.xlsx", range = "A2:C37", col_names = TRUE)
        names(dist_gwh_2011_2012)[1] <- "Province"
        dist_gwh_2011_2012 <- pivot_longer(dist_gwh_2011_2012, cols = 2:3, names_to = "year", values_to = "dist_gwh")
        
        dist_gwh_2013_2015 <- read_excel("data_demand_forecast/gWh/distributed_gwh_2013_2015.xlsx", range = "A2:D37", col_names = TRUE)
        names(dist_gwh_2013_2015)[1] <- "Province"
        dist_gwh_2013_2015 <- pivot_longer(dist_gwh_2013_2015, cols = 2:4, names_to = "year", values_to = "dist_gwh")
        
        dist_gwh_2017_2019 <- read_excel("data_demand_forecast/gWh/distributed_gwh_2017_2019.xlsx", range = "A2:D37", col_names = TRUE)
        names(dist_gwh_2017_2019)[1] <- "Province"
        dist_gwh_2017_2019 <- pivot_longer(dist_gwh_2017_2019, cols = 2:4, names_to = "year", values_to = "dist_gwh")
        
        #merge all data
        dist_gwh_2011_2019 <- rbind(dist_gwh_2011_2012, dist_gwh_2013_2015, dist_gwh_2017_2019)
        
        #remove all spaces, and convert comma to dot, replace "-" with 0
        dist_gwh_2011_2019[3] <- sub(",", ".", dist_gwh_2011_2019$dist_gwh, fixed = TRUE)
        dist_gwh_2011_2019[3] <- sub("-", "0", dist_gwh_2011_2019$dist_gwh, fixed = TRUE)
        dist_gwh_2011_2019    <- dist_gwh_2011_2019 %>%  mutate(dist_gwh = str_remove(dist_gwh, "\\s"))
        dist_gwh_2011_2019$dist_gwh    <- as.numeric(dist_gwh_2011_2019$dist_gwh)
        
# Electricity consumption per capita --------------------------------------
# electricity consumption per capita (kWh) = dist electricty (gWh) / population
# calculation using distributed electricity, instead of generated as there are areas without electricity generation facilities
        
        kwh_consumption_percap                 <- merge(dist_gwh_2011_2019, population_data)
        kwh_consumption_percap$kwh_dem_percap  <- 1e+6 * kwh_consumption_percap$dist_gwh / kwh_consumption_percap$population
        kwh_consumption_percap[3:4]            <- NULL
        
        
# Installed capacity ------------------------------------------------------
# capacity in MW for each province
        capacity_2011_2012 <- read_excel("data_demand_forecast/capacity/capacity_mw_2011_2012.xlsx", range = "A2:C37", col_names = TRUE)
          names(capacity_2011_2012)[1] <- "Province"
          capacity_2011_2012 <- pivot_longer(capacity_2011_2012, cols = 2:3, names_to = "year", values_to = "capacity_mw")
          
        capacity_2013_2015 <- read_excel("data_demand_forecast/capacity/capacity_mw_2013_2015.xlsx", range = "A2:D37", col_names = TRUE)
          names(capacity_2013_2015)[1] <- "Province"
          capacity_2013_2015 <- pivot_longer(capacity_2013_2015, cols = 2:4, names_to = "year", values_to = "capacity_mw")
          
        capacity_2017_2019 <- read_excel("data_demand_forecast/capacity/capacity_mw_2017_2019.xlsx", range = "A2:D37", col_names = TRUE)
          names(capacity_2017_2019)[1] <- "Province"
          capacity_2017_2019 <- pivot_longer(capacity_2017_2019, cols = 2:4, names_to = "year", values_to = "capacity_mw")
          
        #merge all data
        capacity_2011_2019 <- rbind(capacity_2011_2012,
                                    capacity_2013_2015,
                                    capacity_2017_2019)
        #remove all spaces, and convert comma to dot, replace "_" with 0
        capacity_2011_2019[3]          <- sub(",", ".", capacity_2011_2019$capacity_mw, fixed = TRUE)
        capacity_2011_2019[3]          <- sub("-", "0", capacity_2011_2019$capacity_mw, fixed = TRUE)
        capacity_2011_2019             <- capacity_2011_2019 %>% mutate(capacity_mw = str_remove(capacity_mw, "\\s"))
        capacity_2011_2019$capacity_mw <- as.numeric (capacity_2011_2019$capacity_mw)

# Bind all data ---------------------------------------------------------------------
        historical_data_source <- merge(gdpr_2010_2020,         gdpr_percap_2010_2020, by = c("Province","year"), sort = FALSE)
        historical_data_source <- merge(historical_data_source, gdpr_growth_2011_2020, by = c("Province","year"), sort = FALSE)
        historical_data_source <- merge(historical_data_source, population_data,       by = c("Province","year"), sort = FALSE)
        historical_data_source <- merge(historical_data_source, gen_gwh_2011_2019,     by = c("Province","year"), sort = FALSE)
        historical_data_source <- merge(historical_data_source, dist_gwh_2011_2019,    by = c("Province","year"), sort = FALSE)
        historical_data_source <- merge(historical_data_source, kwh_consumption_percap,by = c("Province","year"), sort = FALSE)
        historical_data_source <- merge(historical_data_source, capacity_2011_2019,    By = c("Province","year"), sort = FALSE)

# assign energy intensity to the main data
        energy_intensity[3:14] <- NULL
        energy_intensity[1]    <- NULL
        historical_data_source <- merge(historical_data_source, energy_intensity, all.x = TRUE, by = "year", sort  = FALSE)
# set year as numeric
        historical_data_source$year <- as.numeric(historical_data_source$year)
        
# replace or NA with 0 (zero)
        historical_data_source[is.na(historical_data_source)] <- 0

# add region column
        
        province_region <- read_excel("data_demand_forecast/glossary.xlsx", sheet = "region")
        historical_data_source <- merge(province_region, historical_data_source, all.x = TRUE, by = "Province", sort = TRUE)

# End ---------------------------------------------------------------------
    # write data
    # write_csv(historical_data_source, "data_demand_forecast/forecast_data_source.csv") #whole data

