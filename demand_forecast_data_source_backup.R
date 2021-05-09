library(readxl)
library(tidyverse)
setwd("C:/Users/Brajamusthi/OneDrive/Master of Energy Research/Github/ArchEnSys")

#set region names
Region_1 <- "SUMATERA"
Region_2 <- "KALIMANTAN"
Region_3 <- "SULAWESI"
Region_4 <- "MALUKU"
Region_5 <- "PAPUA"
Region_6 <- "NUSA TENGGARA"
Region_7 <- "JAVA"
Region_8 <- "NATIONAL"

# Real GDPR data ----------------------------------------------------------------
# all in million rupiah, 2010 based
gdpr_const_2011 <- read_excel("data_demand_forecast/GDPR/gdpr_const_2011.xlsx", range = "A4:AJ39", col_names = TRUE)
  gdpr_const_2011[2:35] <- NULL
  names(gdpr_const_2011)[1] <- "Province"
  names(gdpr_const_2011)[2] <- "2011"
  gdpr_const_2011$Province[35] <- "INDONESIA"
  gdpr_const_2011 <- pivot_longer(gdpr_const_2011, cols = 2, names_to = "year", values_to = "gdpr")
  
gdpr_const_2012 <- read_excel("data_demand_forecast/GDPR/gdpr_const_2012.xlsx", range = "A4:AJ39", col_names = TRUE)
  gdpr_const_2012[2:35] <- NULL
  names(gdpr_const_2012)[1] <- "Province"
  names(gdpr_const_2012)[2] <- "2012"
  gdpr_const_2012$Province[35] <- "INDONESIA"
  gdpr_const_2012 <- pivot_longer(gdpr_const_2012, cols = 2, names_to = "year", values_to = "gdpr")
  
gdpr_const_2013 <- read_excel("data_demand_forecast/GDPR/gdpr_const_2013.xlsx", range = "A4:AJ39", col_names = TRUE)
  gdpr_const_2013[2:35] <- NULL
  names(gdpr_const_2013)[1] <- "Province"
  names(gdpr_const_2013)[2] <- "2013"
  gdpr_const_2013$Province[35] <- "INDONESIA"
  gdpr_const_2013 <- pivot_longer(gdpr_const_2013, cols = 2, names_to = "year", values_to = "gdpr")

gdpr_const_2014 <- read_excel("data_demand_forecast/GDPR/gdpr_const_2014.xlsx", range = "A4:AJ39", col_names = TRUE)
  gdpr_const_2014[2:35] <- NULL
  names(gdpr_const_2014)[1] <- "Province"
  names(gdpr_const_2014)[2] <- "2014"
  gdpr_const_2014$Province[35] <- "INDONESIA"
  gdpr_const_2014 <- pivot_longer(gdpr_const_2014, cols = 2, names_to = "year", values_to = "gdpr")

gdpr_const_2015 <- read_excel("data_demand_forecast/GDPR/gdpr_const_2015.xlsx", range = "A4:AJ39", col_names = TRUE)
  gdpr_const_2015[2:35] <- NULL
  names(gdpr_const_2015)[1] <- "Province"
  names(gdpr_const_2015)[2] <- "2015"
  gdpr_const_2015$Province[35] <- "INDONESIA"
  gdpr_const_2015 <- pivot_longer(gdpr_const_2015, cols = 2, names_to = "year", values_to = "gdpr")

gdpr_const_2016 <- read_excel("data_demand_forecast/GDPR/gdpr_const_2016.xlsx", range = "A4:AJ39", col_names = TRUE)
  gdpr_const_2016[2:35] <- NULL
  names(gdpr_const_2016)[1] <- "Province"
  names(gdpr_const_2016)[2] <- "2016"
  gdpr_const_2016$Province[35] <- "INDONESIA"
  gdpr_const_2016 <- pivot_longer(gdpr_const_2016, cols = 2, names_to = "year", values_to = "gdpr")

gdpr_const_2017 <- read_excel("data_demand_forecast/GDPR/gdpr_const_2017.xlsx", range = "A4:AJ39", col_names = TRUE)
  gdpr_const_2017[2:35] <- NULL
  names(gdpr_const_2017)[1] <- "Province"
  names(gdpr_const_2017)[2] <- "2017"
  gdpr_const_2017$Province[35] <- "INDONESIA"
  gdpr_const_2017 <- pivot_longer(gdpr_const_2017, cols = 2, names_to = "year", values_to = "gdpr")
  
gdpr_const_2018 <- read_excel("data_demand_forecast/GDPR/gdpr_const_2018.xlsx", range = "A4:AJ39", col_names = TRUE)
  gdpr_const_2018[2:35] <- NULL
  names(gdpr_const_2018)[1] <- "Province"
  names(gdpr_const_2018)[2] <- "2018"
  gdpr_const_2018$Province[35] <- "INDONESIA"
  gdpr_const_2018 <- pivot_longer(gdpr_const_2018, cols = 2, names_to = "year", values_to = "gdpr")
  
gdpr_const_2019 <- read_excel("data_demand_forecast/GDPR/gdpr_const_2019.xlsx", range = "A4:AJ39", col_names = TRUE)
  gdpr_const_2019[2:35] <- NULL
  names(gdpr_const_2019)[1] <- "Province"
  names(gdpr_const_2019)[2] <- "2019"
  gdpr_const_2019$Province[35] <- "INDONESIA"
  gdpr_const_2019 <- pivot_longer(gdpr_const_2019, cols = 2, names_to = "year", values_to = "gdpr")

  
#merge data
gdpr_2011_2019 <- rbind(gdpr_const_2011, gdpr_const_2012, gdpr_const_2013, gdpr_const_2014, gdpr_const_2015, 
                        gdpr_const_2016, gdpr_const_2017, gdpr_const_2018, gdpr_const_2019)
#remove all spaces, and convert comma to dot, replace "-" with 0
gdpr_2011_2019[3] <- sub(",", ".", gdpr_2011_2019$gdpr, fixed = TRUE)
gdpr_2011_2019[3] <- sub("-", "0", gdpr_2011_2019$gdpr, fixed = TRUE)
gdpr_2011_2019    <- gdpr_2011_2019 %>%  mutate(gdpr = str_remove(gdpr, "\\s"))
gdpr_2011_2019    <- gdpr_2011_2019 %>%  mutate(gdpr = str_remove(gdpr, "\\s"))
gdpr_2011_2019    <- gdpr_2011_2019 %>%  mutate(gdpr = str_remove(gdpr, "\\s"))


# Nominal GDP data ------------------------------------------------------------
# all in million rupiah
gdpr_nom_2011 <- read_excel("data_demand_forecast/GDPR/gdpr_nom_2011.xlsx", range = "A4:AJ39", col_names = TRUE)
  gdpr_nom_2011[2:35] <- NULL
  names(gdpr_nom_2011)[1] <- "Province"
  names(gdpr_nom_2011)[2] <- "2011"
  gdpr_nom_2011$Province[35] <- "INDONESIA"
  gdpr_nom_2011 <- pivot_longer(gdpr_nom_2011, cols = 2, names_to = "year", values_to = "gdpr_nom")
  
gdpr_nom_2012 <- read_excel("data_demand_forecast/GDPR/gdpr_nom_2012.xlsx", range = "A4:AJ39", col_names = TRUE)
  gdpr_nom_2012[2:35] <- NULL
  names(gdpr_nom_2012)[1] <- "Province"
  names(gdpr_nom_2012)[2] <- "2012"
  gdpr_nom_2012$Province[35] <- "INDONESIA"
  gdpr_nom_2012 <- pivot_longer(gdpr_nom_2012, cols = 2, names_to = "year", values_to = "gdpr_nom")

gdpr_nom_2013 <- read_excel("data_demand_forecast/GDPR/gdpr_nom_2013.xlsx", range = "A4:AJ39", col_names = TRUE)
  gdpr_nom_2013[2:35] <- NULL
  names(gdpr_nom_2013)[1] <- "Province"
  names(gdpr_nom_2013)[2] <- "2013"
  gdpr_nom_2013$Province[35] <- "INDONESIA"
  gdpr_nom_2013 <- pivot_longer(gdpr_nom_2013, cols = 2, names_to = "year", values_to = "gdpr_nom")
  
gdpr_nom_2014 <- read_excel("data_demand_forecast/GDPR/gdpr_nom_2014.xlsx", range = "A4:AJ39", col_names = TRUE)
  gdpr_nom_2014[2:35] <- NULL
  names(gdpr_nom_2014)[1] <- "Province"
  names(gdpr_nom_2014)[2] <- "2014"
  gdpr_nom_2014$Province[35] <- "INDONESIA"
  gdpr_nom_2014 <- pivot_longer(gdpr_nom_2014, cols = 2, names_to = "year", values_to = "gdpr_nom")

gdpr_nom_2015 <- read_excel("data_demand_forecast/GDPR/gdpr_nom_2015.xlsx", range = "A4:AJ39", col_names = TRUE)
  gdpr_nom_2015[2:35] <- NULL
  names(gdpr_nom_2015)[1] <- "Province"
  names(gdpr_nom_2015)[2] <- "2015"
  gdpr_nom_2015$Province[35] <- "INDONESIA"
  gdpr_nom_2015 <- pivot_longer(gdpr_nom_2015, cols = 2, names_to = "year", values_to = "gdpr_nom")

gdpr_nom_2016 <- read_excel("data_demand_forecast/GDPR/gdpr_nom_2016.xlsx", range = "A4:AJ39", col_names = TRUE)
  gdpr_nom_2016[2:35] <- NULL
  names(gdpr_nom_2016)[1] <- "Province"
  names(gdpr_nom_2016)[2] <- "2016"
  gdpr_nom_2016$Province[35] <- "INDONESIA"
  gdpr_nom_2016 <- pivot_longer(gdpr_nom_2016, cols = 2, names_to = "year", values_to = "gdpr_nom")

gdpr_nom_2017 <- read_excel("data_demand_forecast/GDPR/gdpr_nom_2017.xlsx", range = "A4:AJ39", col_names = TRUE)
  gdpr_nom_2017[2:35] <- NULL
  names(gdpr_nom_2017)[1] <- "Province"
  names(gdpr_nom_2017)[2] <- "2017"
  gdpr_nom_2017$Province[35] <- "INDONESIA"
  gdpr_nom_2017 <- pivot_longer(gdpr_nom_2017, cols = 2, names_to = "year", values_to = "gdpr_nom")
  
gdpr_nom_2018 <- read_excel("data_demand_forecast/GDPR/gdpr_nom_2018.xlsx", range = "A4:AJ39", col_names = TRUE)
  gdpr_nom_2018[2:35] <- NULL
  names(gdpr_nom_2018)[1] <- "Province"
  names(gdpr_nom_2018)[2] <- "2018"
  gdpr_nom_2018$Province[35] <- "INDONESIA"
  gdpr_nom_2018 <- pivot_longer(gdpr_nom_2018, cols = 2, names_to = "year", values_to = "gdpr_nom")
  
gdpr_nom_2019 <- read_excel("data_demand_forecast/GDPR/gdpr_nom_2019.xlsx", range = "A4:AJ39", col_names = TRUE)
  gdpr_nom_2019[2:35] <- NULL
  names(gdpr_nom_2019)[1] <- "Province"
  names(gdpr_nom_2019)[2] <- "2019"
  gdpr_nom_2019$Province[35] <- "INDONESIA"
  gdpr_nom_2019 <- pivot_longer(gdpr_nom_2019, cols = 2, names_to = "year", values_to = "gdpr_nom")


#merge data
gdpr_nom_2011_2019 <- rbind(gdpr_nom_2011, gdpr_nom_2012, gdpr_nom_2013, gdpr_nom_2014, gdpr_nom_2015,
                            gdpr_nom_2016, gdpr_nom_2017, gdpr_nom_2018, gdpr_nom_2019)

#remove all spaces, and convert comma to dot, replace "-" with 0
gdpr_nom_2011_2019[3] <- sub(",", ".", gdpr_nom_2011_2019$gdpr_nom, fixed = TRUE)
gdpr_nom_2011_2019[3] <- sub("-", "0", gdpr_nom_2011_2019$gdpr_nom, fixed = TRUE)
gdpr_nom_2011_2019    <- gdpr_nom_2011_2019 %>%  mutate(gdpr_nom = str_remove(gdpr_nom, "\\s"))
gdpr_nom_2011_2019    <- gdpr_nom_2011_2019 %>%  mutate(gdpr_nom = str_remove(gdpr_nom, "\\s"))
gdpr_nom_2011_2019    <- gdpr_nom_2011_2019 %>%  mutate(gdpr_nom = str_remove(gdpr_nom, "\\s"))



# Population data ---------------------------------------------------------
# it is dumb, no actual data were found for population between 2010-2019
# therefore, calculated from population density (person / km2) x province area (km2)
# province area
#area_province_km2 <- read_excel("data_demand_forecast/population/area_province_km2.xls", range = "A5:AP40", col_names = TRUE)
#  area_province_km2[2:41] <- NULL
#  names(area_province_km2)[1] <- "Province"
#  names(area_province_km2)[2] <- "area_km2"
#  area_province_km2$Province <- toupper(area_province_km2$Province)
#  area_province_km2$Province <- sub("KEPULAUAN", "KEP.", area_province_km2$Province, fixed = TRUE)

#province population density
#are you kidding me? 2011 and 2012 data are not available
#population_density_province_2010_2014 <- read_excel("data_demand_forecast/population/population_density_2010_2014.xlsx", range = "A2:D37", col_names = TRUE)
#  names(population_density_province_2010_2014)[1] <- "Province"
#population_density_province_2015_2019 <- read_excel("data_demand_forecast/population/population_density_2015_2019.xlsx", range = "A2:D37", col_names = TRUE)
#  names(population_density_province_2010_2014)[1] <- "Province"
# data will be taken from: http://satudata.sumselprov.go.id/v3/data/index.php?v=Kelompok-Lainnya-Pilih&q=Data-View&s=193

population_2015_2019 <- read_csv("data_demand_forecast/population/population_2015_2019.csv")
  names(population_2015_2019)[1] <- "Province"
  population_2015_2019$Province <- toupper(population_2015_2019$Province)
  population_2015_2019$Province <- sub("KEPULAUAN", "KEP.", population_2015_2019$Province, fixed = TRUE)

# Energy intensity data ---------------------------------------------------



# Electricity price data --------------------------------------------------



# Distributed electricity gWh data ----------------------------------------
# gfrom BPS, stored locally in github folder
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
dist_gwh_2011_2019 <- dist_gwh_2011_2019 %>%  mutate(dist_gwh = str_remove(dist_gwh, "\\s"))

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
gen_gwh_2011_2019 <- rbind(gen_gwh_2011_2012, gen_gwh_2013_2015, gen_gwh_2017_2019)

#remove all spaces, and convert comma to dot, replace "-" with 0
gen_gwh_2011_2019[3] <- sub(",", ".", gen_gwh_2011_2019$gen_gwh, fixed = TRUE)
gen_gwh_2011_2019[3] <- sub("-", "0", gen_gwh_2011_2019$gen_gwh, fixed = TRUE)
gen_gwh_2011_2019 <- gen_gwh_2011_2019 %>%  mutate(gen_gwh = str_remove(gen_gwh, "\\s"))


# Sectoral gWh data -------------------------------------------------------
# this section will not be included for this project, due to time limitations

# Bind all data ---------------------------------------------------------------------
historical_data_source <- merge (gdpr_2011_2019, gdpr_nom_2011_2019, by = c("Province","year"), sort = FALSE)
historical_data_source <- merge (historical_data_source, dist_gwh_2011_2019, by = c("Province","year"), sort = FALSE)
historical_data_source <- merge (historical_data_source, gen_gwh_2011_2019, by = c("Province","year"), sort = FALSE)

# add region column
#historical_data_source <- transform(historical_data_source, Region = historical_data_source$Province)
historical_data_source <- within(historical_data_source, {
  #create blank region
  Region <- NA
  #Sumatera region
  Region[Province == "ACEH"]                  <- Region_1
  Region[Province == "SUMATERA UTARA"]        <- Region_1
  Region[Province == "SUMATERA BARAT"]        <- Region_1
  Region[Province == "RIAU"]                  <- Region_1
  Region[Province == "JAMBI"]                 <- Region_1
  Region[Province == "SUMATERA SELATAN"]      <- Region_1
  Region[Province == "BENGKULU"]              <- Region_1
  Region[Province == "LAMPUNG"]               <- Region_1
  Region[Province == "KEP. BANGKA BELITUNG"]  <- Region_1
  Region[Province == "KEP. RIAU"]             <- Region_1
  #Java region
  Region[Province == "DKI JAKARTA"]   <- Region_7
  Region[Province == "JAWA BARAT"]    <- Region_7
  Region[Province == "JAWA TENGAH"]   <- Region_7
  Region[Province == "DI YOGYAKARTA"] <- Region_7
  Region[Province == "JAWA TIMUR"]    <- Region_7
  Region[Province == "BANTEN"]        <- Region_7
  #Bali & Nusa Tenggara region
  Region[Province == "BALI"]                  <- Region_6
  Region[Province == "NUSA TENGGARA BARAT"]   <- Region_6
  Region[Province == "NUSA TENGGARA TIMUR"]   <- Region_6
  #Kalimantan region
  Region[Province == "KALIMANTAN BARAT"]      <- Region_2
  Region[Province == "KALIMANTAN TENGAH"]     <- Region_2
  Region[Province == "KALIMANTAN SELATAN"]    <- Region_2
  Region[Province == "KALIMANTAN TIMUR"]      <- Region_2
  Region[Province == "KALIMANTAN UTARA"]      <- Region_2
  #Sulawesi region
  Region[Province == "SULAWESI UTARA"]        <- Region_3
  Region[Province == "SULAWESI TENGAH"]       <- Region_3
  Region[Province == "SULAWESI SELATAN"]      <- Region_3
  Region[Province == "SULAWESI TENGGARA"]     <- Region_3
  Region[Province == "GORONTALO"]             <- Region_3
  Region[Province == "SULAWESI BARAT"]        <- Region_3
  #Maluku region
  Region[Province == "MALUKU"]       <- Region_4
  Region[Province == "MALUKU UTARA"] <- Region_4
  #Papua region
  Region[Province == "PAPUA BARAT"]  <- Region_5
  Region[Province == "PAPUA"]        <- Region_5
  #National region
  Region[Province == "INDONESIA"]    <- Region_8
})


# End ---------------------------------------------------------------------
#write data
write_csv(historical_data_source, "data_demand_forecast/forecast_data_source.csv") #whole data


