library(readxl)

# generation data (GWh) from BPS, stored locally in github folder
generation_gwh_2011_2012 <- read_excel("data_demand_forecast/generation_gwh_2011_2012.xlsx")
generation_gwh_2013_2015 <- read_excel("data_demand_forecast/generation_gwh_2013_2015.xlsx")
generation_gwh_2017_2019 <- read_excel("data_demand_forecast/generation_gwh_2013_2015.xlsx")
View(generation_gwh_2011_2012)
View(generation_gwh_2013_2015)
View(generation_gwh_2017_2019)

