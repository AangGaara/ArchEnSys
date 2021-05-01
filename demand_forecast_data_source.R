library(readxl)
library(tidyverse)
setwd("C:/Users/Brajamusthi/OneDrive/Master of Energy Research/Github/ArchEnSys")


# GDP data ----------------------------------------------------------------


# Population data ---------------------------------------------------------



# Energy intensity data ---------------------------------------------------


# Electricity price data --------------------------------------------------


# Total (non-sectoral) gWh data ----------------------------------------------------------------

# generation data (GWh) from BPS, stored locally in github folder
gwh_2011_2012 <- read_excel("data_demand_forecast/gWh/generation_gwh_2011_2012.xlsx", range = "A2:C37", col_names = TRUE)
names(gwh_2011_2012)[1] <- col1
gwh_2013_2015 <- read_excel("data_demand_forecast/gWh/generation_gwh_2013_2015.xlsx", range = "A2:D37", col_names = TRUE)
names(gwh_2013_2015)[1] <- col1
gwh_2017_2019 <- read_excel("data_demand_forecast/gWh/generation_gwh_2017_2019.xlsx", range = "A2:D37", col_names = TRUE)
names(gwh_2017_2019)[1] <- col1

#create long pivoted data
gwh_2011_2012_long <- pivot_longer(gwh_2011_2012, cols = 2:3, names_to = "year", values_to = "gen_gwh")
gwh_2013_2015_long <- pivot_longer(gwh_2013_2015, cols = 2:4, names_to = "year", values_to = "gen_gwh")
gwh_2017_2019_long <- pivot_longer(gwh_2017_2019, cols = 2:4, names_to = "year", values_to = "gen_gwh")

#merge all data
gwh_2011_2019 <- rbind(gwh_2011_2012_long,gwh_2013_2015_long,gwh_2017_2019_long)

#remove all spaces, and convert comma to dot, replace "-" with NA
gwh_2011_2019[3] <- sub(",", ".", gwh_2011_2019$gen_gwh, fixed = TRUE)
gwh_2011_2019[3] <- sub("-", "NA", gwh_2011_2019$gen_gwh, fixed = TRUE)
gwh_2011_2019 <- gwh_2011_2019 %>%  mutate(gen_gwh = str_remove(gen_gwh, "\\s"))
gwh_2011_2019[3]

#write csv
write_csv(gwh_2011_2019, "data_demand_forecast/gwh_2011_2019.csv")



# Sectoral gWh data -------------------------------------------------------


# End ---------------------------------------------------------------------

