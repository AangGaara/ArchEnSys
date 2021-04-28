library(readxl)
library(tidyverse)

# generation data (GWh) from BPS, stored locally in github folder
# read-transpose-merge-transpose
gwh_province  <- read_excel("data_demand_forecast/gWh/generation_gwh_2011_2012.xlsx", range = "A2:A37", col_names = FALSE)
gwh_2011_2012 <- read_excel("data_demand_forecast/gWh/generation_gwh_2011_2012.xlsx", range = "B2:C37", col_names = FALSE)
gwh_2013_2015 <- read_excel("data_demand_forecast/gWh/generation_gwh_2013_2015.xlsx", range = "B2:D37", col_names = FALSE)
gwh_2017_2019 <- read_excel("data_demand_forecast/gWh/generation_gwh_2013_2015.xlsx", range = "B2:D37", col_names = FALSE)
#gwh_province  <- t(read_excel("data_demand_forecast/gWh/generation_gwh_2011_2012.xlsx", range = "A2:A37", col_names = FALSE))
#gwh_2011_2012 <- t(read_excel("data_demand_forecast/gWh/generation_gwh_2011_2012.xlsx", range = "B2:C37", col_names = FALSE))
#gwh_2013_2015 <- t(read_excel("data_demand_forecast/gWh/generation_gwh_2013_2015.xlsx", range = "B2:D37", col_names = FALSE))
#gwh_2017_2019 <- t(read_excel("data_demand_forecast/gWh/generation_gwh_2013_2015.xlsx", range = "B2:D37", col_names = FALSE))

merge
write.csv(gwh_province, "data_demand_forecast/gWh_csv/province.csv", row.names = FALSE, col.names = FALSE)
write.csv(gwh_2011_2012, "data_demand_forecast/gWh_csv/gwh_2011_2012.csv", row.names = FALSE, col.names = FALSE)
write.csv(gwh_2013_2015, "data_demand_forecast/gWh_csv/gwh_2013_2015.csv", row.names = FALSE, col.names = FALSE)
write.csv(gwh_2017_2019, "data_demand_forecast/gWh_csv/gwh_2017_2019.csv", row.names = FALSE, col.names = FALSE)
#view(gwh_2011_2012)
#View(gwh_2013_2015)
#View(gwh_2017_2019)

#merge files into one
gwh_list <- dir("data_demand_forecast/gWh_csv", full.names=T) %>% map_df(read_csv)


rm(list = ls())
library(readxl)
library(tidyr)

setwd("C:/Users/Brajamusthi/OneDrive/Master of Energy Research/Github/ArchEnSys/data_demand_forecast/gWh_csv")

my_files <-list.files(pattern="*.csv")
my_files
nba = lapply(my_files, function(i){
  x = read_csv(i)
  x$file=i
  x
})
nba[[2]]
nba = do.call("rbind.data.frame",nba)
