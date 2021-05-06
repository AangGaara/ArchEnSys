#libraries
library(readxl)
library(tidyverse)
library(tidyr)
library(data.table)

#work directory
setwd("C:/Users/Brajamusthi/OneDrive/Master of Energy Research/Github/ArchEnSys")

#important variables
base_year = 2011
end_year = 2019
forecast_year = 2050

#open the mother data
forecast_data <- as_tibble(read.csv("data_demand_forecast/data_demand.csv")) #data format set as a tidyverse tibble

