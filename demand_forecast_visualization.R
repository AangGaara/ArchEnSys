library(tidyverse)
library(ggplot2)
library(highcharter)
library(dplyr)

#setwd("C:/Users/Brajamusthi/OneDrive/Master of Energy Research/Github/ArchEnSys")
#source("demand_forecast_data_source.R")

#remove indonesia data
vis_dataset <- historical_data_source[historical_data_source$Province !="INDONESIA",]
vis_dataset <- vis_dataset[order(vis_dataset$dist_gwh),] #order by distributed electricity

#remove unnecessary predictors
#gdpr_percap_thousand_idr, gdpr_growth, gen_gwh, gWh_cons_percap, capacity_mw, intensity_biased
vis_dataset <- vis_dataset[]


#summarize data based on region


#plot
p1 <- ggplot(vis_dataset, 
             aes(year, dist_gwh, colour = Province)) +
  geom_line()
p1 + xlim(2011,2019)

highchart() %>% 
# hc_add_series(data = vis_dataset$year) %>% 
  hc_add_series(data = vis_dataset$gdpr_billion_idr) %>% 
  hc_add_series(data = vis_dataset$population) %>% 
  hc_add_series(data = vis_dataset$dist_gwh) %>% 
  hc_yAxis_multiples(
#    list(lineWidth = 3, lineColor='#7cb5ec', title=list(text="First y-axis")),
    list(lineWidth = 2, lineColor="#434348", title=list(text=names(vis_dataset)[3])),
    list(lineWidth = 2, lineColor="#90ed7d", title=list(text=names(vis_dataset)[6])),
    list(lineWidth = 2, lineColor="#f7a35c", title=list(text=names(vis_dataset)[8]))
  )

