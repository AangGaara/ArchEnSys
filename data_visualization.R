library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(networkD3)

setwd("C:/Users/Brajamusthi/OneDrive/Master of Energy Research/Github/ArchEnSys")


# read supply_mw data
  cap_mw     <- read_excel("data_source/data_source_excel_model_macro.xlsm", sheet = "cap_mw")
  remain_pot <- read_excel("data_source/data_source_excel_model_macro.xlsm", sheet = "remain_pot")
  sup_gwh    <- read_excel("data_source/data_source_excel_model_macro.xlsm", sheet = "sup_gwh")
  co2_ton    <- read_excel("data_source/data_source_excel_model_macro.xlsm", sheet = "co2_ton")
  cost_musd  <- read_excel("data_source/data_source_excel_model_macro.xlsm", sheet = "cost_musd")
  prim_pj    <- read_excel("data_source/data_source_excel_model_macro.xlsm", sheet = "prim_pj")
  sup_pj     <- read_excel("data_source/data_source_excel_model_macro.xlsm", sheet = "sup_pj")
  los_pj     <- read_excel("data_source/data_source_excel_model_macro.xlsm", sheet = "los_pj")

  min_year   <- min(cap_mw$year)
  max_year   <- max(cap_mw$year)


# demand and supply -------------------------------------------------------
  
  demsup_plot <- pivot_longer(sup_gwh[c('region', 'year', 'demand', 'supply')], 
                              cols = 3:4, 
                              names_to = "var",
                              values_to = "GWh") %>% 
    ggplot(aes(year, GWh, colour = var)) +
    geom_line() +
    facet_grid(cols = vars(region)) +
    labs(title = "Electricity Demand & Supply Projection 2019-2038, by region",
         x = "year",
         y = "GWh")
  ggplotly(demsup_plot)
  
  

# supply ------------------------------------------------------------------
  #demand <- NULL
  #exclude Indonesia

  sup_plot <- 
    subset(sup_gwh, region != 'INDONESIA') %>% 
    subset(select = -c(total, supply, demand)) %>% 
    pivot_longer(cols= 3:15, names_to = "source", values_to = "GWh") %>% 
    ggplot(aes(year, GWh, fill = source)) +
    geom_area() +
    facet_grid(cols = vars(region)) +
    labs(title = "Electricity Supply Ressource Allocation 2019-2038, by region",
         x = "year",
         y = "GWh")
  ggplotly(sup_plot)


# installed capacity ------------------------------------------------------
  
  cap_plot <- 
    subset(cap_mw, region != 'INDONESIA') %>% 
    subset(select = -c(total)) %>% 
    pivot_longer(cols= 3:15, names_to = "source", values_to = "cap_mw") %>% 
    ggplot(aes(year, cap_mw, fill = source)) +
    geom_area() +
    facet_grid(cols = vars(region)) +
    labs(title = "Electricity Supply Ressource Allocation 2019-2038, by region",
         x = "year",
         y = "MW")
  ggplotly(cap_plot)


# cost (million USD) ------------------------------------------------------

  cost_plot <- 
    subset(cap_mw, region != 'INDONESIA') %>% 
    subset(select = -c(total)) %>% 
    pivot_longer(cols= 3:15, names_to = "source", values_to = "cost_musd") %>% 
    ggplot(aes(year, cost_musd, fill = source)) +
    geom_area() +
    facet_grid(cols = vars(region)) +
    labs(title = "Electricity Supply Ressource Allocation 2019-2038, by region",
         x = "year",
         y = "Million USD")
  ggplotly(cost_plot)


# CO2 emission ------------------------------------------------------------

  co2_plot <- 
    subset(cap_mw, region != 'INDONESIA') %>% 
    subset(select = -c(total)) %>% 
    pivot_longer(cols= 3:15, names_to = "source", values_to = "co2_mton") %>% 
    ggplot(aes(year, co2_mton, fill = source)) +
    geom_area() +
    facet_grid(cols = vars(region)) +
    labs(title = "Electricity Supply Ressource Allocation 2019-2038, by region",
         x = "year",
         y = "Metric ton CO2")
  ggplotly(co2_plot)
  


# Sankey - potential & installed capacity ---------------------------------
  
  
  
  links2 <- read_excel("indonesia_electricity_flow.xlsx", 
                       sheet = "links", range = "A1:D265")
  nodes2 <- read_excel("indonesia_electricity_flow.xlsx", 
                       sheet = "nodes", range = "A1:B61")
  
  sankeyNetwork(Links = links2, Nodes = nodes2, Source = "source",
                Target = "target", Value = "value", NodeID = "Nodes",
                fontFamily = "sans-serif", iterations = 1000, units = "MW",
                fontSize = 10, nodeWidth = 10)
  

# Sankey - primary energy, generation, and supply ------------------------


  
  
  
  
  
  
  