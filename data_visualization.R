library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(networkD3)
library(scales)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to this script's location
options(scipen = 999, digits = 2) # avoid scientific notations


# ******************************************************************************
# Demand forecasting visualizations
# ******************************************************************************
  source("demand_forecast_neural_network_all.R")
  
  demand_forecast_ind_plot <-
    nn_results[c('region','year','ref_value','predict_value')] %>% 
    subset(region == 'INDONESIA') %>% 
    pivot_longer(cols = 3:4, names_to = "type", values_to = "GWh") %>% 
    ggplot(aes(year, GWh, group = type)) +
    geom_line(aes(linetype = type, color = type)) +
    facet_grid(cols = vars(region)) +
    labs(title = "Electricity Demand Projection, RUKN vs ANN, by region",
         x = "year",
         y = "GWh")
  ggplotly(demand_forecast_ind_plot)
  
  demand_forecast_regional_plot <-
    nn_results[c('region','year','ref_value','predict_value')] %>% 
    subset(region != 'INDONESIA') %>% 
    pivot_longer(cols = 3:4, names_to = "type", values_to = "GWh") %>% 
    ggplot(aes(year, GWh, group = type)) +
    geom_line(aes(linetype = type, color = type)) +
    facet_grid(cols = vars(region)) +
    labs(title = "Electricity Demand Projection, RUKN vs ANN, by region",
         x = "year",
         y = "GWh")
  ggplotly(demand_forecast_regional_plot)
  
  
  const_nrn_plot <- 
    constraints[c('year', 'nrn_target', 'nrn_act')] %>%
    pivot_longer(cols=2:3, names_to = "type", values_to = "share") %>% 
    ggplot(aes(year, share, group = type)) +
    geom_line(aes(linetype = type, color = type)) +
    labs(title = "Target vs Actual Share from Renewable Generation",
         x = "year",
         y = "Share (%)")
  ggplotly(const_nrn_plot)
  

# ******************************************************************************
# Resource Allocation Visualizations
# ******************************************************************************


# read supply_mw data
  cap_mw       <- read_excel("data_source_resource_allocation/data_source_excel_model_macro.xlsm", sheet = "cap_mw")
  add_cap_mw   <- read_excel("data_source_resource_allocation/data_source_excel_model_macro.xlsm", sheet = "add_cap_mw")
  remain_pot   <- read_excel("data_source_resource_allocation/data_source_excel_model_macro.xlsm", sheet = "remain_pot")
  sup_gwh      <- read_excel("data_source_resource_allocation/data_source_excel_model_macro.xlsm", sheet = "sup_gwh")
  co2_ton      <- read_excel("data_source_resource_allocation/data_source_excel_model_macro.xlsm", sheet = "co2_ton")
  cost_musd    <- read_excel("data_source_resource_allocation/data_source_excel_model_macro.xlsm", sheet = "cost_musd")
  prim_pj      <- read_excel("data_source_resource_allocation/data_source_excel_model_macro.xlsm", sheet = "prim_pj")
  sup_pj       <- read_excel("data_source_resource_allocation/data_source_excel_model_macro.xlsm", sheet = "sup_pj")
  los_pj       <- read_excel("data_source_resource_allocation/data_source_excel_model_macro.xlsm", sheet = "los_pj")
  constraints  <- read_excel("data_source_resource_allocation/data_source_excel_model_macro.xlsm", sheet = "constraints")
  
  
  min_year   <- min(cap_mw$year)
  max_year   <- max(cap_mw$year)


# demand and supply -------------------------------------------------------
  
  sup_gwh$region <- factor(sup_gwh$region, levels = c("INDONESIA", "SUMATERA", "JAWA", "BALI_NT", "KALIMANTAN", "SULAWESI", "MALUKU_PAPUA"))
  demsup_plot <- sup_gwh[c('region', 'year', 'demand')] %>% 
    ggplot(aes(year, demand, colour = region)) +
    geom_line() +
    #facet_grid(cols = vars(region)) +
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
    labs(title = "Electricity Supply Resource Allocation 2019-2038, by region",
         x = "year",
         y = "GWh")
  ggplotly(sup_plot)
  
  
  sup_weight_plot <- 
    subset(sup_gwh, region != 'INDONESIA') %>% 
    subset(select = -c(total, supply, demand)) %>% 
    pivot_longer(cols= 3:15, names_to = "source", values_to = "GWh") %>% 
    group_by(region, year, source) %>% 
    summarise(n = sum(GWh)) %>% 
    mutate(percentage = round((100* n / sum(n)),2)) %>% 
    ggplot(aes(year, percentage, fill = source)) +
    geom_area() +
    facet_grid(cols = vars(region)) +
    labs(title = "Electricity Generation Mix 2019-2038, by region",
         x = "year",
         y = "share (%)")
  ggplotly(sup_weight_plot)
  

# installed capacity ------------------------------------------------------
  
  cap_plot <- 
    subset(cap_mw, region != 'INDONESIA') %>% 
    subset(select = -c(total)) %>% 
    pivot_longer(cols= 3:15, names_to = "source", values_to = "cap_mw") %>% 
    ggplot(aes(year, cap_mw, fill = source)) +
    geom_area() +
    facet_grid(cols = vars(region)) +
    labs(title = "Required Generatoin capacity 2019-2038, by region",
         x = "year",
         y = "Capacity (MW)")
  ggplotly(cap_plot)
  
  cap_weight_plot <- 
    subset(cap_mw, region != 'INDONESIA') %>% 
    subset(select = -c(total)) %>% 
    pivot_longer(cols= 3:15, names_to = "source", values_to = "cap_mw") %>% 
    group_by(region, year, source) %>% 
    summarise(n = sum(cap_mw)) %>% 
    mutate(percentage = round((100* n / sum(n)),2)) %>% 
    ggplot(aes(year, percentage, fill = source)) +
    geom_area() +
    facet_grid(cols = vars(region)) +
    labs(title = "Generation Capacity Mix 2019-2038, by region",
         x = "year",
         y = "share (%)")
  ggplotly(cap_weight_plot)
  
  add_cap_plot <- 
    subset(add_cap_mw, region != 'INDONESIA') %>% 
    subset(select = -c(total)) %>% 
    pivot_longer(cols= 3:15, names_to = "source", values_to = "cap_mw") %>% 
    ggplot(aes(year, cap_mw, fill = source)) +
    geom_area() +
    facet_grid(cols = vars(region)) +
    labs(title = "Electricity Supply Resource Allocation 2019-2038, by region",
         x = "year",
         y = "Capacity (MW)")
  ggplotly(add_cap_plot)
  


# cost (million USD) ------------------------------------------------------

  cost_plot <- 
    subset(cost_musd, region != 'INDONESIA') %>% 
    subset(select = -c(total)) %>% 
    pivot_longer(cols= 3:15, names_to = "source", values_to = "cost_musd") %>% 
    ggplot(aes(year, cost_musd, fill = source)) +
    geom_area() +
    facet_grid(cols = vars(region)) +
    labs(title = "Cost estimation for power generation, by region",
         x = "year",
         y = "Cost (Million USD)")
  ggplotly(cost_plot)
  
  cost_weight_plot <- 
    subset(cost_musd, region != 'INDONESIA') %>% 
    subset(select = -c(total)) %>% 
    pivot_longer(cols= 3:15, names_to = "source", values_to = "cost_musd") %>% 
    group_by(region, year, source) %>% 
    summarise(n = sum(cost_musd)) %>% 
    mutate(percentage = round((100* n / sum(n)),2)) %>% 
    ggplot(aes(year, percentage, fill = source)) +
    geom_area() +
    facet_grid(cols = vars(region)) +
    labs(title = "Cost structure of power generation, by region",
         x = "year",
         y = "Share(%)")
  ggplotly(cost_weight_plot)
  


# CO2 emission ------------------------------------------------------------

  co2_plot <- 
    subset(co2_ton, region != 'INDONESIA') %>% 
    subset(select = -c(total)) %>% 
    pivot_longer(cols= 3:15, names_to = "source", values_to = "co2_tonne") %>% 
    ggplot(aes(year, co2_tonne, fill = source)) +
    geom_area() +
    facet_grid(cols = vars(region)) +
    labs(title = "CO2 emission estimation from power generation 2019-2038, by region",
         x = "year",
         y = "Metric tonne CO2")
  ggplotly(co2_plot)
  
  co2_weight_plot <- 
    subset(co2_ton, region != 'INDONESIA') %>% 
    subset(select = -c(total)) %>% 
    pivot_longer(cols= 3:15, names_to = "source", values_to = "co2_tonne") %>% 
    group_by(region, year, source) %>% 
    summarise(n = sum(co2_tonne)) %>% 
    mutate(percentage = round((100* n / sum(n)),2)) %>% 
    ggplot(aes(year, percentage, fill = source)) +
    geom_area() +
    facet_grid(cols = vars(region)) +
    labs(title = "Cost structure of power generation, by region",
         x = "year",
         y = "Share(%)")
  ggplotly(co2_weight_plot)
  
  

# Constraints -------------------------------------------------------------

  # GWh target
  const_gwh_plot <- 
    constraints[c('year', 'gwh_target', 'gwh_act')] %>%
    pivot_longer(cols=2:3, names_to = "type", values_to = "GWh") %>% 
    ggplot(aes(year, GWh, group = type)) +
    geom_line(aes(linetype = type, color = type)) +
    labs(title = "Target vs Actual GWh production",
         x = "year",
         y = "Generation (GWh)")
  ggplotly(const_gwh_plot)
  
  #carbon cap
  const_co2_plot <- 
    constraints[c('year', 'carbon_cap', 'carbon_act')] %>%
    pivot_longer(cols=2:3, names_to = "type", values_to = "growth") %>% 
    ggplot(aes(year, growth, group = type)) +
    geom_line(aes(linetype = type, color = type)) +
    labs(title = "Cap vs Actual CO2 emission growth",
         x = "year",
         y = "Growth (%)")
  ggplotly(const_co2_plot)
  
  #renewable target
  const_nrn_plot <- 
    constraints[c('year', 'nrn_target', 'nrn_act')] %>%
    pivot_longer(cols=2:3, names_to = "type", values_to = "share") %>% 
    ggplot(aes(year, share, group = type)) +
    geom_line(aes(linetype = type, color = type)) +
    labs(title = "Target vs Actual Share from Renewable Generation",
         x = "year",
         y = "Share (%)")
  ggplotly(const_nrn_plot)
  
  
    

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


  
  
  
  
  
  
  