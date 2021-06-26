library(readxl)
library(networkD3)

links2 <- read_excel("indonesia_electricity_flow.xlsx", 
                     sheet = "links", range = "A1:D265")
nodes2 <- read_excel("indonesia_electricity_flow.xlsx", 
                     sheet = "nodes", range = "A1:B61")

sankeyNetwork(Links = links2, Nodes = nodes2, Source = "source",
              Target = "target", Value = "value", NodeID = "Nodes",
              fontFamily = "sans-serif", iterations = 1000, units = "MW",
              fontSize = 10, nodeWidth = 10)
