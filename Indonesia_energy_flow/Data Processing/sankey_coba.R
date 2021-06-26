library(readxl)
library(networkD3)

library(readxl)
links2 <- read_excel("sankey_coba.xlsx", sheet = "links")
nodes2 <- read_excel("sankey_coba.xlsx", sheet = "nodes")

sankeyNetwork(Links = links2, Nodes = nodes2, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             fontFamily = "sans-serif", iterations = 0)



