#### Libs ####
# library(osmar)
# library(osmdata)
library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
# library(httr)
library(igraph)
library(tidygraph)
library(PROJ)
library(sfnetworks)
library(data.table)
library(pct)
library(dodgr)
library(foreach)
library(doParallel)
library(profvis)
library(ggplot2)
library(rbenchmark)
library(RcppParallel)
library(cppRouting)
library(units)
library(reshape2)
library(rlist)
library(bikedata)
library(extrafont)
library(units)

font_import()
# Show the full list
fonts()

options(max.print = 50)

# install.packages('bikedata')

# devtools::install_github ("mpadge/bikedata")

# useBasiliskEnv(path.expand("~/.cache/basilisk/1.2.1/velociraptor-1.0.0/env"))

# httr_options()
# httr::set_config(config(ssl_verifyhost = 1)
#                  ,override = TRUE)

packageVersion("PROJ")
remotes::install_github("luukvdmeer/sfnetworks", "fix-onattach")

##### 

image_folder <- "images"
data_folder <- "data"

thames <- st_read("data/thames.geojson") %>% st_transform(4326)

#### Loading the .gpkg  ####

# readig the file for all of london
st_layers("data/london_cycle.gpkg")

london_nodes_dt <- st_read("data/london_cycle.gpkg", layer = "nodes") %>% as.data.table()
london_nodes_dt[,"osmid"] <- 
  london_nodes_dt[, lapply(.SD, as.character), .SDcols = c("osmid")]

# loading the edges
london_edges_dt <- st_read("data/london_cycle.gpkg", layer = "edges") %>% as.data.table()
london_edges_dt[, c("from","to")] <- 
  london_edges_dt[, lapply(.SD, as.character), .SDcols = c("from","to")]



## Reading files for brithgon
brighton_edges_dt <- st_read("data/brighton_cycle.gpkg", layer = "edges") %>% as.data.table()
brighton_edges_dt[, c("from","to")] <- 
  brighton_edges_dt[, lapply(.SD, as.character), .SDcols = c("from","to")]


brighton_nodes_dt <- st_read("data/brighton_cycle.gpkg", layer = "nodes") %>% as.data.table()

brighton_nodes_dt[,"osmid"] <- 
  brighton_nodes_dt[, lapply(.SD, as.character), .SDcols = c("osmid")]

brighton_edges_dt %>% st_as_sf()

# brighton_network <- sfnetworks::sfnetwork(nodes = brighton_nodes_dt
#                                           ,edges = brighton_edges_dt
#                                           ,node_key = "osmid"
#                                           ,directed = FALSE
#                                           ,edges_as_lines = TRUE
#                                           )

# summary statistics of london cycle network from osmnx ####

london_nodes_dt %>%
  colnames()
london_edges_dt %>% 
  colnames()

tmap_mode("view")
head(london_edges_dt,5000) %>% st_as_sf() %>% qtm()

#### Using data tables and sfnetworks to generate the network ####
## attempt to use data.tables

# did not run yet
# london_network <- sfnetworks::sfnetwork(nodes = london_nodes_dt
#                                         ,edges = london_edges_dt
#                                         ,node_key = "osmid"
#                                         ,directed = FALSE
#                                         ,edges_as_lines = TRUE
#                                         )

# with tbl graph 

# london_tibble_network <- tbl_graph(nodes = london_nodes_dt
#                                    ,edges = london_edges_dt
#                                    ,node_key = "osmid")

#### London Network
# Next steps : 
#   summary stats : density of nodes and length of streets per msoa
#   msoa centroids and routing between them

