#### Libs ####
library(osmar)
library(osmdata)
library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(httr)
library(igraph)
library(tidygraph)
library(PROJ)
library(sfnetworks)
library(data.table)
library(pct)
library(dodgr)
library(foreach)
library(profvis)
library(ggplot2)
library(multiplex)

# useBasiliskEnv(path.expand("~/.cache/basilisk/1.2.1/velociraptor-1.0.0/env"))

# httr_options()
# httr::set_config(config(ssl_verifyhost = 1)
#                  ,override = TRUE)

packageVersion("PROJ")
remotes::install_github("luukvdmeer/sfnetworks", "fix-onattach")



##### 

image_folder <- "images"
data_folder <- "data"

#### Loading the .gpkg  ####

# readig the file for all of london
st_layers("data/london_cycle.gpkg")

london_nodes_dt <- st_read("data/london_cycle.gpkg", layer = "nodes") %>% as.data.table()
london_edges_dt <- st_read("data/london_cycle.gpkg", layer = "edges") %>% as.data.table()

london_nodes_dt[,"osmid"] <- 
  london_nodes_dt[, lapply(.SD, as.character), .SDcols = c("osmid")]

london_edges_dt[, c("from","to")] <- 
  london_edges_dt[, lapply(.SD, as.character), .SDcols = c("from","to")]

## Reading files for brithgon

brighton_edges_dt <- st_read("data/brighton_cycle.gpkg", layer = "edges") %>% as.data.table()
brighton_nodes_dt <- st_read("data/brighton_cycle.gpkg", layer = "nodes") %>% as.data.table()

brighton_nodes_dt[,"osmid"] <- 
  brighton_nodes_dt[, lapply(.SD, as.character), .SDcols = c("osmid")]

brighton_edges_dt[, c("from","to")] <- 
  brighton_edges_dt[, lapply(.SD, as.character), .SDcols = c("from","to")]

brighton_edges_dt %>% st_as_sf()

# summary statistics of london cycle network from osmnx ####

london_nodes_dt %>% colnames()
london_edges_dt %>% colnames()

tmap_mode("view")
head(london_edges_dt,5000) %>% st_as_sf() %>% qtm()

#### Using data tables and sfnetworks to generate the network ####
## attempt to use data.tables

# did not run yet
london_network <- sfnetworks::sfnetwork(nodes = london_nodes_dt
                                        ,edges = london_edges_dt
                                        ,node_key = "osmid"
                                        ,directed = FALSE
                                        ,edges_as_lines = TRUE
                                        )

# with tbl graph 

london_tibble_network <- tbl_graph(nodes = london_nodes_dt
                                   ,edges = london_edges_dt
                                   ,node_key = "osmid")


edges <- NULL

ggplot() +
  geom_sf(data = london_tibble_network %>% activate(edges) %>% as_tibble() %>% st_as_sf())

#### 

# Next steps:
#   summary stats : density of nodes and length of streets per msoa
#   msoa centroids and routing between them




