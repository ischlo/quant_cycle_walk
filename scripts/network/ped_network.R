#### Libs ####
# library(osmar)
# library(osmdata)
library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
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
library(wellknown)


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

london_nodes_dt <- st_read("../data/london_cycle.gpkg", layer = "nodes") %>% as.data.table()
london_nodes_dt[,"osmid"] <- 
  london_nodes_dt[, lapply(.SD, as.character), .SDcols = c("osmid")]

# loading the edges
london_edges_dt <- st_read("../data/london_cycle.gpkg", layer = "edges") %>% as.data.table()
london_edges_dt[, c("from","to")] <- 
  london_edges_dt[, lapply(.SD, as.character), .SDcols = c("from","to")]

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

msoa_wcent_geom <- 
  st_read("/Users/ivann/Desktop/CASA/data/msoa_wcent_geom/msoa_wcent_geom.shp")

london_centroid_geom <- 
  st_read("/Users/ivann/Desktop/CASA/data/london_centroid_geom/london_centroid_geom.shp")

sample_node_within_geom <- 
  st_read("/Users/ivann/Desktop/CASA/data/sample_node_within_geom/sample_node_within_geom.shp")

tmap_mode("view")
qtm(msoa_wcent_geom) + 
  qtm(london_centroid_geom
      ,dots.col = "red") +
  qtm(sample_node_within_geom
      ,dots.col = "lightgreen")

# comparing with other centroids
st_distance(
  london_msoa[,"pop_weight_geom"]
  ,st_geometry(msoa_wcent_geom)
  ,by_element = TRUE) %>% set_units(NULL) %>% hist()

# centroids in the network
network_centroid <- sample_node_within_geom %>% 
  dplyr::group_by(geo_code) %>% 
  summarise(net_centr = geometry %>% st_combine() %>% st_centroid())

# creating the distance values for intra borough flows. 

typ_dist_intra_msoa <- sample_node_within_geom %>% 
  dplyr::group_by(geo_code) %>% 
  summarise(tip_dist = mean(st_distance(geometry)))

typ_dist_intra_msoa <- typ_dist_intra_msoa %>% 
  mutate(tip_dist = tip_dist %>% set_units(NULL))

# sqrt of the area of an msoa as alternative tip distances
tip_dist <- london_msoa$geometry %>% st_area() %>% sqrt %>% set_units(NULL)

plot(tip_dist,typ_dist_intra_msoa$tip_dist
     ,log = "xy")
lines(1:4000,1:4000)

tmap_mode("view")
london_msoa$geometry %>% qtm(fill.alpha = .5) + network_centroid %>% qtm

# adding the column with the typical distances to the msoa data set

london_msoa$intra_dist <- typ_dist_intra_msoa$tip_dist

london_msoa$net_centr <- st_geometry(network_centroid)

london_msoa <- list.load("data/london_msoa.rds")

london_msoa[,c("geometry","centr_geom","pop_weight_geom","workplace_centr","net_centr")] <- 
  london_msoa[,c("geometry","centr_geom","pop_weight_geom","workplace_centr","net_centr")] %>% 
  sapply(sf_convert) %>% as.data.frame()

#### 
tmap_mode("view")

london_msoa[,c("geo_name","net_centr")] %>% st_as_sf(wkt = "net_centr",crs = 4326) %>% qtm()


#### workplace zones centroids ####

workplace_zones$msoa <- pred %>% as.numeric() 

workplace_centr <- workplace_zones %>% 
  st_as_sf(crs = 4326) %>% 
  st_transform(27700) %>% 
  dplyr::group_by(msoa) %>% 
  summarise(workplace_centr = st_centroid(st_combine(geometry))) %>% 
  st_transform(4326)

london_msoa$workplace_centr <- workplace_centr$workplace_centr %>% wellknown::sf_convert()

node_id <- find_nearest_node_on_graph(london_graph_simple
                           ,london_msoa[,c("geo_name","workplace_centr")] %>% st_as_sf(wkt="workplace_centr"
                                                                                       ,crs = 4326))

london_msoa$workplace_centr_id <- node_id$node_id

london_msoa %>% list.save("data/london_msoa.rds")

# london_msoa[,c("geo_name","workplace_centr")] %>% st_as_sf(wkt = "workplace_centr",crs = 4326) %>% qtm() +
#   (london_msoa %>% st_as_sf(wkt = "centr_geom",crs = 4326) %>% qtm(dots.col = "red")) +
#   (london_msoa %>% st_as_sf(wkt = "pop_weight_geom",crs = 4326) %>% qtm(dots.col = "blue")) +
#   (london_msoa %>% st_as_sf(wkt = "net_centr",crs = 4326) %>% qtm(dots.col = "green"))

#### local init of gravity model run ####
# 
graph <- list.load("../data/london_graph_simple.rds")

city <- list.load("../data/london_msoa.rds") %>%
  as.data.table() # %>% st_as_sf(crs = 4326)

# geom_centr <- st_read("../data/london_centr_geom.geojson") %>% st_as_sf(crs = 4326)

# pw_centr <- st_read("../data/london_pw_centr.geojson") %>% st_as_sf(crs = 4326)

flows <- list.load("../data/flows_london.rds") %>%
  as.data.table()

graph_dist_matrix <- list.load("/Users/ivann/Desktop/CASA/data/dist_matrix_london.rds")

city <- city[grepl("Camden",city$geo_name) |
               grepl("Islington",city$geo_name) |
               grepl("City of London",city$geo_name) #|
             # grepl("Westminster",city$geo_name) |
             # grepl("Southwark",city$geo_name) |
             # grepl("Hackney",city$geo_name) |
             # grepl("Haringey", city$geo_name)
             ,]

city[,"id"] <- 1:nrow(city)

flows_london_sample <- flows[(workplace %in% city$geo_code) &
                               (residence %in% city$geo_code),-c("from_id","to_id")]

flows_london_sample <- flows_london_sample %>% merge(city[,c("geo_code","id")] #%>% st_drop_geometry()
                                                     ,by.x = "residence"
                                                     ,by.y = "geo_code")

flows_london_sample <- flows_london_sample %>% merge(city[,c("geo_code","id")] #%>% st_drop_geometry()
                                                     ,by.x = "workplace"
                                                     ,by.y = "geo_code")

setnames(flows_london_sample
         ,old = c("id.x","id.y")
         ,new = c("from_id","to_id")
)

flows_mat <- foreach(i = city[,id]
                     ,.combine = rbind
                     ,.final = as.matrix) %do%
  {
    x <- rep_len(0,nrow(city))
    d <- flows_london_sample[from_id == i,.(bike,to_id)] # flows
    x[d[,to_id]] <- d[,bike]
    x
  }
# 
# flows_mat %>% dim
# 
cores <- 2
# 
# print("flows matrix computed locally")
# 
# # view(flows_matrix)
# 
# # test of the function is successful.
# print(" finished all the preliminary steps, starting the simulation ")
# 
# # #### OSM, norm1, pop_weight ####
# #
# run <- "osm_unfilt_norm1"
# 
graph = graph
graph_dist_matrix = NULL#graph_dist_matrix
flows_matrix = flows_mat
region_data = city %>% as.data.table()
from = "pop_weight_geom"
to = "workplace_centr"
norm = 2
time = FALSE
run_name = "osm_unfilt_norm1"
n_cores = cores


#### smaller networks ####
#
london_edges_dt[,highway:=as.character(highway)]

london_edges_dt[!grepl("motorway",highway),]


#### Nearest points algorithm test ####

p_1

p_2

#### ####

a <- matrix(data = c(1,1,1
                     ,2,2,2
                     ,3,3,3)
            ,byrow = TRUE
            ,ncol = 3)

a %>% typeof()

apply(a,MARGIN = 2,FUN = sum)


