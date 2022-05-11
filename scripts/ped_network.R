#### Libs ####
# library(osmar)
# library(osmdata)

library(tmap)
library(tmaptools)
library(tidyverse)
library(data.table)
library(cppRouting)
library(foreach)
library(doParallel)
library(units)
library(reshape2)
library(rlist)
library(sf)
library(osmextract)
library(dodgr)
library(rbenchmark)
library(microbenchmark)
library(profvis)
library(jsonlite)
library(Matrix)

# library(benchmark)

# install.packages('bikedata')

# devtools::install_github ("mpadge/bikedata")

# useBasiliskEnv(path.expand("~/.cache/basilisk/1.2.1/velociraptor-1.0.0/env"))

# httr_options()
# httr::set_config(config(ssl_verifyhost = 1)
#                  ,override = TRUE)
# 
# packageVersion("PROJ")
# remotes::install_github("luukvdmeer/sfnetworks", "fix-onattach")

gc()

##### 

image_folder <- "images"
data_folder <- "data"

thames <- st_read("data/thames.geojson") %>% st_transform(4326)

#### Loading the .gpkg  ####

# readig the file for all of london
st_layers("data/london_cycle.gpkg")

london_nodes_dt <- st_read("RC_outputs/london_all/london_all.gpkg", layer = "nodes") %>% as.data.table()
london_nodes_dt[,"osmid"] <- 
  london_nodes_dt[, lapply(.SD, as.character), .SDcols = c("osmid")]

# loading the edges
london_edges_dt <- st_read("RC_outputs/london_all/london_all.gpkg", layer = "edges") %>% as.data.table()
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

aix_edges <- sf::st_read("RC_outputs/data_download_aix/data/area_custom_walk.gpkg"
                         ,layer = "edges") %>% as.data.table()
aix_nodes <- sf::st_read("RC_outputs/data_download_aix/data/area_custom_walk.gpkg"
                         ,layer = "nodes") %>% as.data.table()


tmap_mode("plot")
aix_nodes %>% qtm()


aix_graph <- aix_edges[,.(from,to,length)] %>% makegraph(directed = FALSE
                                                         ,coords = aix_nodes[,.(node_id = osmid
                                                                                ,x,y)])

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
# head(london_edges_dt,5000) %>% st_as_sf() %>% qtm()


### reding the file with all the network of london ####
# 
# sf::st_layers("data/london_OSM/London_OSM.shp")
# 
# london_osm <- sf::st_read("data/london_OSM/London_OSM.shp"
#                           )
# 
# london_osm %>% size
# 
# #### exploratory description of the network ####
# 
# london_osm <- london_osm %>% 
#   as.data.table() 
# 
# 
# # comparing perf
# # data table seems faster for this purpose
# tidy_vs_dt_length <- 
#   benchmark("dt" = london_osm_dt[1:10000,set_units(st_length(geometry),NULL)]
#             ,"tidy" = london_osm[1:10000,] %>% 
#               mutate(length = set_units(st_length(geometry),NULL))
#             ,replications = 10)
# 
# london_osm[,length := set_units(st_length(st_transform(geometry,27700)),NULL)]
# 
# london_osm_simple <- 
#   london_osm[,.(st_transform(geometry,27700))][,st_coordinates(V1)] %>% 
#   as_tibble() %>%
#   group_by(L1) %>%
#   summarise(res = cbind(X[1],Y[1],X[n()],Y[n()])) %>%
#   as.data.table()
# 
# london_osm_simple[,length := london_osm[,length]]
# 
# setnames(london_osm_simple
#          ,old = c("res.V1","res.V2","res.V3","res.V4")
#          ,new = c("X1","Y1","X2","Y2")
#          )
# 
# # done in the script routing.R
# london_graph <- makegraph(london_edges_dt[,.(from,to,length)]
#                           ,directed = FALSE
#                           ,coords = london_nodes_dt[,.(node_id = osmid
#                                                        ,X = x
#                                                        ,Y = y)])
# 
# london_graph_simple <- london_graph %>%
#   cppRouting::cpp_simplify(rm_loop = TRUE
#                            ,iterate = FALSE)

A_rotation <- function(theta = 30) {
  matrix(data = c(cos(theta),-sin(theta),sin(theta),cos(theta)),byrow = TRUE,nrow = 2,ncol = 2)
}

A_compression <- function(x = 1, y = .5) {
  matrix(data = c(x,0,0,y),byrow = FALSE, nrow = 2, ncol = 2)
}

A_translation <- function(data,x = 0, y = 100) {
  cbind(data[,1] + x,data[,2]  + y)
}

data_rotation <- function(data,comp = a_c,rot = a_r) {
  (data %>% st_sfc() %>%
     st_geometry()) * a_r * a_c
}

tmap_mode("view")
# old_coord[,.SD,by = L3,.SDcols = c("X","Y")]

a_r <- A_rotation(pi/6)
a_c <- A_compression(3,1)

thames_new_coord <- sapply(thames_poly$geometry,data_rotation,simplify = TRUE) %>% st_sfc(crs = 4326)

london_msoa_rot <- sapply(london_msoa$geometry,data_rotation,simplify = TRUE) %>% st_sfc(crs = 4326)

path_sf_rot <- sapply(paths_sf,data_rotation,simplify = TRUE) %>% st_sfc(crs = 4326)

?plot.sf

london_msoa_rot %>% qtm(fill = "white"
                        ,borders = "dimgray"
                        ) +
  thames_new_coord %>% qtm(fill = "darkblue"
                           ,borders = "darkblue"
  ) + 
  path_sf_rot %>% qtm(lines.lwd = .5
                      ,lines.col = "darkred"
  )

for(i in 1:max(new_coord_2[,3])
        # ,.final = st_sfc
        # ,crs = 27700
    ) {
  lines(new_coord_2[V3 == i
                   ,cbind(V1,V2)]
        ,col = "darkred"
        ,lwd = 0.5)
}

old_coord_road <- london_edges_dt[grep("motorway",highway),"geom"][,st_transform(geom,27700)] %>% st_coordinates(geom) 
  
new_coord_road <- old_coord_road[,c(1,2)] %>% apply(MARGIN = 1, FUN = function(v) a_r%*%a_c%*%v) %>% 
  t() %>% A_translation(x = 0,y = 0)

plot(old_coord_road,col = "red")

# test[,get_lines(as.matrix(.(V1,V2),ncol=2,byrow = FALSE)),by = .(V3)]


#### ####

thames_poly <- st_read("data/greater-london-latest-free/gis_osm_water_a_free_1.shp") %>% as.data.table()

thames_poly <- thames_poly[fclass=='riverbank' | name=='River Thames',]

thames_poly %>% st_as_sf(crs = 4326) %>% qtm(fill = "blue")

#### Experinment with road degree ####

# find the angle of the road in a polar coordinates 
# reference frame on a tangent plane to earth. 
# for that purpose the segments are simplified and only 
# the start and end point are kept
# atan returns the value in radians.
# acos is better because it does not return NA
# when theta is pi/2

london_osm_simple[,theta := acos((X2-X1)/length)]

london_osm_simple[,summary(theta)]

# 
# nodes_test <- rbind(london_osm_simple[,.(x = X1,y = Y1)]
#                     ,london_osm_simple[,.(x = X2,y = Y2)]) %>% 
#   unique()
# 
# # giving ids to the nodes
# nodes_test <- nodes_test[,.N,by = .(x,y)][,.(Node_id = seq_along(N),x,y)]
# 
# # setkey(nodes_test,x,y)
# 
# from <- 
#   london_osm_simple[,map2_int(X1,Y1,.f = function(x1,y1) nodes_test[x == x1 & y ==y1,Node_id])]
# 
# # to <- 
#   london_osm_simple[1:100,map2_int(X2,Y2,.f = function(a,b) nodes_test[(x == a) & (y == b),Node_id])]
# 
# london_osm_simple[,"from"] <- from
# london_osm_simple[,"to"] <- to
# 
# london_osm_graph <- london_osm_simple[,.(from,to,length)] %>% 
#   makegraph(directed = FALSE
#             ,coords = nodes_test)
# 
# ind_from <- sample(nodes_test$id,100)
# ind_to <- sample(nodes_test$id,100)
# 
# test_paths <- get_path_pair(london_osm_graph
#                             ,from = london_osm_graph$coords$id[ind_from]
#                             ,to = london_osm_graph$coords$id[ind_to]
#                             ,long = FALSE
#                             )

theta_dens <- london_osm_simple[,density(theta)]

theta_weight_dens <- london_osm_simple[,density(theta
                                                ,weights = length/sum(length))]

pdf("images/orientation_distribution.pdf"
    ,height = 5.83
    ,width = 8.27)
plot(theta_weight_dens
     ,main = paste0("Degree orientation distribution of roads")
     ,sub = "London"
     ,xlab = "degreer, rad"
     ,col = "darkblue"
     ,xlim = c(0,3.14)
     ,lwd = 2)
lines(theta_dens
      ,col = "darkred"
      ,lwd = 2)
legend("topright"
       ,legend = c("length weighted", "unweighted")
       ,col = c("darkblue","darkred")
       ,lwd = 2)
dev.off()
 
#### Learning R ####

a <- pi

call("round",a)

print_a <- function(x) print(x)

print_a(b)
a <- 5
print_a(b)

london_msoa <- london_msoa %>% st_as_sf()

column <- c("graph_id", "geo_name")

london_msoa[,column]
#### 

city_data <- city_data %>% st_as_sf(sf_column_name = "pop_weight_geom")

centroid_distance_1 <- sapply(st_geometry(city_data), function(t) {t %>% 
    st_sfc(crs = 4326) %>% 
    norm_p(st_geometry(city_data),p = 1)}) %>%
  t()

centroid_distance_2 <- lapply(st_geometry(city_data), function(t) t %>% st_sfc(crs = 4326) %>% norm_p(st_geometry(city_data),p = 1)) %>%
  unlist %>% 
  matrix(ncol = nrow(city_data), byrow = FALSE)

identical(centroid_distance_1,(centroid_distance_2))

#### READING THE BIG geofabrik file ####
# 
# sf::st_layers("data/ukraine-latest.osm.pbf")
# 
# ukraine_file <-  oe_read("data/ukraine-latest.osm.pbf"
#                     ,layer = "lines"
#                     ,max_file_size = 1e+4)

#### Testing data import with reticulte ####

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

