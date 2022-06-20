library(tidyverse)
library(data.table)
library(microbenchmark)
library(benchmarkme)
library(igraph)
library(sf)
library(rlist)
library(dodgr)
library(cppRouting)
library(sfnetworks)
library(tidygraph)
library(tmap)
library(RcppParallel)

citation("cppRouting")

### routing benchmarks
 # dodgr vs cppRouting
 # all the other packages are way to complicated to set up...

## What needs to be checked:
# single OD, distance, path
# many to many OD
# distance matrix
# paths matrix
# different algorithms if possible. 

# loading the edges
london_edges_dt <- st_read("/Users/ivann/Desktop/CASA/RC_outputs/london_all/london_all.gpkg", layer = "edges") %>% 
  as.data.table()
london_edges_dt[, c("from","to")] <- 
  london_edges_dt[, lapply(.SD, as.character), .SDcols = c("from","to")]

london_nodes_dt <- st_read("/Users/ivann/Desktop/CASA/RC_outputs/london_all/london_all.gpkg"
                           ,layer = "nodes") %>% 
  as.data.table()
london_nodes_dt[,"osmid"] <- 
  london_nodes_dt[, lapply(.SD, as.character), .SDcols = c("osmid")]

london_edges_dt <- NULL
london_nodes_dt <- NULL

london_msoa <- list.load("/Users/ivann/Desktop/CASA/data/london_msoa.rds") %>% as.data.table()

london_msoa[,c("centr_geom","workplace_centr","pop_weight_geom","net_centr","geometry")] <-
 lapply(london_msoa[,c("centr_geom","workplace_centr","pop_weight_geom","net_centr","geometry")], FUN=as.character)

# london_msoa %>% str()

## Networks 

network_cppr <- list.load("/Users/ivann/Desktop/CASA/data/london_graph_simple.rds")

dodgr_cycle <- dodgr::weight_streetnet(london_edges_dt %>% st_as_sf()
                                       ,wt_profile = "bicycle")

dodgr_cycle %>% list.save("dodgr_cycle.rds")

dodgr_cycle <- list.load("dodgr_cycle.rds")

## 

network_tidygraph <- tbl_graph(nodes = london_nodes_dt #%>% as.data.frame()
                               ,edges = london_edges_dt #%>% as.data.frame()
                               ,directed = FALSE
                               ,node_key = "osmid")

ind_of_interest <- find_nearest_node_on_graph(network_tidygraph %N>% select(osmid,x,y) %>% as.data.frame()
                                                ,london_msoa$centr_geom)

network_tidygraph %>% 
  activate(nodes) %>% 
  filter(osmid %in% nodes_of_interest)

#
network_sf <- list.load("network_sf.rds")
# network_sf <- as_sfnetwork(network_tidygraph)
# network_sf %>% list.save("network_sf.rds")

sf_node_ind <- find_nearest_node_on_graph(network_sf %N>% select(osmid,x,y) %>% as.data.frame()
                                          ,london_msoa[,"centr_geom"] %>% st_as_sf(wkt=1,crs=4326))

net_sf_nodes <- network_sf %N>% 
  select(osmid,x,y) %>% 
  as.data.frame() %>% 
  .[sf_node_ind,"osmid"]

## dodgr setup 

from_xy <- london_msoa[,"pop_weight_geom"] %>% st_as_sf(wkt=1,crs=4326) %>% st_coordinates()

from_ind <- match_points_to_graph(v_cycle, from_xy, connected = TRUE)

###

x <- sample(1:983,50)

benchmark_packages_dist_matrix_speed <- microbenchmark(
  # igraph
  # "tidygraph" = igraph::distances(network_tidygraph
  #                              ,v = ind_of_interest[x]
  #                              ,to =  ind_of_interest[x]
  #                              ,algorithm = "dijkstra"
  #                              ,weights = network_tidygraph %>% activate(edges) %>% pull(length))
  "sf_networks" = sfnetworks::st_network_cost(network_sf
                                               ,from = sf_node_ind[x]
                                               ,to = sf_node_ind[x]
                                               ,weights = "length")
  # dodgr test routing
  ,"dists_dodgr_fheap" =  dodgr_dists(dodgr_cycle
                                      ,from = v_cycle$id[from_ind[x]]
                                      ,to = v_cycle$id[from_ind[x]]
                                      ,shortest = TRUE
                                      ,quiet = FALSE
                                      ,heap = "FHeap"
                                      ,parallel = TRUE)
  ,"dists_dodgr_bheap" =  dodgr_dists(dodgr_cycle
                                      ,from =v_cycle[from_ind[x],c("id")]
                                      ,to = v_cycle[from_ind[x],c("id")]
                                      ,shortest = TRUE
                                      ,quiet = FALSE
                                      ,heap = "BHeap"
                                      ,parallel = TRUE)
  ,"dists_dodgr_triheap" =  dodgr_dists(dodgr_cycle
                                        ,from = v_cycle$id[from_ind[x]]
                                        ,to = v_cycle$id[from_ind[x]]
                                        ,shortest = TRUE
                                        ,quiet = FALSE
                                        ,heap = "TriHeap"
                                        ,parallel = TRUE)
  # ,# cppR test
  ,"dists_cppr_phast" = cppRouting::get_distance_matrix(network_cppr
                                                       ,from = london_msoa$pw_centr_id[x]
                                                       ,to = london_msoa$pw_centr_id[x]
                                                       ,allcores = TRUE
                                                       ,algorithm = "phast")
  ,"dists_cppr_mch" = cppRouting::get_distance_matrix(network_cppr
                                                      ,from = london_msoa$pw_centr_id[x]
                                                      ,to = london_msoa$pw_centr_id[x]
                                                      ,allcores = FALSE
                                                      ,algorithm = "mch")
  ,"dists_cppr_mch_par" = cppRouting::get_distance_matrix(network_cppr
                                                          ,from = london_msoa$pw_centr_id[x]
                                                          ,to = london_msoa$pw_centr_id[x]
                                                          ,allcores = TRUE
                                                          ,algorithm = "mch")
  ,times = 10
)

print(benchmark_packages_dist_matrix_speed)

#   | expr                | min       | lq        | mean      | median    | uq        | max       | neval |
#   |---------------------|-----------|-----------|-----------|-----------|-----------|-----------|-------|
#   | sf_networks         | 11.425048 | 12.293273 | 14.103827 | 13.514490 | 15.400162 | 20.021397 | 10    |
#   | dists_dodgr_fheap   | 34.470602 | 38.371247 | 42.340785 | 41.295455 | 46.054709 | 51.192035 | 10    |
#   | dists_dodgr_bheap   | 27.029551 | 29.278896 | 31.449074 | 30.329462 | 31.925256 | 40.661944 | 10    |
#   | dists_dodgr_triheap | 38.094605 | 40.434782 | 46.844634 | 44.666436 | 51.348507 | 64.424096 | 10    |
#   | dists_cppr_phast    | 2.174485  | 2.334185  | 2.731863  | 2.362348  | 3.060203  | 3.926539  | 10    |
#   | dists_cppr_mch      | 5.752811  | 6.545932  | 7.125564  | 7.056096  | 7.686406  | 8.750525  | 10    |
#   | dists_cppr_mch_par  | 2.123374  | 2.454510  | 2.645163  | 2.624814  | 2.839143  | 3.209421  | 10    |
# 
# 
#   | expr          | mean      | threads |
#   |---------------|-----------|---------|
#   | sf_networks   | 14.103827 | 1       |
#   | dodgr_fheap   | 42.340785 | 4       |
#   | dodgr_bheap   | 31.449074 | 4       |
#   | dodgr_triheap | 46.844634 | 4       |
#   | cppr_phast    | 2.731863  | 4       |
#   | cppr_mch      | 7.125564  | 1       |
#   | cppr_mch_par  | 2.645163  | 4       |

# memory perf
profvis_sf_net <- profvis::profvis(
  expr = { sfnetworks::st_network_cost(network_sf
                                       ,from = sf_node_ind[x]
                                       ,to = sf_node_ind[x]
                                       ,weights = "length")}
)

profvis_sf_net

profvis_cppr_onecore <- profvis::profvis(
  expr = {cppRouting::get_distance_matrix(network_cppr
                                          ,from = london_msoa$pw_centr_id[x]
                                          ,to = london_msoa$pw_centr_id[x]
                                          ,allcores = FALSE
                                          ,algorithm = "mch")}
)

profvis_cppr_onecore

profvis_cppr_allcore <- profvis::profvis(
  expr = {cppRouting::get_distance_matrix(network_cppr
                                          ,from = london_msoa$pw_centr_id[x]
                                          ,to = london_msoa$pw_centr_id[x]
                                          ,allcores = TRUE
                                          ,algorithm = "mch")}
)

profvis_cppr_allcore

# checking consistency of the points routed
tmap_mode("view")
v[from_ind[1:10],] %>% st_as_sf(coords = c("x","y"), crs = 4326) %>% qtm() + 
  (london_msoa[1:10,pop_weight_geom] %>% qtm(dots.col = "red")) +
  (network_cppr$coords[match(london_msoa$pw_centr_id[1:10], node_id,),.(X,Y)] %>% 
     st_as_sf(coords = c("X","Y"),crs = 4326) %>% qtm(dots.col = "blue"))
# all good

