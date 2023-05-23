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
library(units)

# citation("cppRouting")

### routing benchmarks
 # dodgr,sf_networks,tidygraph, cppRouting
 # all the other packages are to complicated to set up...

## What needs to be checked:
# distance matrix
# different algorithms if possible. 

# loading the edges
london_edges_dt <- st_read("/Users/ivannschlosser/Documents/CASA/RC_outputs/london_all/london_all.gpkg", layer = "edges") %>% 
  as.data.table()
london_edges_dt[, c("from","to")] <- 
  london_edges_dt[, lapply(.SD, as.character), .SDcols = c("from","to")]

london_nodes_dt <- st_read("/Users/ivannschlosser/Documents/CASA/RC_outputs/london_all/london_all.gpkg"
                           ,layer = "nodes") %>% 
  as.data.table()

london_nodes_dt[,"osmid"] <- 
  london_nodes_dt[, lapply(.SD, as.character), .SDcols = c("osmid")]

london_msoa <- list.load("/Users/ivannschlosser/Documents/CASA/data/london_msoa.rds") %>% as.data.table()

london_msoa[,c("centr_geom","workplace_centr","pop_weight_geom","net_centr","geometry")] <-
 lapply(london_msoa[,c("centr_geom","workplace_centr","pop_weight_geom","net_centr","geometry")], FUN=as.character)

# london_msoa %>% str()

## Networks 

network_cppr <- make_cppr_net(london_edges_dt[,.(from,to,length)]
                              ,london_nodes_dt[,.(osmid,x,y)])#list.load("../data/london_graph_simple.rds")

network_cppr <- network_cppr |> cpp_simplify()

network_cppr |> rlist::list.save('cppr_networks/cppr_all.rds')

cppr_nodes <- find_nearest_node_on_graph(network_cppr$coords
                                         ,london_msoa[,"centr_geom"] |> st_as_sf(wkt=1,crs=4326))

network_cppr_ch <- cpp_contract(network_cppr)

## 
dodgr_cycle <- dodgr::weight_streetnet(london_edges_dt %>% st_as_sf()
                                       ,wt_profile = "bicycle")
dodgr_cycle <- dodgr_cycle[dodgr_cycle$component==1,]

dodgr_cycle %>% list.save("cppr_networks/dodgr_cycle.rds")

# dodgr_cycle <- list.load("cppr_networks/dodgr_cycle.rds")

## 

network_tidygraph <- tbl_graph(nodes = london_nodes_dt #%>% as.data.frame()
                               ,edges = london_edges_dt #%>% as.data.frame()
                               ,directed = FALSE
                               ,node_key = "osmid")

ind_of_interest <- find_nearest_node_on_graph(network_tidygraph |> activate(nodes)|> select(osmid,x,y) %>% as.data.frame()
                                                ,london_msoa[,"centr_geom"] |> st_as_sf(wkt=1,crs=4326))

network_tidygraph %>% 
  activate(nodes) %>% 
  filter(osmid %in% nodes_of_interest)

#
network_sf <- list.load("network_sf.rds")
# network_sf <- as_sfnetwork(network_tidygraph)
# network_sf %>% list.save("network_sf.rds")


# replace by st_nearest node
sf_node_ind <- find_nearest_node_on_graph(network_sf |> activate(nodes) |> select(osmid,x,y) |>  as.data.frame()
                                          ,london_msoa[,"centr_geom"] |> st_as_sf(wkt=1,crs=4326))

net_sf_nodes <- network_sf |> 
  activate(nodes) |> 
  select(osmid,x,y) |>  
  as.data.frame() 

net_sf_nodes <- net_sf_nodes[sf_node_ind,"osmid"]

## dodgr setup 

from_xy <- london_msoa[,"pop_weight_geom"] %>% st_as_sf(wkt=1,crs=4326) %>% st_coordinates()

from_ind <- match_points_to_graph(dodgr_cycle, from_xy, connected = TRUE,)
###


# change the following parameters and comment out based on whatever you want to benchmarks
x <- sample(1:983,50)
parallel <- TRUE

# RcppParallel::defaultNumThreads()
RcppParallel::setThreadOptions(numThreads = 1)

benchmark_packages_dist_matrix_speed <- microbenchmark(
  # igraph
  "tidygraph" = igraph::distances(network_tidygraph
                               ,v = ind_of_interest[x]
                               ,to =  ind_of_interest[x]
                               ,algorithm = "dijkstra"
                               ,weights = network_tidygraph |> activate(edges) |> pull(length))
  ,"sf_networks" = sfnetworks::st_network_cost(network_sf
                                               ,from = sf_node_ind[x]
                                               ,to = sf_node_ind[x]
                                               ,weights = "length")
  # dodgr test routing
  ,"dists_dodgr_fheap" =  dodgr_dists(dodgr_cycle
                                      ,from = dodgr_cycle$from_id[from_ind[x]]
                                      ,to = dodgr_cycle$from_id[from_ind[x]]
                                      ,shortest = TRUE
                                      ,quiet = FALSE
                                      ,heap = "FHeap"
                                      ,parallel = parallel)
  ,"dists_dodgr_bheap" =  dodgr_dists(dodgr_cycle
                                      ,from =dodgr_cycle$from_id[from_ind[x]]
                                      ,to = dodgr_cycle$from_id[from_ind[x]]
                                      ,shortest = TRUE
                                      ,quiet = FALSE
                                      ,heap = "BHeap"
                                      ,parallel = parallel)
  ,"dists_dodgr_triheap" =  dodgr_dists(dodgr_cycle
                                        ,from = dodgr_cycle$from_id[from_ind[x]]
                                        ,to = dodgr_cycle$from_id[from_ind[x]]
                                        ,shortest = TRUE
                                        ,quiet = FALSE
                                        ,heap = "TriHeap"
                                        ,parallel = parallel)
  # ,# cppR test
  ,"dists_cppr_phast" = cppRouting::get_distance_matrix(network_cppr
                                                       ,from = network_cppr$coords$osmid[cppr_nodes[x]]
                                                       ,to = network_cppr$coords$osmid[cppr_nodes[x]]
                                                       ,algorithm = "phast")
  ,"dists_cppr_mch_par" = cppRouting::get_distance_matrix(network_cppr_ch
                                                          ,from = network_cppr$coords$osmid[cppr_nodes[x]]
                                                          ,to = network_cppr$coords$osmid[cppr_nodes[x]]
                                                          ,algorithm = "mch")
  ,times = 10
)

print(benchmark_packages_dist_matrix_speed)

benchmark_packages_dist_matrix_speed

citation('microbenchmark')

##########
# memory perf
# profvis_sf_net <- profvis::profvis(
#   expr = { sfnetworks::st_network_cost(network_sf
#                                        ,from = sf_node_ind[x]
#                                        ,to = sf_node_ind[x]
#                                        ,weights = "length")}
# )
# 
# profvis_sf_net
# 
# profvis_cppr_onecore <- profvis::profvis(
#   expr = {cppRouting::get_distance_matrix(network_cppr
#                                           ,from = london_msoa$pw_centr_id[x]
#                                           ,to = london_msoa$pw_centr_id[x]
#                                           ,allcores = FALSE
#                                           ,algorithm = "mch")}
# )
# 
# profvis_cppr_onecore
# 
# profvis_cppr_allcore <- profvis::profvis(
#   expr = {cppRouting::get_distance_matrix(network_cppr
#                                           ,from = london_msoa$pw_centr_id[x]
#                                           ,to = london_msoa$pw_centr_id[x]
#                                           ,allcores = TRUE
#                                           ,algorithm = "mch")}
# )
# 
# profvis_cppr_allcore
# 
# # checking consistency of the points routed
# tmap_mode("view")
# v[from_ind[1:10],] %>% st_as_sf(coords = c("x","y"), crs = 4326) %>% qtm() + 
#   (london_msoa[1:10,pop_weight_geom] %>% qtm(dots.col = "red")) +
#   (network_cppr$coords[match(london_msoa$pw_centr_id[1:10], node_id,),.(X,Y)] %>% 
#      st_as_sf(coords = c("X","Y"),crs = 4326) %>% qtm(dots.col = "blue"))
# # all good

