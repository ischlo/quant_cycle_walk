## Benchmarking networks
# 
# THIS IS STILL A BIT MESSY, CLEAN !!!
# TO DO:
# #   - INITIATION OF THE CPPR_NETWORKS. SEPARATE FILE ? IT DOES NOT REALLY RELY ON THE ROUTING_BENCHMARK
# Have two scripts, one that initiates the networks for their native packages, 
# and them is executed before routing_benchmark
# 
# And another that build cppr networks from the native graphs
# and call it everytime i need the graphs. 


# networks are routed in cppr but with the weighting profiles 
# and segment selection corresponding to the mentioned packages for 
# specified mode of transport
# this script needs to run after routing_benchmarks.R


london_msoa[, "tip_dist"] <- london_msoa[,"geometry"] |>
              st_as_sf(wkt = 1,crs = 4326) |>
              st_area() |>
              sqrt() |>
              round() |>
              units::set_units(NULL)

# all: the whole network with just motoroways removed. 
all_network <- list.load("/Users/ivannschlosser/Documents/CASA/Benchmarks/cppr_networks/cppr_all.rds")

# INITIATING THE GRAPH WITH THE OMSNX OUTPUT 
# london_edges_lcc <- get_lcc(london_edges_dt[!grepl("motorway",highway),.(from,to,length)])
# 
# all_network <- make_cppr_net(edges = london_edges_lcc
#                              ,nodes = london_nodes_dt[,.(osmid,x,y)])

# a link that can be seen on google maps but is missing in the osm data is added manually, because it creates a disconnection otherwise. 
# some disconnections are added when the motorways are removed, so in the case when a node falls into one, it has to be added. 
# missing_links_osm <- list(c("1768610567","91698655"))
# could be used with lapply vectorially. But the node determination hs to be manual as of now.
# alternatively keep only the largest connected component
# all_network <- add_edge_cppr(all_network
#                              ,from = missing_links_osm[[1]][1]
#                              ,to = missing_links_osm[[1]][2]
#                              ,l = 15)
# 
# finding the nodes corresponding to the centroids for the network. 
all_network_nodes <- find_nearest_node_on_graph(all_network$coords
                                                ,london_msoa[,"centr_geom"]  |>  st_as_sf(wkt = 1,crs = 4326))

# contracting speeds things up even more, but unfortunately the new graph has no info on the nodes coordinates. 
#  so the contraction is left out. possibly make a function that adds the nodes with id and coordinates
# back into the graph when contracted.
#  
# all_network <- all_network |> cppRouting::cpp_contract()

#### OS network setup

os_cppr <- list.load("cppr_networks/os_graph.rds")

os_nn <- find_nearest_node_on_graph(os_cppr$coords
                                    ,london_msoa[,"centr_geom"] |> st_as_sf(wkt = 1,crs = 4326))

os_cppr_simple <- os_cppr |> cpp_contract()

#### cycle

## dodgr
dodgr_cycle <- list.load("cppr_networks/dodgr_cycle.rds") |> as.data.table()

# uncomment to recompute the nodes.
dodgr_cycle_nodes <- dodgr_cycle[,rbind(data.table(osmid = from_id,x=from_lon,y =from_lat)
                                          ,data.table(osmid = to_id,x = to_lon,y = to_lat))][
                                            !duplicated(osmid)
                                          ]

dodgr_cycle <- make_cppr_net(edges = dodgr_cycle[,.(from = from_id,to = to_id,length = round(d_weighted))]
                             ,nodes = dodgr_cycle_nodes)
# dodgr_cycle$data$dist <- dodgr_cycle$data$dist |> round()

dodgr_cycle_noi <- find_nearest_node_on_graph(dodgr_cycle$coords
                                              ,london_msoa[,"centr_geom"] |> st_as_sf(wkt = 1,crs = 4326))

dodgr_cycle <- dodgr_cycle |> cpp_contract()

## osmnx
osmnx_cycle <- make_cppr_net(edges = london_edges_dt[,.(from,to,length)]
                             ,nodes = london_nodes_dt[,.(osmid,x,y)])

osmnx_cycle$data$dist <- osmnx_cycle$data$dist |> round()

osmnx_cycle_noi <- find_nearest_node_on_graph(osmnx_cycle$coords
                                              ,london_msoa[,"centr_geom"] |> st_as_sf(wkt = 1,crs = 4326))

osmnx_cycle <- osmnx_cycle |> cpp_contract()

#### walk

##dodgr

dodgr_walk <- list.load("dodgr_raw_networks/dodgr_walk_raw.rds")

# dodgr_walk <- dodgr::weight_streetnet(london_edges_dt[,.(from,to,highway,length,geom)] |> st_as_sf(crs = 4326)
#                                       ,wt_profile = "foot") |> as.data.table()
# 
# list.save(dodgr_walk,"dodgr_walk_raw.rds")
dodgr_walk_nodes <- dodgr_walk[,rbind(data.table(osmid = from_id,x=from_lon,y =from_lat)
                                         ,data.table(osmid = to_id,x = to_lon,y = to_lat))][
                                           !duplicated(osmid)
                                           ]
# dodgr_walk_nodes |> list.save("dodgr_walk_nodes.rds")
# 
# dodgr_walk_nodes <- list.load("dodgr_walk_nodes.rds")

dodgr_walk <- make_cppr_net(edges = dodgr_walk[,.(from = from_id,to = to_id,length = round(d_weighted))]
                            ,nodes = dodgr_walk_nodes)

dodgr_walk_noi <- find_nearest_node_on_graph(dodgr_walk$coords
                                             ,london_msoa[,"centr_geom"] |> st_as_sf(wkt=1,crs = 4326))

dodgr_walk <- dodgr_walk |> cpp_contract()

## osmnx
# 
# osmnx_walk <- make_cppr_net(edges = london_edges_dt[,.(from,to,length)]
#                             ,nodes = london_nodes_dt[,.(osmid,x,y)])
# 
# osmnx_walk$data$dist <- osmnx_walk$data$dist |> round()
# 
# osmnx_walk_noi <- find_nearest_node_on_graph(osmnx_walk$coords
#                                              ,london_msoa[,"centr_geom"]|> st_as_sf(wkt = 1,crs = 4326))
# 
# osmnx_walk <- osmnx_walk |> cpp_contract()

# checking that all netwrks are loaded:
# #### Saving the networks

dodgr_walk
osmnx_cycle
# osmnx_walk

#### Runnning distance matrices ####

# the reference distance is the euclidean one
distances_0 <- st_distance(london_msoa[,"centr_geom"] |> st_as_sf(wkt = 1,crs = 4326)
                           ,by_element = FALSE) |> 
  round() |> 
  units::set_units(NULL)

distances_0 <- `diag<-`(distances_0,london_msoa[,tip_dist])

dist_0_interest <- which(distances_0 < 15000,arr.ind = TRUE)

#### distance matrices 
RcppParallel::setThreadOptions(numThreads = 6)

dist_1 <- get_distance_matrix(dodgr_walk
                            ,from = dodgr_walk$dict$ref[dodgr_walk_noi]
                            ,to = dodgr_walk$dict$ref[dodgr_walk_noi]
                            ) |> round()

dist_2 <- get_distance_matrix(osmnx_walk
                            ,from = osmnx_walk$dict$ref[osmnx_walk_noi]
                            ,to = osmnx_walk$dict$ref[osmnx_walk_noi]
                            ) |> round()

dist_3 <- get_distance_matrix(dodgr_cycle
                            ,from = dodgr_cycle$dict$ref[dodgr_cycle_noi]
                            ,to = dodgr_cycle$dict$ref[dodgr_cycle_noi]
                            )|> round()

dist_4 <- get_distance_matrix(osmnx_cycle
                            ,from = osmnx_cycle$dict$ref[osmnx_cycle_noi]
                            ,to = osmnx_cycle$dict$ref[osmnx_cycle_noi]
                            )|> round()

dist_5 <- get_distance_matrix(all_network
                            ,from = all_network$dict$ref[all_network_nodes]
                            ,to = all_network$dict$ref[all_network_nodes]
                            )|> round()

dist_6 <- get_distance_matrix(os_cppr_simple
                            ,from = nn_id
                            ,to = nn_id
                            )


####
distances <- list(#"dodgr_walk" = dist_1
                  #,"osmnx_walk" = dist_2
                  "dodgr_cycle" = dist_3
                  # ,"osmnx_cycle" = dist_4
                  ,"all_network_osm" = dist_5
                  ,"all_network_os" = dist_6)

distances <- distances |> lapply(function(m) `diag<-`(m,london_msoa[,tip_dist]))

deltas <- list("delta_dodgr" = delta_dodgr_geom,"delta_osm" = delta_osm_geom,"delta_os" = delta_os_geom)

# lapply(deltas,FUN = function(x) x |> dim)

distances <- mapply(distances,deltas, FUN = function(mat,delt) (mat[dist_0_interest] + delt[dist_0_interest]))

distances <- as.data.frame(distances)

distances$origin <- london_msoa$geo_code[dist_0_interest[,1]]
distances$destination <- london_msoa$geo_code[dist_0_interest[,2]]
distances$euclid <- distances_0[dist_0_interest]

distances <- distances[,c(4:6,1:3)]

distances |> head()
distances |> summary()

distances |> drop_na() |> write_csv("distances_tidy.csv")

######

# list.save(distances, "distances.rds")

##### benchmark NBA vs distmatrix

