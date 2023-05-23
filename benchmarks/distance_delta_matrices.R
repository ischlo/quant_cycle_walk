### computing distance matrices for the nezworks of interest:

# osm

all_network_commute <- cbind(
  find_nearest_node_on_graph(all_network$coords
                             ,london_msoa[,"pop_weight_geom"] %>% st_as_sf(wkt = 1, crs =4326))
  ,find_nearest_node_on_graph(all_network$coords
                              ,london_msoa[,"workplace_centr"] %>% st_as_sf(wkt = 1, crs =4326))
  )

all_network_net <- find_nearest_node_on_graph(all_network$coords
                                              ,london_msoa[,"net_centr"] %>% st_as_sf(wkt = 1, crs =4326))

# adding missing links:

missing_links_osm <- list(c("1768610567","91698655"))

all_network <- add_edge_cppr(all_network
                             ,from = missing_links_osm[[1]][1]
                             ,to = missing_links_osm[[1]][2]
                             ,l = 50)

# geom centroids:

delta_osm_geom <- delta_matrix(all_network$coords[match(all_network$dict$ref[all_network_nodes]
                                                        ,osmid),.(x,y)] %>% st_as_sf(crs = 4326, coords = c(1,2))
                               ,london_msoa[,"centr_geom"] %>% st_as_sf(wkt = 1, crs =4326))

osm_dist_matrix <- get_distance_matrix(all_network
                                       ,from = all_network$dict$ref[all_network_nodes]
                                       ,to = all_network$dict$ref[all_network_nodes]
                                       ,allcores = TRUE)

osm_dist_matrix %>% as.numeric %>% summary

(osm_dist_matrix+delta_osm_geom) %>% list.save("osm_dist_matrix_standard.rds")

# commute centroids WORK IN PROGRESS

delta_commute <- delta_matrix(all_network$coords[match(all_network$dict$ref[all_network_commute[,1]]
                                                       ,osmid),.(x,y)] %>% st_as_sf(crs = 4326, coords = c(1,2))
                              ,london_msoa[,"pop_weight_geom"] %>% st_as_sf(wkt = 1, crs =4326))

osm_dist_matrix_commute <- get_distance_matrix(all_network
                                               ,from = all_network$coords$osmid[all_network_commute[,1]]
                                               ,to = all_network$coords$osmid[all_network_commute[,2]]
                                               ,allcores = TRUE)

osm_dist_matrix_commute %>% as.numeric %>% summary

# which((osm_dist_matrix_commute)> 1000000, arr.ind = TRUE)
# tmap_mode("view")
# all_network$coords[all_network_commute[510,1],] %>% st_as_sf(crs = 4326,coords = c(2,3)) %>% qtm()

(osm_dist_matrix_commute+delta_commute) %>% list.save("osm_dist_matrix_commute.rds")

# network centroids

delta_osm_network <- delta_matrix(all_network$coords[match(all_network$dict$ref[all_network_net]
                                                           ,osmid),.(x,y)] %>% st_as_sf(crs = 4326, coords = c(1,2))
                                  ,london_msoa[,"net_centr"] %>% st_as_sf(wkt = 1, crs =4326))

osm_dist_matrix_network <- get_distance_matrix(all_network
                                               ,from = all_network$dict$ref[all_network_net]
                                               ,to = all_network$dict$ref[all_network_net]
                                               ,allcores = TRUE)

osm_dist_matrix_network%>% as.numeric %>% summary

(osm_dist_matrix_network+delta_osm_network) %>% list.save("osm_dist_matrix_network.rds")


#### OS network setup and distance calculation

# os_cppr_data <- list.load("os_network_data.rds")

os_cppr_simple <- rlist::list.load("Benchmarks/cppr_networks/os_all_graph.rds")

os_cppr_simple <- os_cppr_data$cppr_graph
nn_id <- os_cppr_simple$coords$osmid[os_cppr_data$nn_ind]

# os_cppr_simple <- os_cppr_simple %>% cpp_contract()

# nearest nodes commute:
os_commute_nodes <- cbind(
  find_nearest_node_on_graph(os_cppr_simple$coords %>% st_as_sf(coords = c(2,3), crs = 4326)
                             ,london_msoa[,"pop_weight_geom"] %>% st_as_sf(wkt = 1, crs =4326))
  ,find_nearest_node_on_graph(os_cppr_simple$coords %>% st_as_sf(coords = c(2,3), crs = 4326)
                              ,london_msoa[,"workplace_centr"] %>% st_as_sf(wkt = 1, crs =4326))
)

# nearest nodes network:

os_net_nodes <- find_nearest_node_on_graph(os_cppr_simple$coords %>% st_as_sf(coords = c(2,3), crs = 4326)
                                           ,london_msoa[,"net_centr"] %>% st_as_sf(wkt = 1, crs =4326))

### distances with OS network 

# geometric centroids
delta_os_geom <- delta_matrix(os_cppr_simple$coords[match(nn_id,osmid),.(x,y)] %>% st_as_sf(crs = 4326, coords = c(1,2))
                              ,london_msoa[,"centr_geom"] %>% st_as_sf(wkt = 1, crs =4326)
                              )

os_network_missing <- list(c("0219C53F-D424-478D-A9EC-79EDE23D475B","F2295492-4CB7-4764-A0E0-6665B090E9D7"))

os_cppr_simple <- add_edge_cppr(graph = os_cppr_simple
                                ,from = os_network_missing[[1]][1]
                                ,to = os_network_missing[[1]][2]
                                ,l = 250)

os_dist_matrix_geom <- get_distance_matrix(os_cppr_simple
                                           ,from = nn_id
                                           ,to = nn_id
                                           ,allcores = TRUE)

os_dist_matrix_geom %>% as.numeric %>% summary

# which((os_dist_matrix_geom)> 1000000, arr.ind = TRUE)
# tmap_mode("view")
# os_cppr_simple$coords[osmid == nn_id[114],] %>% st_as_sf(crs = 4326,coords = c(2,3)) %>% qtm()

(os_dist_matrix_geom+delta_os_geom) %>% list.save("os_dist_matrix_geom.rds")

# commute centroids

delta_os_commute <- delta_matrix(os_cppr_simple$coords[match(nn_id,osmid),.(x,y)] %>% st_as_sf(crs = 4326, coords = c(1,2))
                                 ,london_msoa[,"pop_weight_geom"] %>% st_as_sf(wkt = 1, crs =4326))


os_dist_matrix_commute <- get_distance_matrix(os_cppr_simple
                                              ,from = os_cppr_simple$coords$osmid[os_commute_nodes[,1]]
                                              ,to =  os_cppr_simple$coords$osmid[os_commute_nodes[,2]]
                                              ,allcores = TRUE)

os_dist_matrix_commute %>% as.numeric %>% summary()

# os_cppr_simple$coords[os_commute_nodes[114,1],] %>% st_as_sf(crs = 4326,coords = c(2,3)) %>% qtm()

(delta_os_commute+os_dist_matrix_commute) %>% list.save("os_dist_matrix_commute.rds")


# network centroids

delta_os_network <- delta_matrix(os_cppr_simple$coords[match(nn_id,osmid),.(x,y)] %>% st_as_sf(crs = 4326, coords = c(1,2))
                                 ,london_msoa[,"net_centr"] %>% st_as_sf(wkt = 1, crs =4326))

os_dist_matrix_network <- get_distance_matrix(os_cppr_simple
                                              ,from = os_cppr_simple$coords$osmid[os_net_nodes]
                                              ,to = os_cppr_simple$coords$osmid[os_net_nodes]
                                              ,allcores = TRUE)

os_dist_matrix_network %>% as.numeric %>% summary

(delta_os_network+os_dist_matrix_network) %>% list.save("os_dist_matrix_network.rds")


#### dodgr_network 

dodgr_commute_nodes <- cbind(
  find_nearest_node_on_graph(dodgr_cycle$coords %>% st_as_sf(coords = c(2,3), crs = 4326)
                             ,london_msoa[,"pop_weight_geom"] %>% st_as_sf(wkt = 1, crs =4326))
  ,find_nearest_node_on_graph(dodgr_cycle$coords %>% st_as_sf(coords = c(2,3), crs = 4326)
                              ,london_msoa[,"workplace_centr"] %>% st_as_sf(wkt = 1, crs =4326))
)

dodgr_network_nodes <- 
  find_nearest_node_on_graph(dodgr_cycle$coords %>% st_as_sf(coords = c(2,3), crs = 4326)
                             ,london_msoa[,"net_centr"] %>% st_as_sf(wkt = 1, crs =4326))


###

dodgr_cycle_missing <- list(c("760048","272489",120)
                            ,c("699423","612715",120)
                            ,c("454251","1146675",120)
                            ,c("1039227","46211",120)
                            ,c("636289","358397",60)
                            ,c("1015810","59036",100)
                            ,c("450392","1181744",50)
                            ,c("730518","131783",50)
                            ,c("1112892","596546",200))



# for(i in 1:length(dodgr_cycle_missing)){
#   dodgr_cycle <- add_edge_cppr(graph = dodgr_cycle
#                                ,from = dodgr_cycle_missing[[i]][1]
#                                ,to = dodgr_cycle_missing[[i]][2]
#                                ,l = as.numeric(dodgr_cycle_missing[[i]][3]))
#   }


# geom centroids distance matrix

delta_dodgr_geom <- delta_matrix(dodgr_cycle$coords[dodgr_cycle_noi,.(x,y)] %>% st_as_sf(crs = 4326, coords = c(1,2))
                                 ,london_msoa[,"centr_geom"] %>% st_as_sf(wkt = 1, crs =4326))

dodgr_dists_geom <- get_distance_matrix(dodgr_cycle
                                        ,from = dodgr_cycle$coords$osmid[dodgr_cycle_noi]
                                        ,to = dodgr_cycle$coords$osmid[dodgr_cycle_noi]
                                        ,allcores = TRUE)

dodgr_dists_geom %>% as.numeric %>% summary

# which((dodgr_dists_geom)> 1000000, arr.ind = TRUE)
# tmap_mode("view")
# dodgr_cycle$coords[dodgr_cycle_noi[870],] %>% st_as_sf(crs = 4326,coords = c(2,3)) %>% qtm()

(delta_dodgr_geom+dodgr_dists_geom) %>% list.save("dodgr_dists_geom.rds")


# commute centroids distance matrix

delta_dodgr_commute <- delta_matrix(dodgr_cycle$coords[dodgr_commute_nodes[,1],.(x,y)] %>% st_as_sf(crs = 4326, coords = c(1,2))
                                    ,london_msoa[,"pop_weight_geom"] %>% st_as_sf(wkt = 1, crs =4326))

dodgr_dists_commute <- cppRouting::get_distance_matrix(dodgr_cycle
                                                       ,from = dodgr_cycle$coords$osmid[dodgr_commute_nodes[,1]]
                                                       ,to = dodgr_cycle$coords$osmid[dodgr_commute_nodes[,2]]
                                                       ,allcores = TRUE
)

dodgr_dists_commute %>% as.numeric() %>% summary()

# which((dodgr_dists_commute)> 1000000, arr.ind = TRUE)
# tmap_mode("view")
# dodgr_cycle$coords[dodgr_commute_nodes[727,2],] %>% st_as_sf(crs = 4326,coords = c(2,3)) %>% qtm()

(dodgr_dists_commute+delta_dodgr_commute) %>% list.save("dodgr_dists_commute.rds")

## network centroids distance matrix

delta_dodgr_network <- delta_matrix(dodgr_cycle$coords[dodgr_network_nodes,.(x,y)] %>% st_as_sf(crs = 4326, coords = c(1,2))
                                    ,london_msoa[,"net_centr"] %>% st_as_sf(wkt = 1, crs =4326))

dodgr_dists_network <- get_distance_matrix(dodgr_cycle
                                           ,from = dodgr_cycle$coords$osmid[dodgr_network_nodes]
                                           ,to = dodgr_cycle$coords$osmid[dodgr_network_nodes]
                                           ,allcores = TRUE)

# which(dodgr_dists_network > 10000000, arr.ind = TRUE) # 582, 870, 959
# tmap_mode("view")
# dodgr_cycle$coords[dodgr_network_nodes[582],] %>% st_as_sf(crs = 4326,coords = c(2,3)) %>% qtm()

dodgr_dists_network %>% as.numeric() %>% summary

(dodgr_dists_network+delta_dodgr_network) %>% list.save("dodgr_dists_network.rds")

### Saving the modififed graphs

list.save(dodgr_cycle,"dodgr_cycle_cppr_modified.rds")
list.save(os_cppr_simple,"os_cppr_modified.rds")
list.save(all_network,"osm_cppr_modified.rds")


