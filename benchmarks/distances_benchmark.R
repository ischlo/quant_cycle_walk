## Benchmarking networks

# networks are routed in cppr but with the weighting profiles 
# and segment selection corresponding to the mentioned package for 
# specified mode of transport

london_msoa[, "tip_dist"] <- london_msoa[,"geometry"] %>%
              st_as_sf(wkt = 1,crs = 4326) %>%
              st_area() %>%
              sqrt() %>%
              round() %>%
              set_units(NULL)

# all: the whole network with just motoroways removed. 
all_network <- list.load("/Users/ivann/Desktop/CASA/benchmarks/osm_all_graph.rds")

# INITIATING THE GRAPH WITH THE OMSNX OUTPUT 
# all_network <- make_cppr_net(edges = london_edges_dt[!grepl("motorway",highway),.(from,to,length)]
#                              ,nodes = london_nodes_dt[,.(osmid,x,y)]) 

# a link that can be seen on google maps but is missing in the osm data is added manually, because it creates a disconnection otherwise. 
# some disconnections are added when the motorways are removed, so in the case when a node falls into one, it has to be added. 

missing_links_osm <- list(c("1768610567","91698655"))

# could be used with lapply vectorially. But the node determination hs to be manual as of now.
all_network <- add_edge_cppr(all_network
                             ,from = missing_links_osm[[1]][1]
                             ,to = missing_links_osm[[1]][2]
                             ,l = 15)

# finding the nodes corresponding to the centroids for the network. 
all_network_nodes <- find_nearest_node_on_graph(all_network$coords
                                                ,london_msoa[,"centr_geom"] %>% st_as_sf(wkt = 1,crs = 4326))

# contracting speeds things up even more, but unfortunately the new graph has no info on the nodes coordinates. 
#  so the contraction is left out. possibly make a function that adds the nodes with id and coordinates back into the graph when contracted.
#  
# all_network <- all_network %>% cppRouting::cpp_contract()

#### OS network setup

os_cppr_data <- list.load("os_network_data.rds")

os_cppr_simple <- os_cppr_data$cppr_graph
nn_id <- os_cppr_simple$coords$osmid[os_cppr_data$nn_ind]

# os_cppr_simple <- os_cppr_simple %>% cpp_contract()

# can get rid of this file to save memory after getting the graph and nodes of interest.
os_cppr_data <- NULL

#### cycle

## dodgr
dodgr_cycle <- list.load("dodgr_cycle.rds") %>% as.data.table()

# uncomment to recompute the nodes.
# dodgr_cycle_nodes <- dodgr_cycle[,rbind(data.table(osmid = from_id,x=from_lon,y =from_lat)
#                                           ,data.table(osmid = to_id,x = to_lon,y = to_lat))][
#                                             !duplicated(osmid)
#                                           ]
# 
# dodgr_cycle_nodes %>% list.save("dodgr_cycle_nodes.rds")
# 
dodgr_cycle_nodes <- list.load("dodgr_cycle_nodes.rds")

dodgr_cycle <- make_cppr_net(edges = dodgr_cycle[,.(from = from_id,to = to_id,length = round(d_weighted))]
                             ,nodes = dodgr_cycle_nodes)
# dodgr_cycle$data$dist <- dodgr_cycle$data$dist %>% round()

dodgr_cycle_noi <- find_nearest_node_on_graph(dodgr_cycle$coords
                                              ,london_msoa[,"centr_geom"] %>% st_as_sf(wkt = 1,crs = 4326))

# dodgr_cycle <- dodgr_cycle %>% cpp_contract()

## osmnx
osmnx_cycle <- make_cppr_net(edges = london_edges_dt[,.(from,to,length)]
                             ,nodes = london_nodes_dt[,.(osmid,x,y)])

osmnx_cycle$data$dist <- osmnx_cycle$data$dist %>% round()

osmnx_cycle_noi <- find_nearest_node_on_graph(osmnx_cycle$coords
                                              ,london_msoa[,"centr_geom"] %>% st_as_sf(wkt = 1,crs = 4326))

osmnx_cycle <- osmnx_cycle %>% cpp_contract()

#### walk

##dodgr

dodgr_walk <- list.load("dodgr_walk_raw.rds")

# dodgr_walk <- dodgr::weight_streetnet(london_edges_dt[,.(from,to,highway,length,geom)] %>% st_as_sf(crs = 4326)
#                                       ,wt_profile = "foot") %>% as.data.table()
# 
# list.save(dodgr_walk,"dodgr_walk_raw.rds")
# dodgr_walk_nodes <- dodgr_walk[,rbind(data.table(osmid = from_id,x=from_lon,y =from_lat)
#                                          ,data.table(osmid = to_id,x = to_lon,y = to_lat))][
#                                            !duplicated(osmid)
#                                            ]
# dodgr_walk_nodes %>% list.save("dodgr_walk_nodes.rds")
# 
dodgr_walk_nodes <- list.load("dodgr_walk_nodes.rds")

dodgr_walk <- make_cppr_net(edges = dodgr_walk[,.(from = from_id,to = to_id,length = round(d_weighted))]
                            ,nodes = dodgr_walk_nodes)

dodgr_walk_noi <- find_nearest_node_on_graph(dodgr_walk$coords
                                             ,london_msoa[,"centr_geom"] %>% st_as_sf(wkt=1,crs = 4326))

dodgr_walk <- dodgr_walk %>% cpp_contract()

## osmnx

osmnx_walk <- make_cppr_net(edges = london_edges_dt[,.(from,to,length)]
                            ,nodes = london_nodes_dt[,.(osmid,x,y)])

osmnx_walk$data$dist <- osmnx_walk$data$dist %>% round()

osmnx_walk_noi <- find_nearest_node_on_graph(osmnx_walk$coords
                                             ,london_msoa[,"centr_geom"]%>% st_as_sf(wkt = 1,crs = 4326))

osmnx_walk <- osmnx_walk %>% cpp_contract()

# checking that all netwrks are loaded:
# #### Saving the networks

dodgr_cycle %>% list.save("dodgr_cycle_graph.rds")
dodgr_walk
osmnx_cycle
osmnx_walk
all_network %>% list.save("osm_all_graph.rds")
list("graph" = os_cppr_simple
     ,"nn_id" = nn_id ) %>% list.save("os_all_graph.rds")

# saving the nodes:

dodgr_cycle$dict$ref[dodgr_cycle_noi] %>% list.save("dodgr_cycle_noi.rds")

all_network$dict$ref[all_network_nodes] %>% list.save("osm_all_nodes.rds")

all_network$dict$id[all_network_nodes]

#### Runnning distance matrices ####

# the reference distance is the euclidean one
distances_0 <- st_distance(london_msoa[,"centr_geom"] %>% st_as_sf(wkt = 1,crs = 4326)
                           ,by_element = FALSE) %>% 
  round() %>% 
  set_units(NULL)

distances_0 <- `diag<-`(distances_0,london_msoa[,tip_dist])

dist_0_interest <- which(distances_0 < 15000,arr.ind = TRUE)

orig <- dist_0_interest[,1]
dest <- dist_0_interest[,2]

x <- sample(1:nrow(dist_0_interest),15000)

#### distance matrices 

#### distance pairs ####

dist_1 <- get_distance_pair(dodgr_walk
                            ,from = dodgr_walk$dict$ref[dodgr_walk_noi[orig[x]]]
                            ,to = dodgr_walk$dict$ref[dodgr_walk_noi[dest[x]]]
                            ,algorithm = "NBA"
                            ,allcores = TRUE) %>% round()

dist_2 <- get_distance_pair(osmnx_walk
                            ,from = osmnx_walk$dict$ref[osmnx_walk_noi[orig[x]]]
                            ,to = osmnx_walk$dict$ref[osmnx_walk_noi[dest[x]]]
                            ,algorithm = "NBA"
                            ,allcores = TRUE) %>% round()

dist_3 <- get_distance_pair(dodgr_cycle
                            ,from = dodgr_cycle$dict$ref[dodgr_cycle_noi[orig[x]]]
                            ,to = dodgr_cycle$dict$ref[dodgr_cycle_noi[dest[x]]]
                            ,algorithm = "NBA"
                            ,allcores = TRUE)%>% round()

dist_4 <- get_distance_pair(osmnx_cycle
                            ,from = osmnx_cycle$dict$ref[osmnx_cycle_noi[orig[x]]]
                            ,to = osmnx_cycle$dict$ref[osmnx_cycle_noi[dest[x]]]
                            ,algorithm = "NBA"
                            ,allcores = TRUE)%>% round()

dist_5 <- get_distance_pair(all_network
                            ,from = all_network$dict$ref[all_network_nodes[orig[x]]]
                            ,to = all_network$dict$ref[all_network_nodes[dest[x]]]
                            ,algorithm = "NBA"
                            ,allcores = TRUE)%>% round()

dist_6 <- get_distance_pair(os_cppr_simple
                            ,from = nn_id[orig[x]]
                            ,to = nn_id[dest[x]]
                            ,algorithm = "NBA"
                            ,allcores = TRUE)

####
distances <- list("dodgr_walk" = dist_1
                  ,"osmnx_walk" = dist_2
                  ,"dodgr_cycle" = dist_3
                  ,"osmnx_cycle" = dist_4
                  ,"all_network_osm" = dist_5
                  ,"all_network_os" = dist_6)
# distances <- distances %>% lapply(function(m) `diag<-`(m,london_msoa[x,tip_dist]))
# list.save(distances, "distances.rds")

lapply(distances,summary)

{
par(mfrow = c(3,2)
    ,mar = c(2,2,2,2)
    ,oma = c(1.1,1.1,1.1,1.1))
distances %>% lapply(function(x) if(!is_null(x)) { 
  
  x %>% 
    as.numeric() %>% 
    density(na.rm = TRUE) %>% 
    plot(xlim = c(0,50000)
         ,main = "")
  
})
par(mfrow = c(1,1))
}

trip_dilation <- lapply(distances, function(mat) ((mat %>% as.numeric())/distances_0[dist_0_interest[x,]]))

trip_dilation %>% lapply(function(mat) if(!is_null(mat)) {mat %>% as.numeric() %>% summary()})

# trip_dilation <- as.data.frame(trip_dilation)
# 

{
par(mfrow = c(1,1)
    ,mai = c(1.5,1.5,1.5,1.5)
    ,oma = c(1.1,1.1,1.1,1.1)
    ,xaxs = "i"
    ,lwd = 2
    ,cex =1.3
    )
  
cols <- c("blue","black","red","darkred","green","purple")
lty <- c(1,2,3)
  
pdf("dilation_distr.pdf"
    ,height = 5.83
    ,width = 5.83)  

trip_dilation[[1]] %>% 
  as.numeric() %>% 
  density(na.rm = TRUE) %>% 
  plot(xlim = c(1,2)
       ,ylim = c(0,8)
       ,ylab = "density"
       ,xlab = "dilation factor"
       ,col = "blue"
       ,ann = TRUE
       ,main = "Distribution of dilation")

legend(x = "topright"
       ,legend = names(trip_dilation)
       ,col = cols
       ,lwd = 2
       ,lty = 1)

trip_dilation %>% mapply(cols, FUN = function(x,y) if(!is_null(x)) { 
  
  x %>% 
    as.numeric() %>% 
    density(na.rm = TRUE) %>% 
    lines(col = y
          ,lwd = 2
          ,lty = 
         )

  })
dev.off()
par(mfrow = c(1,1))
}
 


##### benchmark NBA vs distmatrix

s2 <- sample(1:893,200)

# nba_vsdist_matrix <- microbenchmark::microbenchmark("t1" = get_distance_pair(all_network
#                                                                              ,from = all_network$dict$ref[all_network_nodes[orig[x]]]
#                                                                              ,to = all_network$dict$ref[all_network_nodes[dest[x]]]
#                                                                              ,algorithm = "NBA"
#                                                                              ,allcores = TRUE) %>% round()
#                                                     ,"t2" = get_distance_matrix(all_network
#                                                                                 ,from = all_network$dict$ref[all_network_nodes[s2]]
#                                                                                 ,to = all_network$dict$ref[all_network_nodes[s2]]
#                                                                                 ,allcores = TRUE) %>% round()
#                                                     ,times = 1
# )


m1 <- get_distance_matrix(os_cppr
                          ,from = nn_id[s2]
                          ,to = nn_id[s2]
                          ,allcores = TRUE) %>% round()

m2 <- get_distance_matrix(os_cppr_simple
                          ,from = nn_id[s2]
                          ,to = nn_id[s2]
                          ,allcores = TRUE) %>% round()

identical(
  m1 %>% round()
  ,m2 %>% round()
)

test_contracted_vs_not <- microbenchmark::microbenchmark(
  "m1" = get_distance_matrix(os_cppr
                             ,from = nn_id[s2]
                             ,to = nn_id[s2]
                             ,allcores = TRUE) %>% round()
  ,# contracted graph
  "m2" = get_distance_matrix(os_cppr_simple
                             ,from = nn_id[s2]
                             ,to = nn_id[s2]
                             ,allcores = TRUE) %>% round()
  ,times = 10
)

test_contracted_vs_not

####


