library(data.table)
library(sf)
library(dplyr)
library(rlist)
library(cppRouting)

## OS data 

os_london <- st_read("data/OS_network/TQ_RoadLink.shp") |> st_transform(4326) |> as.data.table()

london_msoa <- list.load("data/london_msoa.rds")

tmap::tmap_mode("view")

os_london  |>  colnames()

os_london <- os_london[,.(identifier,length,startNode,endNode,geometry)]

os_lines <- os_london[,.(startNode,endNode,geometry)][,st_coordinates(geometry)] |>
  as_tibble() |> 
  dplyr::group_by(L1) |>
  dplyr::reframe(start_x = X[1]
            ,start_y = Y[1]
            ,end_x = X[n()]
            ,end_y = Y[n()]) |> 
  as.data.table()

os_london <- cbind(os_london,os_lines)

os_nodes <- rbind(os_london[,.(osmid = startNode
                            ,x = start_x
                            ,y = start_y)]
               ,os_london[,.(osmid = endNode
                             ,x = end_x
                             ,y = end_y)])

os_nodes <- os_nodes[!duplicated(osmid),]

# os_london <- get_lcc(os_london[,.(from = startNode,to = endNode,length)])

os_cppr_simple <- make_cppr_net(edges = os_london
                                ,nodes = os_nodes)

# REDO THIS WITH ST_NEAREST_FEATURE
# nn_ind <- find_nearest_node_on_graph(os_cppr_simple$coords
#                                      ,london_msoa |> st_as_sf(wkt = "net_centr",crs = 4326))

# os_network_data <- list(cppr_graph = os_cppr_simple
                        # ,nn_ind = nn_ind)

# list.save(os_network_data,"os_network_data.rds")

list.save(os_cppr_simple,"Benchmarks/cppr_networks/os_graph.rds")
