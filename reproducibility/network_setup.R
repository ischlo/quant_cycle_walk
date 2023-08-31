
library(sf)
library(data.table)
library(dodgr)
# library(Btoolkit)

# Functions for setting up networks. 
source("scripts/network/cppr_network_setup.R")

source("scripts/network/os_data.R")

### Setup OSM:

osm_filepath <- 'RC_outputs/london_all/london_all.gpkg'


if(file.exists('benchmarks/cppr_networks/osm_all.rds')) {
  cat('OSM all network already exists.\n'
      ,'If you want to regenerate, erase first\n')
} else {
  # full osm
  london_all_edges <- st_read(osm_filepath, layer = 'edges') |> as.data.table()
  
  london_all_nodes <- st_read(osm_filepath, layer = 'nodes') |> as.data.table()
  
  osm_all <- make_cppr_net(london_all_edges[,.(from,to,length)]
                           ,nodes = london_all_nodes[,.(osmid,x,y)])
  
  osm_all |> rlist::list.save('benchmarks/cppr_networks/osm_all.rds')
}
## osm cycle
# 

if(file.exists('benchmarks/cppr_networks/osm_bike.rds')) {
  cat('OSM cycle network already exists.\n'
      ,'If you want to regenerate, erase first\n')
} else {
  london_cycle_edges <- st_read('RC_outputs/bike_graph.gpkg', layer = 'edges') |> as.data.table()
  
  london_cycle_nodes <- st_read('RC_outputs/bike_graph.gpkg', layer = 'nodes') |> as.data.table()
  
  osm_cycle <- make_cppr_net(london_cycle_edges[,.(from,to,length)]
                             ,nodes = london_cycle_nodes[,.(osmid,x,y)])
  
  # save result
  osm_cycle |> rlist::list.save('benchmarks/cppr_networks/osm_bike.rds')
}


# osm walk
if(file.exists('benchmarks/cppr_networks/osm_walk.rds')) {
  cat('OSM walk network already exists.\n'
      ,'If you want to regenerate, erase first\n')
} else {
  london_walk_edges <- st_read('RC_outputs/walk_graph.gpkg', layer = 'edges') |> as.data.table()
  
  london_walk_nodes <- st_read('RC_outputs/walk_graph.gpkg', layer = 'nodes') |> as.data.table()
  
  osm_walk <- make_cppr_net(london_walk_edges[,.(from,to,length)]
                            ,nodes = london_walk_nodes[,.(osmid,x,y)])
  
  # save result
  osm_walk |> rlist::list.save('benchmarks/cppr_networks/osm_walk.rds')
}
## DODGR NETWORK

## dodgr cycle
if(file.exists('benchmarks/cppr_networks/dodgr_cycle.rds')) {
  cat('DODGR cycle network already exists.\n'
      ,'If you want to regenerate, erase first\n')
} else {
  dodgr_cycle <- london_all_edges[,.(from,to,highway,osmid,length,geom)] |>
    sf::st_as_sf() |>
    dodgr::weight_streetnet(wt_profile = 'bicycle',id_col = 'osmid')
  
  dodgr_cycle <- as.data.table(dodgr_cycle)
  
  dodgr_cycle_edges <- dodgr_cycle[component==1,.(id=edge_id,from=from_id,to=to_id,length=d_weighted)]
  
  dodgr_cycle_nodes <- data.table::rbindlist(list(
    dodgr_cycle[component==1,.(osmid=from_id,x=from_lon,y=from_lat)]
    ,dodgr_cycle[component==1,.(osmid=to_id,x=to_lon,y=to_lat)]
  ))
  
  dodgr_cycle_nodes <- dodgr_cycle_nodes[!duplicated(osmid),]
  
  dodgr_cycle_cppr <- make_cppr_net(edges = dodgr_cycle_edges
                                    ,nodes = dodgr_cycle_nodes)
  
  dodgr_cycle_cppr |> rlist::list.save('benchmarks/cppr_networks/dodgr_cycle.rds')
}
### dodgr_walk
if(file.exists('benchmarks/cppr_networks/dodgr_walk.rds')) {
  cat('DODGR walk network already exists.\n'
      ,'If you want to regenerate, erase first\n')
} else {
  dodgr_walk <- london_all_edges[,.(from,to,highway,osmid,length,geom)] |> 
    sf::st_as_sf() |>
    dodgr::weight_streetnet(wt_profile = 'foot',id_col = 'osmid')
  
  dodgr_walk <- as.data.table(dodgr_walk)
  
  dodgr_walk_edges <- dodgr_walk[component==1,.(id=edge_id,from=from_id,to=to_id,length=d_weighted)]
  
  dodgr_walk_nodes <- data.table::rbindlist(list(
    dodgr_walk[component==1,.(osmid=from_id,x=from_lon,y=from_lat)]
    ,dodgr_walk[component==1,.(osmid=to_id,x=to_lon,y=to_lat)]
  ))
  
  dodgr_walk_nodes <- dodgr_walk_nodes[!duplicated(osmid),]
  
  dodgr_walk_cppr <- make_cppr_net(edges = dodgr_walk_edges
                                   ,nodes = dodgr_walk_nodes)
  
  dodgr_walk_cppr |> rlist::list.save('benchmarks/cppr_networks/dodgr_walk.rds')
}
gc()
