## Benchmarking networks
# 
library(data.table)
library(dodgr)
library(rlist)
library(sf)
library(cppRouting)


# THIS IS STILL A BIT MESSY, CLEAN !!!
# consider writing a function called for ex as_geo that takes a data.frame/table, and automatically turns it into a sf table.
# arguments could be the column name containing the geometry of interest in wkt, or other args passed to st_as_sf.
as_geo <- function(dt,colname='geometry', crs = 4326,...){
  
  if(inherits(dt,'character')){
    return(tryCatch(dt |> data.table::as.data.table() |> sf::st_as_sf(wkt=1,crs=crs,...)
                    ,error=function(e) cat('Failed, provide a vector containing wkt geometries.')
                    ,warning = function(w) print(w)))
  }
  # if the provided data set contains just one column, don't bother requiring colname to contain anything
  if(ncol(dt)==1) return(dt |> sf::st_as_sf(wkt=1,crs=crs,...))
  
  if(is.character(colname)) return(dt |> sf::st_as_sf(wkt=which(colnames(dt)==colname),crs=crs,...))
  else if (is.numeric(colname)) return(dt |> sf::st_as_sf(wkt=colname,crs=crs,...))
}

# networks are routed in cppr but with the weighting profiles 
# and segment selection corresponding to the mentioned packages for 
# specified mode of transport
# this script needs to run after routing_benchmarks.R

london_msoa <- rlist::list.load('data/london_msoa.rds')

#  does not work, the polygons seem to be corrupted
london_msoa[, "tip_dist"] <- london_msoa[,"geometry"] |>
  as_geo() |>
  # sf::st_cast('POLYGON',do_split = FALSE) |>
  sf::st_make_valid() |>
  sf::st_geometry() |>
  sf::st_area() |>
  sqrt() |>
  round() |>
  units::set_units(NULL)


# the nodes of the network for routing
nodes <- rlist::list.load('benchmarks/centroids.rds')

# all: the whole network with just motoroways removed. 
all_network <- rlist::list.load("benchmarks/cppr_networks/osm_all.rds")

all_network <- all_network |> cppRouting::cpp_contract()

#### OS network setup

os_cppr <- rlist::list.load("benchmarks/cppr_networks/os_graph.rds")

# os_nn <- find_nearest_node_on_graph(os_cppr$coords
#                                     ,london_msoa[,"centr_geom"] |> st_as_sf(wkt = 1,crs = 4326))

os_cppr_simple <- os_cppr |> cppRouting::cpp_contract()

#### cycle

## dodgr
dodgr_cycle <- rlist::list.load("benchmarks/cppr_networks/dodgr_cycle.rds")

dodgr_cycle <- dodgr_cycle |> cppRouting::cpp_contract()

## osmnx
# osmnx_cycle <- make_cppr_net(edges = london_edges_dt[,.(from,to,length)]
#                              ,nodes = london_nodes_dt[,.(osmid,x,y)])
# 
# osmnx_cycle$data$dist <- osmnx_cycle$data$dist |> round()
# 
# osmnx_cycle_noi <- find_nearest_node_on_graph(osmnx_cycle$coords
#                                               ,london_msoa[,"centr_geom"] |> st_as_sf(wkt = 1,crs = 4326))
# 
# osmnx_cycle <- osmnx_cycle |> cpp_contract()

#### walk

##dodgr

dodgr_walk <- rlist::list.load("benchmarks/cppr_networks/dodgr_walk.rds")

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

# checking that all networks are loaded:
# #### Saving the networks

# dodgr_walk
# osmnx_cycle
# osmnx_walk

#### Runnning distance matrices ####

# the reference distance is the euclidean one
distances_0 <- st_distance(london_msoa[,"centr_geom"]  |> as_geo()
                           ,by_element = FALSE) |> 
  round() |> 
  units::set_units(NULL)

distances_0 <- `diag<-`(distances_0,london_msoa[,tip_dist])

dist_0_interest <- which(distances_0 < 15000,arr.ind = TRUE)

#### distance matrices 
RcppParallel::setThreadOptions(numThreads = 6)

dist_1 <- get_distance_matrix(dodgr_walk
                            ,from = nodes$dodgr_walk_geom$from
                            ,to = nodes$dodgr_walk_geom$to
                            ) |> round()

# dist_2 <- get_distance_matrix(osmnx_walk
#                             ,from = 
#                             ,to = 
#                             ) |> round()

dist_3 <- get_distance_matrix(dodgr_cycle
                            ,from = nodes$dodgr_cycle_geom$from
                            ,to = nodes$dodgr_cycle_geom$to
                            )|> round()

# dist_4 <- get_distance_matrix(osmnx_cycle
#                             ,from = 
#                             ,to = 
#                             )|> round()

dist_5 <- get_distance_matrix(all_network
                            ,from = nodes$osm_all_geom$from
                            ,to = nodes$osm_all_geom$to
                            )|> round()

dist_6 <- get_distance_matrix(os_cppr_simple
                            ,from = nodes$os_geom$from
                            ,to = nodes$os_geom$to
                            )


####
distances <- list("dodgr_walk" = dist_1
                  #,"osmnx_walk" = dist_2
                  ,"dodgr_cycle" = dist_3
                  # ,"osmnx_cycle" = dist_4
                  ,"all_network_osm" = dist_5
                  ,"all_network_os" = dist_6)

distances <- distances |> lapply(function(m) `diag<-`(m,london_msoa[,tip_dist]))

# deltas <- list("delta_dodgr" = delta_dodgr_geom,"delta_osm" = delta_osm_geom,"delta_os" = delta_os_geom)

# lapply(deltas,FUN = function(x) x |> dim)

distances <- mapply(distances,deltas, FUN = function(mat,delt) {(mat[dist_0_interest]) #+ delt[dist_0_interest]
  })

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

