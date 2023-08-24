# A generation of typical mean values of detour for randomly selected origin and destination
# in the network using a Monte Carlo like process.

library(rlist)
library(data.table)
library(sf)
library(Btoolkit)
library(cppSim)
library(foreach)


### OS

os_graph <- rlist::list.load("../Benchmarks/cppr_networks/os_graph.rds")

##### OSM
osm_graph <- rlist::list.load("../Benchmarks/cppr_networks/cppr_all.rds")

##### FUNCTIONs #####
# a function that does Monte Carlo and accepts a threshold so that we can use a max or min value
# on the reference distance, crow fly in this case.

detour_index_mc <- function(graph,threshold = 15000,threads = 1, samp_size = 100,iterations = 50) {
  # Add this to Btoolkit at some point
  detour_random <- list()
  w = list()
  RcppParallel::setThreadOptions(numThreads = threads)
  for(i in 1:iterations) {
    sample_id <- graph$coords[sample(1:nrow(graph$coords),samp_size),][!duplicated(osmid),]
    # crow fly distance
    crow_fly <- sf::st_distance(sample_id |> 
                                  sf::st_as_sf(coords = c(2,3),crs = 4326), by_element = FALSE) |> 
      units::set_units(NULL)
    
    # put 1 on the diagonal so when we divide we don't get NaNs
    crow_fly <- `diag<-`(crow_fly,1)
    
    #####
    
    dist_mat <- cppRouting::get_distance_matrix(graph
                                                ,from = unlist(sample_id[,1])
                                                ,to = unlist(sample_id[,1])
    )
    
    # dist_mat <- `diag<-`(dist_mat,15001)
    #####
    # detour for meaningfull at values < 15000 m 
    if(is.null(threshold)){
      detour_random_i <<- (dist_mat/crow_fly)
    } else {
      detour_random_i <<- (dist_mat[crow_fly<threshold]/crow_fly[crow_fly<threshold])
    }

    detour_random <- append(detour_random, mean(detour_random_i[detour_random_i!=0]))
    w = append(w,length(detour_random_i))
  }
  RcppParallel::defaultNumThreads()
  
  return(data.frame('detour' = unlist(detour_random),'weight'= unlist(w)))
}

# 
# this functino was added to Btoolkit
# 
# get_lcc <- function(ways, graph_mode = "weak") {
#   # add this to btoolkit at some point
#   # require("igraph")
#   
#   stopifnot("data.table" %in% class(ways), "from" %in% colnames(ways), "to" %in% colnames(ways))
#   
#   igraph_ways <- igraph::graph_from_data_frame(ways[,.(from,to)],directed = FALSE)
#   
#   if(igraph_ways |> igraph::is_connected(mode = graph_mode)) {stop("Already a connected graph")}
#   
#   nodes_comp <- igraph::components(igraph_ways,mode = graph_mode)
#   
#   vert_ids <- igraph::V(igraph_ways)[nodes_comp$membership == which.max(nodes_comp$csize)]$name
#   
#   return(ways[from %in% vert_ids & to %in% vert_ids,])
# }

# the simulation from 22 march 2023 gave the result for the mean : 1.189598
# only for OD that are less than 15000 m crow-fly from each other. 
#### DETOUR for osm #####

detour_random_osm <- detour_index_mc(osm_graph
                                     ,threshold = 15000
                                     ,threads = 3
                                     ,samp_size = 100
                                     ,iterations = 100)
detour_random_osm

detour_random_osm |> mean()
detour_random_osm |> sd()

#####
# detour without distance threshold
# 
# detour_random <- (dist_mat/crow_fly)
# 
# plot(crow_fly[detour_random!=1]
#      ,detour_random[detour_random!=1]
#      ,log = "x"
#      ,pch = 20
#      ,ylab = expression(delta)
#      ,xlab = expression(d)
#      ,cex.lab = 1.4
#      ,main = "Detour index compared to crow-fly distance")

########
# detour for OS

# detour_index_os <- detour_index_mc(os_graph
#                                    ,threshold = 15000
#                                    ,threads = 3
#                                    ,iterations = 100
#                                    ,samp_size = 100)
# 
# print(paste0("Mean : ",detour_index_os |> mean()))
# print(paste0("SD : ",detour_index_os |> sd()))
# excluding, mean = 1.2918
