
# THIS SCRIPT IS VERY OLD ! IT CONTAINS SOME INITIAL TEST ON THE ROUTING, AND VARIOUS CENTROID RELATED THINGS. 
# NOT RECOMMENDED TO TRYING RUNNING IT. 

library(data.table)
library(dodgr)
library(RcppParallel)
library(cppRouting)

#### London network and flows aggregating


london_network <- dodgr::weight_streetnet(london_edges_dt %>% st_as_sf()
                                          ,wt_profile = "bicycle")

flows_london <- flows[ (residence %in% london_msoa$geo_code &
                        workplace %in% london_msoa$geo_code) & bike != 0,]


flows_graph_london <- dodgr_flows_aggregate(london_network
                                            ,from = flows_london$geom_from[1:3000] %>% 
                                              st_coordinates()
                                            ,to = flows_london$geom_to[1:3000] %>% 
                                              st_coordinates()
                                            ,flows = flows_london$bike[1:3000])

dodgr_flowmap(flows_graph_london, linescale = 10)

dodgr_flowmap(flows_graph)


# this function is copy pasted from the github of dodgr 
# and slightly modified to plot the flows prettier

dodgr_flowmap <- function (net, bbox = NULL, linescale = 1) {
  
  if (!"flow" %in% names (net))
    net$flow <- 1
  if (is.null (bbox))
    bbox <- c (min (net$from_lon), min (net$from_lat),
               max (net$from_lon), max (net$from_lat))
  
  xlims <- c (bbox [1], bbox [3])
  ylims <- c (bbox [2], bbox [4])
  #cols <- colorRampPalette (c ("lawngreen", "red")) (30)
  plot (NULL, xlim = xlims, ylim = ylims, xlab = "lon", ylab = "lat")
  net <- net [which (net$flow > 0), ]
  net$flow <- net$flow / max (net$flow)
  ncols <- 10
  cols <- colorRampPalette (c ("grey", "purple")) (ncols)
  cols <- cols [ceiling (net$flow * ncols)]
  
  with (net, segments (from_lon, from_lat, to_lon, to_lat,
                       col = cols
                       ,lwd = 1#linescale * net$flow
  ))
}



#### Constructing the network and working with the flows in Brighton ####
# dodgr package
## Reading files for brithgon in the ped_network.R file.

brighton_network <- dodgr::weight_streetnet(brighton_edges_dt %>% st_as_sf()
                                            ,wt_profile = "bicycle")

#  selecting the biggest connected component
brighton_network <- brighton_network[brighton_network$component == 1, ]

brighton_centroid <- brighton_zones %>% 
  st_as_sf() %>% 
  st_centroid() %>% 
  st_coordinates()

brighton_euclid_dist <- brighton_zones %>% 
  st_as_sf() %>% 
  st_centroid() %>% 
  st_distance() %>%
  set_units(NULL) # units are in meters everywhere.

# set the max number of cores to use 
# in the dodgr function

RcppParallel::setThreadOptions(numThreads = 3)

brighton_network %>% head()

brighton_dists <- dodgr_dists(brighton_network
                              ,from = brighton_centroid
                              ,to = brighton_centroid
                              ,shortest = FALSE
                              ,quiet = FALSE
                              ,parallel = TRUE
)

which(is.na(brighton_dists), arr.ind = TRUE) %>% unique()

# the zones that have NA values for distances
brighton_zones %>% 
  st_as_sf() %>% 
  qtm(bbox = brighton_bbox) + 
  qtm(brighton_zones[c(16,36,44,47,63,112,130,98,99,63,44,47),] %>% 
        st_as_sf()
      ,fill = "red")

# not necessary
# colnames(brighton_dists) <- brighton_zones$geo_code
# rownames(brighton_dists) <- brighton_zones$geo_code
(brighton_dists/brighton_euclid_dist) %>% hist()

# the fraction of the computed trips that were more than 3 times greater
# than the Euclidean distance between the MSOA
# centroids is roughly 0.004, shall we consider this as negligible ?

length(which((brighton_dists/brighton_euclid_dist) > 3))/length(brighton_dists)

# these next lines will get the corner cases of path that are very big compared to 
# the euclidean distance between the points and plot them to check for consistency. 
corner_cases_brighton <- which((brighton_dists/brighton_euclid_dist) > 3,arr.ind = TRUE)
case <- corner_cases_brighton[sample(1:nrow(corner_cases_brighton),10),]

case_from <- brighton_centroid[case[,1],]
case_to <- brighton_centroid[case[,2],]

corner_paths <- dodgr::dodgr_paths(brighton_network
                                   ,from = case_from
                                   ,to = case_to
                                   ,vertices = FALSE
                                   ,pairwise = TRUE)

j <- sample(length(corner_paths)
            ,1)

from <- brighton_network[corner_paths[[j]][[1]],c("from_lon","from_lat")]
to <- brighton_network[corner_paths[[j]][[1]],c("to_lon","to_lat")] 

test <- get_lines(from = from
                   ,to = to)
#tmap_mode("view")
test %>% qtm(fill = "orange")

# from_ind <- which((brighton_dists-brighton_euclid_dist) > 20000,arr.ind = TRUE)[,1]
# to_ind <- which((brighton_dists-brighton_euclid_dist) > 20000,arr.ind = TRUE)[,2]
#  
# from <-  brighton_zones %>% st_as_sf() %>% st_centroid()
# to <-  brighton_zones %>% st_as_sf() %>% st_centroid()

flows_brighton <- flows[residence %in% brighton_zones$geo_code & 
                          workplace %in% brighton_zones$geo_code,]


# flows_brighton[residence == workplace,bike]

flows_graph <- dodgr_flows_aggregate(brighton_network
                                     ,from = flows_brighton$geom_from %>% st_coordinates()
                                     ,to = flows_brighton$geom_to %>% st_coordinates()
                                     ,flows = flows_brighton$bike)

flows_graph$flow <- log(1+flows_graph$flow)

flows_brighton

dodgr_flowmap(flows_graph, linescale = 30)

#### making the flow matrix for Brighton

# there is probably a more efficient way to do this, but it works... 
registerDoParallel(cores = 3)
brighton_flows_matrix <- foreach(i = 1:length(brighton_zones$geo_code)
                                 ,.combine = rbind) %dopar% {
  x <- rep_along(brighton_zones$geo_code,0)
  for (j in 1:length(brighton_zones$geo_code)) {
    
    n <- flows_brighton[residence == brighton_zones$geo_code[i] & 
                          workplace == brighton_zones$geo_code[j],bike]
    if (length(n)!=0){
      x[j] <- n
      }
  }
  x
}
stopImplicitCluster()

# saved to send to Valentina
# list.save(brighton_zones$geo_code, "brighton_geocode.rds")

# attempt to make the flows matrix with dcast and melt was failed. 
# this is the more efficient way to do it. 
flows_brighton %>% acast(formula = residence ~ workplace
                                    ,value.var = "bike"
                                    ,fill = 0)

# plot(brighton_euclid_dist
#      ,brighton_flows_matrix
#      ,log = "xy"
#      )

isSymmetric.matrix(brighton_euclid_dist)
isSymmetric.matrix(brighton_flows_matrix)

# this plot helps visualize the deviation between the Euclidean distance and 
# the shortest path between all the pairs of MSOAs centroids.

pdf("images/brighton_dists.pdf")
plot(brighton_euclid_dist,brighton_dists
     #,log = "xy"
     ,col = "navyblue"
     #,asp = 1
     ,pch = 20
     ,xlab = "crow-fly distance, m"
     ,ylab = "network distance, m"
     ,main = "deviation between shortest path and striaght line distances"
     ,cex = 1.4
     )
lines(1:max(brighton_euclid_dist)
      ,col = "darkred"
      ,lwd = 2)
legend(x = 60000
       ,y = 2e4
       ,legend = c("y=x")
       ,cex = 1.3
       ,col = c("darkred")
       ,lwd = 2)
dev.off()

###
### CPP routing ####

# the presence of "selected" in the name of a variable means
# it contains data for the test that is done on selected msoas
# in central london: the city, islington, camden

# this creates a logical matrix of what node belongs to what msoa
predicate <- london_nodes_dt %>% 
  st_as_sf() %>%
  st_intersects(london_selected_msoa[,geometry] %>% st_as_sf()
                ,sparse = FALSE)

# this reduces the matrix along each line to one column vector 
# where TRUE is kept if it is present in a row else FALSE. 
x <- predicate %>% 
  apply(FUN = function(x) reduce(x,or)
        ,MARGIN = 1)

# the following takes a data table with spatial lines,
# extracts all the points in the lines, and only keeps the 1st and the last. 
# this evens out the lines and reduces the number of 
# nodes and segments that constitute a "line", 
# we still keep the information about the weight of 
# the line so in terms of shortest path 
# calculation it should not change the results. 

# surprisingly, using tidyverse is faster than data.tables,
# probably because doing something wrong 
london_edges <- london_selected_edges %>% 
  st_as_sf() %>%
  st_coordinates() %>% 
  as_tibble() %>%
  group_by(L1) %>% 
  summarise(res = cbind(X[1],Y[1],X[n()],Y[n()]))

# keep only the relevant column to construct the graph with cppRouting
london_edges[,c("from","to","length")] <- london_selected_edges[,c("from","to","length")]

# only the endpoints of lines are accepted as nodes, 
# so the following line extracts them from the set of edges. 

london_nodes <- rbind(london_edges[,.(node_ID = from,x = res.V1,y = res.V2)]
                      ,london_edges[,.(node_ID = to,x = res.V3,y = res.V4)]) %>% 
  unique() # store each node only once. 

london_selected_nodes <- london_nodes_dt[x,]
london_selected_nodes <- london_selected_nodes[,.(node_ID=osmid,X=x,Y=y)]

london_selected_edges <- as.data.table(london_selected_edges)
london_selected_nodes[,node_ID] %>% unique() %>% length()
# london_selected_edges[,st_coordinates(geom)]

london_selected_graph <- london_edges[,.(from,to,length)]
# we don't have enough info on the edges to make a directed graph, so the assumption 
# is taken that you can cycle in both direction on any edge
london_selected_graph <- london_selected_graph %>% makegraph(directed = FALSE
                                                             ,coords = london_nodes)
# simplifying the graph to test
london_selected_graph_simple <- london_selected_graph %>%
  cppRouting::cpp_simplify(rm_loop = TRUE
                           ,iterate = FALSE) # iterating may cause changes that we don't want

# 
# use the function "find_nearest_node_on_graph" instead of the code below
# 
# 
# london_selected_msoa_cent <- london_selected_msoa %>% 
#   st_as_sf() %>%
#   st_centroid()
# 
# msoa_dist <- london_nodes %>% 
#   st_as_sf(coords = c("x","y"), crs = 4326) %>% 
#   st_distance(london_selected_msoa_cent) %>% 
#   units::set_units(NULL)
# 
# msoa_ind <- foreach(i = 1:ncol(msoa_dist),.combine = c) %do% {
#  which(msoa_dist[,i] == min(msoa_dist[,i]), arr.ind = TRUE)
# }

# comparig when using the normal graph or simplified one. 
orig <- london_selected_graph_simple %>%
  find_nearest_node_on_graph(london_selected_msoa_cent)

orig_simple <- london_selected_graph_simple %>%
  find_nearest_node_on_graph(london_selected_msoa_cent)

dist_matrix_selected <- get_distance_matrix(london_selected_graph, orig, orig)
dist_matrix_simple <- get_distance_matrix(london_selected_graph_simple, orig_simple, orig_simple)

# we see that the differences are almost 0.
(dist_matrix_selected-dist_matrix_simple) %>% hist()


simple_vs_normal <- benchmark("normal_graph" = {get_distance_matrix(london_selected_graph, orig, orig)}
                              ,"simple_graph" = {get_distance_matrix(london_selected_graph_simple
                                                                     ,orig_simple
                                                                     ,orig_simple)}
                              ,replications = 10)
# it seems better to use the simplified graph for 
# performance gain with almost zero precision gain.
simple_vs_normal

# make a matrice with euclidean distances between points to see if the 
# routes do not deviate to much from the straight line 

dist_matrix_ref <- london_nodes[msoa_ind,] %>% 
  st_as_sf(coords = c("x","y"),crs = 4326) %>%
  st_distance() %>% 
  units::set_units(NULL)

# we see that for most of the cases the length is about x1.4 times higher than the straight line, 
# but for a small number of cases it goes up to x2.9
((dist_matrix_simple)/dist_matrix_ref) %>% hist()

#### cppRouting the msoas in london ####
# The MSOAs at until now were only those that have a santander cycle station in them.
# creating the cppRouting graph from london edges:
# reusing old variable names to save memory. rerun previous code if necessary. 

london_edges <- london_edges_dt[,st_coordinates(geom)] %>% as.data.table()

# london_edges <- london_edges[,.SD[c(1,.N)],by = L1]

## Parenthesis with performance test between data.table and tidyverse:
# london_edges_tidy <- london_edges %>% as_tibble()
# 
# benchmark(data.tab = london_edges[1:1000,.(X1 = .SD[1L,X],Y1 = .SD[1L,Y],X2 = .SD[.N,X],Y2 = .SD[.N,Y]),by = L1]
#           ,tidy = london_edges_tidy[1:1000,] %>%
#             group_by(L1) %>% 
#             summarise(res = cbind(X[1],Y[1],X[n()],Y[n()]))
#           ,replications = 5)
# parenthesis closed, tidy seems mode than 10 times faster...

# london_edges <- london_edges[,.(X1 = .SD[1L,X],Y1 = .SD[1L,Y],X2 = .SD[.N,X],Y2 = .SD[.N,Y]),by = L1]

london_edges <- london_edges %>% 
  as_tibble() %>%
  group_by(L1) %>%
  summarise(res = cbind(X[1],Y[1],X[n()],Y[n()]))

# set node ids manually to simplify usage. 
node_ids <- c(london_edges_dt[,to],london_edges_dt[,from]) %>% unique()
node_ids <- data.table(id = node_ids
                ,num_id = 1:length(node_ids))

# keep only the relevant column to construct the graph with cppRouting
london_edges[,c("from","to","length")] <- london_edges_dt[,c("from","to","length")] %>%
  as.data.table()

london_edges <- london_edges %>% merge.data.table(node_ids
                                       ,by.x = "from"
                                       ,by.y = "id")
?merge.data.table
setnames(london_edges
         ,old = "num_id"
         ,new = "from_num")

london_edges <- london_edges %>% merge.data.table(node_ids
                                       ,by.x = "to"
                                       ,by.y = "id")

setnames(london_edges
         ,old = "num_id"
         ,new = "to_num")

# if needed to reassign the class
# london_edges <- london_edges %>% as.data.table()
# save the file locally
# london_edges %>% fwrite(file = "london_edges.csv")

# read in the file with edges of the london graph
# london_edges <- fread("london_edges.csv")

# getting the nodes from the edges with custon ids. 
london_nodes <- rbind(london_edges[,.(node_ID = from_num,X = res.V1,Y = res.V2)]
                      ,london_edges[,.(node_ID = to_num,X = res.V3,Y = res.V4)]) %>% 
  unique()

london_graph <- london_edges[,.(from = from_num,to = to_num,length)]

london_graph <- london_graph %>% makegraph(directed = FALSE
                                           ,coords = london_nodes)

# simplifying the graph to test
london_graph_simple <- london_graph %>%
  cppRouting::cpp_simplify(rm_loop = TRUE
                           ,iterate = FALSE)

#sf_use_s2(FALSE)

profvis({
  centroid_nodes <- london_graph_simple %>%
    find_nearest_node_on_graph(central_centroid, n_cores = 3)
})

dist_matrix_london <- get_distance_matrix(london_graph_simple
                                          ,from = centroid_nodes
                                          ,to = centroid_nodes
                                          ,allcores = TRUE)

paths_centroid_london <- get_multi_paths(london_graph_simple
                                         ,from = centroid_nodes
                                         ,to = centroid_nodes
                                         #,long = TRUE
                                         #,keep = TRUE
                                         )

# paths_grouped <- paths_centroid_london %>% 
#   dplyr::group_by(from,to) %>% 
#   summarise(path = list(node))

dist_matrix_london %>% 
  as.data.frame() %>%
  as.data.table(keep.rownames = TRUE) %>% 
  fwrite(file = "central_london_distance_matrix.csv"
         ,row.names = TRUE
         ,col.names = TRUE)

# # seems to work
# test 
# # should read the data correctly, but with an additional first column of indices and with one names rn for row names.
# test <- fread("central_london_distance_matrix.csv"
#               ,nrows = 100)

#### The euclidean distance between msoas in London #### 

london_dist_test <- st_distance(central_centroid %>% st_transform(27700)
                             ,central_centroid %>% st_transform(27700) 
                             ) %>%
  set_units(NULL)

london_central_msoa_dist <- st_distance(central_centroid
                                        ,central_centroid
                                        ) %>%
  set_units(NULL)

# testing the differences in distances with different crs: 4326 and 27700
# (london_dist_test-london_central_msoa_dist) %>% hist()

(dist_matrix_london/london_central_msoa_dist) %>% hist()
# the difference looks like a consequence of rounding and is 
# very small

# This part looks at the corner cases
# where the shortest path was shorter than the
# euclidean distance. It appears that it is due to the fact that in a small number of
# cases the nearest nodes to the respective centroids were actually closer
# than the centroids and that the path was relatively straight so it remained under
# the distance.
# one way to avoid that is to recompute euclidean distance between the nodes
# instead of the centroids

# ratios of >2.5x seem to be corner cases and outliers
corner_cases <- which((dist_matrix_london/london_dist_test)>2.5, arr.ind = TRUE)

corner_cases_lines <- foreach(i = 1:nrow(corner_cases)
                              ,.combine = rbind
                              ) %do% {
  get_lines(
    central_centroid[corner_cases[i,1],]
    ,central_centroid[corner_cases[i,2],])
}

tmap_mode("view")

corner_cases_lines %>% st_length()

corner_cases_lines %>% qtm()

i <- 5
x <- paths_centroid_london[[corner_cases[i,1]]][[corner_cases[i,2]]]

path_corner_case <- coords[match(x,node_ID),.(X,Y)] %>%
  get_lines(to = NULL) %>%
  st_sfc(crs = 4326)

# path_corner_case %>% st_length()
# corner_cases_lines[3,] %>% st_length()

path_corner_case %>% qtm(col = "red") + qtm(corner_cases_lines[i,]
                                                ,col = "black")

length(central_centroid)

#### mapping paths #### 

coords <- london_graph_simple$coords %>% as.data.table()

i <- 4
# matching the nodes in the right order takes some extra time
# with respect to just extracting the corresponding nodes in the order in which
# they appear in the data
benchmark(
  "in" = coords[node_ID %in% paths_centroid_london[[i]][[1]],.(X,Y)]
  ,"match" = coords[match(paths_centroid_london[[i]][[1]],node_ID),.(X,Y)]
  ,replications = 10
)

tmap_mode("view")

# the following code is very long to run and not very clear if it is usefull 
# can be usefull if run for small numbers of values just to visualize the paths
# registerDoParallel(cores = 3)
# shortest_paths <- foreach(i = 1:2 #nrow(paths_grouped)
#         ,.combine = c
#         ,.final = st_sfc) %dopar% {
#           coords[match(paths_grouped$path[[i]],node_ID),.(X,Y)] %>% 
#             get_lines(to = NULL) 
#         }
# stopImplicitCluster()
# 
# shortest_paths <- shortest_paths %>% 
#   st_as_sf(crs = 4326) %>%
#   dplyr::filter(!st_is_empty(x))
#
# shortest_paths[1:10,] %>% qtm()

#### Mapping ####

path_1 <- paths_centroid_london[[i]] %>% map(function(x) {coords[match(x,node_ID),.(X,Y)] %>% get_lines(to = NULL)}
) %>% 
  st_sfc(crs = 4326)

bb <- london_msoa[as.numeric(predicate)] %>% st_as_sf() %>% st_union()
bb %>% qtm()

pred <- st_intersects(bb,london_edges_dt %>% st_as_sf())

pred <- pred[[1]] %>% as.numeric() %>% unique()

london_central_roads <- london_edges_dt[pred,]

london_central_roads %>% dim()

central_centroid[4,]

path_1_map <- tm_shape(thames, bbox = path_1 %>% st_bbox()) + tm_lines(col = "darkblue") + 
  tm_shape(london_central_roads %>% st_as_sf()) + tm_lines(col = "grey"
                                                           ,lwd = 0.5) +
  tm_shape(path_1) + tm_lines(lwd = 2) + 
  tm_shape(central_centroid) + tm_dots(col = "orange"
                                       ,size = 0.25) +
  tm_shape(central_centroid[4,]) + tm_dots(col ="red"
                                           ,size = 0.25) +
  tm_layout(main.title = "Shortest paths from Camden 019"
            ,main.title.size = 1) + 
  tm_scale_bar(breaks = c(0,2.5,5))

tmap_save(
  path_1_map
  ,"images/shortest_paths_E09000007.pdf"
)


#### routing #### 

centroid_nodes_london <- list.load("RC_outputs/241556.undefined/centroid_nodes.rds")

london_msoa$graph_id <- centroid_nodes_london

#### Routing across all of london ####

centroid_distance <- centroid_msoa %>% 
  st_distance() %>% 
  set_units(NULL)

links_of_interest <- list.load("RC_outputs/tmpdir 2/links_of_interest.rds")

links_of_interest %>% nrow

coords_new <- london_graph_simple$coords
centroid_nodes_london 

samp_1 <- london_msoa$graph_id[sample(1:nrow(london_msoa),100)]
samp_2 <- london_msoa$graph_id[sample(1:nrow(london_msoa),100)]

distances_graph <- get_distance_pair(london_graph_simple
                                     ,from = samp_1
                                     ,to = samp_2
                                     ,allcores = FALSE
                                     )

distances_graph %>% hist()

paths_graph <- get_path_pair(london_graph_simple
                             ,from = samp_1
                             ,to = samp_2
                             #,allcores = FALSE
                             )

paths_graph

paths_sf <- paths_graph %>% 
  lapply(function(x) {coords_new[match(x,node_ID),.(X,Y)] %>% 
    get_lines(to = NULL)
  }) %>% 
  st_sfc(crs = 4326)

# tmap_mode("view")
paths_sf %>% qtm(lines.col = "red")

# centr_dist <- foreach(i = 1:nrow(links_of_interest)
#                       ,.combine = c) %do% {
#                         centroid_distance[links_of_interest[i,1],links_of_interest[i,2]]
#                       }

from <- london_graph_simple$coords[links_of_interest[,1]] %>% 
  st_as_sf(coords = c("X","Y")
           ,crs = 4326)

to <- london_graph_simple$coords[links_of_interest[,2]] %>% 
  st_as_sf(coords = c("X","Y")
           ,crs = 4326)

centr_dist <- st_distance(from,to,by_element = TRUE) %>%
  set_units(NULL)

# 
# links_of_interest
# 
# distances_graph <- 
#   get_distance_pair(london_graph_simple
#                     ,from = london_graph_simple$coords$node_ID[links_of_interest[,1]]
#                     ,to = london_graph_simple$coords$node_ID[links_of_interest[,2]]
#                     ,allcores = TRUE
#                     )

distances_graph <- list.load("RC_outputs/tmpdir/distances_graph.rds")

distances_graph %>% length()

hist(centr_dist)

x <- sample(1:length(distances_graph),15000)

pdf("images/london_dists.pdf")
plot(centr_dist[x],distances_graph[x]
     ,col = "navyblue"
     ,pch = 20
     #,log = "xy"
     ,cex = 0.7
     ,cex.lab = 1.2
     ,main = "network and crowfly distance comparison, London"
     ,xlab = "crow-fly distance, m"
     ,ylab = "network shortest distance, m")
lines(1:max(centr_dist)
      ,1:max(centr_dist)
      ,col = "darkred"
      ,lwd = 2)
legend(x = "bottomright"
       #,y = 1000
       ,legend = c("y=x")
       ,cex = 1.3
       ,col = c("darkred")
       ,lwd = 2)
dev.off()

#### functions ####

## testing performance gain from parallelising st_dist
# sf_use_s2(TRUE)
# benchmark_par_st_dist <- benchmark(
#   test_serial = st_distance(x %>% st_transform(27700)
#                             ,y %>% st_transform(27700)
#                             ,tolerance = 100) %>% 
#     units::set_units(NULL)
#   ,test_par = par_st_dist(x,y,cores = 3,tolerance = 100,coords = 27700)
#   ,replications = 5
# )
# benchmark_par_st_dist
#performance test becomes convincing when doing 20000 and 150 points with a 2/3 time 
#compared to the serial implementation with base sf


# parallel computations using sf_function distance
par_st_dist <- function(x,y,cores = 1,tolerance = 100, coords = NULL) {
  if(!is.null(coords)) {
    x <- x %>% st_transform(coords)
    y <- y %>% st_transform(coords)
  }
  k <- trunc(nrow(x)/cores)
  registerDoParallel(cores = cores)
  res <- foreach(i=1:cores,.combine = rbind) %dopar% {
    st_distance(x[((i-1)*k+1):(i*k),]
                ,y
                ,tolerance = tolerance) %>% 
      units::set_units(NULL)
  }
  stopImplicitCluster()
  # compute distance for the remaining values that are omitted 
  # because of the possible remainders of division for k
  if(k*cores != nrow(x)){
    res_2 <- st_distance(x[(k*cores+1):nrow(x),]
                         ,y
                         ,tolerance = tolerance) %>% 
      units::set_units(NULL)
    res <- rbind(res,res_2)
  }
  res
}


find_nearest_node_on_graph <- function(graph, points_data = NULL, n_cores = 1) {
  if(is.null(points_data)) stop("Provide sf spatial points to link to the network.")
  
  points_dist <- graph$coords %>% 
    st_as_sf(coords = c("X","Y"), crs = 4326) %>% 
    par_st_dist(points_data %>% st_as_sf()
                ,cores = n_cores
                ,tolerance = 300
                ,coords = 27700)
  print("finished computing distances")
  print(as.character(points_dist %>% dim()))
  registerDoParallel(cores = n_cores)
  node_ind <- foreach(i = 1:ncol(points_dist),.combine = c) %dopar% {
    which(points_dist[,i] == min(points_dist[,i]), arr.ind = TRUE)
  }
  stopImplicitCluster()
  graph$coords$node_ID[node_ind]
  
}


get_lines <- function(from, to = NULL, by_element = TRUE) {
  # check that from and to are points as well
  
  if(!is.null(to)) {
    # projection <- st_crs(from)
    if(("sf" %in% class(to)) & ("sf" %in% class(from))) {
      from <- from %>% st_as_sf() %>% st_coordinates()
      to <- to %>% st_as_sf() %>% st_coordinates()
    }
    
    from_to <- cbind(from,to) %>% as.matrix()
    new_lines <- foreach(i = 1:nrow(from_to)
                         ,.combine = c
                         ,.final = st_as_sf) %do% {
                           from_to[i,] %>%
                             matrix(ncol = 2, byrow = TRUE) %>%
                             st_linestring(dim = "XY") %>%
                             st_sfc(crs = 4326)
                         }
  } else if (is.null(to)) {
    # foreach(i = 1:length(from)
    #         ,.combine = c
    #         ,.final = st_as_sf) %do% {
              from %>%
                as.matrix(ncol = 2, byrow = TRUE) %>%
                st_linestring(dim = "XY") 
    # %>%
    #             st_sfc(crs = 4326)
            #}
  }
  
}

#


