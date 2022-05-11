library(rlist)
library(sf)
library(tmap)
library(tmaptools)
library(data.table)
library(tidyverse)
library(units)
library(cppRouting)
library(foreach)
library(doParallel)
library(jsonlite)

#### Reading the output files from the RC_outputs
## More on where the centroids come from 
# https://geoportal.statistics.gov.uk/documents/population-weighted-centroids-guidance/explore 


dist_matrix_cwp <- list.load("RC_outputs/pop_weighted_centroids/dist_matrix_london.rds")

distances_graph_cwp <- 
  list.load("RC_outputs/pop_weighted_centroids/distances_graph.rds")

centroid_nodes_cwp <- list.load("RC_outputs/pop_weighted_centroids/centroid_nodes.rds")
# adding the variable about graph node id to the pop
# weighted centroids
msoa_pop_centr$graph_id <- centroid_nodes_cwp

# see how many graph nodes correspond with two different centroids of MSOAs
intersect(centroid_nodes_cwp, london_msoa$graph_id)

# there are many values that appear to be below the y = x line. 
# this is impossible in real world 2d networks,
# this issues comes from the fact that some network node pairs are closer to each other than 
# the spatial points to which they are assigned
# to avoid having this problem, compute euclidean distances between 
# the coordinates of the network nodes instead
msoa_graph_dist <- coords_new[match(msoa_pop_centr$graph_id,coords_new$node_ID),] %>% 
  st_as_sf(coords = c("X","Y"),crs = 4326) %>% 
  st_distance() %>%
  set_units(NULL)

msoa_graph_dist %>% list.save("gravity_model_files/msoa_graph_dist.rds")

# euclidean distances between the pop weighted centroids. 
# centroid_distance_cwp <- st_distance(msoa_pop_centr %>% st_as_sf()) %>%
#   set_units(NULL)

# take a look at the distribution of trip length divided by euclidean distance between points
# good way to get a first understanding of how the network performs
# expected to see most of the values between 1 and 2. Values above are considered outliers. 
(dist_matrix_cwp/msoa_graph_dist) %>% hist

outliers_cwp <- which((dist_matrix_cwp/msoa_graph_dist) > 12, arr.ind = TRUE)

# distances_graph_out_cwp <- 
#   get_distance_pair(london_graph_simple
#                     ,from = centroid_nodes_cwp[outliers_cwp[,1]]
#                     ,to = centroid_nodes_cwp[outliers_cwp[,2]]
#                     ,allcores = TRUE)

# x <- sample(1:nrow(outliers_cwp),200)

paths_graph_out_cwp <- 
  get_path_pair(london_graph_simple
                ,from = msoa_pop_centr$graph_id[outliers_cwp[,1]]
                ,to = msoa_pop_centr$graph_id[outliers_cwp[,2]]
                )

#### Plotting the outliers ####
# almost all of them are in east london and consist of roads 
# traveling around to get from one side of the river to the other. 

paths_graph_out_cwp %>% length()

paths_sf_swp <- paths_graph_out_cwp[sample(1:length(paths_graph_out_cwp),2)] %>% 
  lapply(function(x) {coords_new[match(x,node_ID),.(X,Y)] %>% 
      get_lines(to = NULL)
  }) %>% 
  st_sfc(crs = 4326)

# tmap_mode("view")
paths_sf_swp %>% qtm()

####

# the code below was used to verify something:
# you can pass to a matrix another matrix with two column in which 
# each row will contain the row and column coordinates of a value we want
# distances_graph_cwp <- foreach(i = 1:nrow(links_of_interest)
#                                ,.combine = c) %do% {
#                                  dist_matrix_cwp[links_of_interest[i,1]
#                                                  ,links_of_interest[i,2]]
#                                }
#   
# identical(dist_matrix_cwp[links_of_interest]
#           ,distances_graph_cwp)

# not run this any more 
# dist_matrix_cwp <- dist_matrix_cwp[match(london_msoa$geo_name, msoa_pop_centr$msoa11nm),] %>%
#   .[,match(london_msoa$geo_name, msoa_pop_centr$msoa11nm)]

jpeg("images/light/pop_weighted_vs_crowfly_comp.jpg"
     ,height = 5.83
     ,width = 5.83
     ,quality = 80
     ,units = "in"
     ,res = 150)

plot(msoa_graph_dist[links_of_interest]
     ,dist_matrix_cwp[links_of_interest]
     ,col = "navyblue"
     ,pch = 20
     #,log = "y"
     ,cex = 0.3
     ,cex.lab = 1.2
     ,main = "pop weighted  and crowfly distance comparison"
     ,xlab = "crow-fly distance, m"
     ,ylab = "network shortest distance, m")
lines(1:max(centr_dist)
      ,1:max(centr_dist)*sqrt(2)
      ,col = "darkorange"
      ,lwd = 2)
lines(1:max(centr_dist)
      ,1:max(centr_dist)
      ,col = "darkred"
      ,lwd = 2)
legend(x = "bottomright"
       #,y = 1000
       ,legend = c("f(x) = 1.414x"
                   ,"f(x) = x"
                   #,"f(x) = 1.236x"
                   )
       ,cex = 1.3
       ,col = c("orange"
                ,"darkred"
                #,"darkgreen"
                )
       ,lwd = 2)

dev.off()

# plotting the ratio of the two distances as computed from the msoa centroids 
# and msoa pop weighted centroids

jpeg("images/light/pop_weight_vs_centroid_dist.jpg"
     ,height = 5.83
     ,width = 5.83
     ,quality = 80
     ,units = "in"
     ,res = 150)

plot(dist_matrix_cwp[links_of_interest]
     ,distances_graph
     ,col = "navyblue"
     ,pch = 20
     ,log = "xy"
     ,cex = 0.3
     ,cex.lab = 1.2
     ,main = "population weighted and centroid nodes network distance, London"
     ,cex.main = .8
     ,xlab = "pop weighted centroid distance, m"
     ,ylab = "centroid distance, m")
lines(1:max(distances_graph)
      ,1:max(distances_graph)
      ,col = "darkred"
      ,lwd = 2)

dev.off()




