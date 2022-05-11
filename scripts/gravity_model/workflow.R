# this script takes a network, a flows data, and constructs everything from scratch to run a model based on some intput parameters. 

## loading data 

# edges
# edges

#nodes
# nodes

# spatial file of partition (administrative units)
# this file contains polygons of administrative units, but also various centroids that will be used for routing.
# london_msoa

## Constructing network

# parameters: 
# edges,nodes, simple
# cppr stuff 
# graph


## Routing (origins and destinations based on the centroids)
# which centroids to route as origin and destination
# params : centroid
# available options: geom_centr, pw_centr, graph_centr, ew_centr (employment weighted centr)

# finding the nearest node in the network to the centroid.
# geom_centr_node
# pw_centr_node
# graph_centr_node

## grav model 
# cost function: exp, pow, distance matrice
# flows: 

# function sketch :

# https://cloud.r-project.org/doc/manuals/r-release/R-lang.html read for inspiration

# merge city data and flows to assign ids from to to the flows.
# 
# graph <- london_graph_simple
# 
# city_data <- london_msoa[grep("Camden",geo_name),]
# 
# flows_london_sample <- flows_london[(workplace %in% city_data$geo_code) &
#                                       (residence %in% city_data$geo_code),]
# 
# flows_matrix <- foreach(i = 1:length(unique(city_data[,id]))
#                         ,.combine = rbind
#                         ,.final = as.matrix) %do% 
#   {
#     x <- rep_len(0,nrow(city_data))
#     d <- flows_london_sample[from_id == (i + 160),.(bike,to_id)]
#     x[d[,to_id]%% 160 ] <- d[,bike]
#     x
#   }
# 
# # view(flows_matrix)
# 
# run_name <- "osm_unfilt"
# centroids <-  "centr_geom"
# 
# # test of the function is successful.
# registerDoParallel(cores = 3)
# test_simu <- simulation(london_graph_simple
#                         ,flows_matrix = flows_matrix
#                         ,city_data = city_data
#                         ,run_name = run_name
#                         ,centroids = centroids)
# stopImplicitCluster()



simulation <- function(graph, flows_matrix, city_data, run_name, centroids = "centr_geom", cost_fun = "exp") {
  # run name is a prefix to inclide in the nae of saved files
  if ( !(centroids %in% colnames(city_data))) return(print("error, provide a existing column with point geometries to 'centroids'"))
  tryCatch({
    dir.create(here::here(paste0("simulations/",run_name,"_",centroids)))
    directory <- here::here(paste0("simulations/",run_name,"_",centroids))
    },error = function(e) print(e) )

  centr_node_id <- paste0(centroids,"_node")
  city_data[,centr_node_id] <- find_nearest_node_on_graph(graph,city_data[,..centroids] %>% st_as_sf(crs = 4326))
  
  centroid_distance <- city_data[,..centroids] %>% 
    st_as_sf(crs = 4326) %>%
    st_distance() %>% 
    set_units(NULL)
  
  graph_dist_matrix <- get_distance_matrix(graph
                                     ,from = city_data[,get(centr_node_id)]
                                     ,to = city_data[,get(centr_node_id)]
                                     ,allcores = TRUE)
  
  links_of_interest <- which(centroid_distance < 15000, arr.ind = TRUE)
  
  # links_of_interest %>% list.save("links_of_interest.rds")
  
  distances_graph <-
    get_distance_pair(graph
                      ,from = city_data[links_of_interest[,1],get(centr_node_id)]
                      ,to = city_data[links_of_interest[,2],get(centr_node_id)]
                      ,allcores = TRUE
    )
  
  centr_dist <- st_distance(x = city_data[links_of_interest[,1],..centroids] %>% st_as_sf(crs = 4326)
                            ,y = city_data[links_of_interest[,2],..centroids] %>% st_as_sf(crs = 4326)
                            ,by_element = TRUE
  ) %>%
    set_units(NULL)
  
  jpeg(paste0(directory,"/",run_name,"_dists_all.jpg")
       ,height = 5.83
       ,width = 8.27
       ,quality = 80
       ,units = "in"
       ,res = 150)
  
  plot(centr_dist,distances_graph
       ,col = "navyblue"
       ,pch = 20
       #,log = "y"
       ,cex = 0.3
       ,cex.lab = 1.2
       ,main = "network and crowfly distance comparison, London"
       ,xlab = "crow-fly distance, m"
       ,ylab = "network shortest distance, m")
  # lines(1:max(centr_dist)
  #       ,1:max(centr_dist)*sqrt(2)
  #       ,col = "darkorange"
  #       ,lwd = 2)
  lines(1:max(centr_dist)
        ,1:max(centr_dist)
        ,col = "darkred"
        ,lwd = 2)
  # lines(centr_dist
  #       ,dist_model$fitted.values
  #       ,col = "darkgreen")
  legend(x = "bottomright"
         #,y = 1000
         ,legend = c(
                     # "f(x) = 1.414x"
                     "f(x) = x"
                     # ,"f(x) = 1.236x"
                     )
         ,cex = 1.3
         ,col = c(
                  # "orange"
                  "darkred"
                  # ,"darkgreen"
                  )
         ,lwd = 2)
  dev.off()
  
  #### Mapping the difficultly accessible locations across london based on the network ####
  
  outliers_of_interest <- which((distances_graph/centr_dist)>sqrt(2))

  node_outliers <-
    city_data[links_of_interest[outliers_of_interest,1]
                ,..centr_node_id][,.(acces_diff=.N/983),by = centr_node_id]

  cols <- c("geo_code","geo_name",centroids,centr_node_id)
  node_outliers <- merge.data.table(city_data[,..cols]
                                    ,node_outliers
                                    ,by = centr_node_id
                                    ,all = TRUE)

  access_difficulty_map <- node_outliers %>%
    st_as_sf(crs = 4326) %>%
    tm_shape() + tm_polygons(col = "acces_diff"
                             ,palette = "Purples"
                             ,style = "pretty"
                             ,border.col = "black"
                             ,title = "Access difficulty"
                             ,contrast = c(.2,1)
                             ,colorNA = "dimgray") +
    tm_layout(main.title = "Difficulty to access"
              ,bg.color = "white"
              ,legend.outside = TRUE

    )
  access_difficulty_map %>% tmap_save(paste0(directory,"/",run_name,"_access_difficulty_map.pdf"))
  
  #### GRAVITY MODEL STUFF
  # converting distance to kilometres
  graph_dist_matrix <- graph_dist_matrix/1000
  
  maximised <- FALSE
  beta <- 3.5
  alpha <- 0.1
  r2_ref <- run_model(flows = flows_matrix
            ,distance = graph_dist_matrix
            ,beta = beta
            ,type = "exp"
            ,cores = 12) %>% .$r2
  
  while ( !maximised ) {
    beta <- beta-alpha 
    print(paste0("calibrating for beta = ",beta))
    run <- run_model(flows = flows_matrix
                     ,distance = graph_dist_matrix
                     ,beta = beta
                     ,type = "exp"
                     ,cores = 3
    )
    print(paste0("R2 = ",run$r2))
    if (run$r2 > r2_ref) { 
      beta <- beta - alpha 
      r2_ref <- run$r2
    } else if(run$r2 <= r2_ref)  {
      maximised <- TRUE } 
  }
  
  # beta_calib <- foreach::foreach(i = 20:40
  #                                ,.combine = rbind) %do% {
  #                                  beta <- 0.1*(i-1)
  #                                  print(paste0("RUNNING MODEL FOR beta = ",beta))
  #                                  run <- run_model(flows = flows_matrix
  #                                                   ,distance = graph_dist_matrix
  #                                                   ,beta = beta
  #                                                   ,type = "exp"
  #                                                   ,cores = 3
  #                                  )
  #                                  
  #                                  cbind(beta, run$r2,run$rmse)
  #                                }
  # 
  # # selecting the best beta
  # beta_best_fit <- beta_calib[which(beta_calib[,2] == max(beta_calib[,2])),1] %>% as.double()
  
  beta_best_fit <- beta
  # running the fit for the best beta. 
  run_best_fit <- run_model(flows = flows_matrix
                            ,distance = graph_dist_matrix
                            ,beta = beta_best_fit
                            ,type = "exp"
                            ,cores = 3
  )
  
  run_best_fit %>% list.save(paste0(directory,"/",run_name,"_best_fit.rds"))
  
  # plotting 
  
  jpeg(paste0(directory,"/",run_name,"_best_fit.jpg")
       ,height = 5.83
       ,width = 5.83
       ,quality = 80
       ,units = "in"
       ,res = 150)
  plot(
    flows_matrix
    ,run_best_fit$values
    ,ylab = "flows model"
    ,xlab = "flows"
    # ,log = "xy"
    ,pch = 19
    ,cex = 0.5
  )
  lines(seq_len(max(run_best_fit$values))
        ,seq_len(max(run_best_fit$values))
        ,col = "darkred"
        ,lwd = 2
  )
  dev.off()
  
}
