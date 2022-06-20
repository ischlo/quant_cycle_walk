# this script takes a network, a flows data, and constructs everything from scratch to run a model based on some intput parameters. 

## loading data 

# edges
# edges

#nodes
# nodes

# spatial file of partition (administrative units)
# this file contains polygons of administrative units, but also various from that will be used for routing.
# london_msoa

## Constructing network

# parameters: 
# edges,nodes, simple
# cppr stuff 
# graph


## Routing (origins and destinations based on the from)
# which from to route as origin and destination
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
# region_data <- london_msoa[grep("Camden",geo_name),]
# 
# flows_london_sample <- flows_london[(workplace %in% region_data$geo_code) &
#                                       (residence %in% region_data$geo_code),]
# 
# flows_matrix <- foreach(i = 1:length(unique(region_data[,id]))
#                         ,.combine = rbind
#                         ,.final = as.matrix) %do%
#   {
#     x <- rep_len(0,nrow(region_data))
#     d <- flows_london_sample[from_id == (i + 160),.(bike,to_id)]
#     x[d[,to_id]%% 160 ] <- d[,bike]
#     x
#   }
# 
# # view(flows_matrix)
# 
# run_name <- "osm_unfilt"
# from <-  "centr_geom"
# 
# profvis({
#   # test of the function is successful.
#   registerDoParallel(cores = 3)
#   test_simu <- simulation(london_graph_simple
#                           ,flows_matrix = flows_matrix
#                           ,region_data = region_data
#                           ,run_name = run_name
#                           ,from = from)
#   stopImplicitCluster()
#   
# })


# 
# flows_matrix <- flows_mat
# 
# region_data <- city
# 
# run_name <- "test_from_to"

simulation <- function(flows_matrix
                       ,region_data
                       ,run_name
                       ,graph = NULL
                       ,from = NULL
                       ,to = NULL
                       ,graph_dist_matrix = NULL
                       ,beta_offset = 0
                       ,norm = 2 
                       ,time = FALSE
                       ,n_cores = 3
                       ,cost_fun = "exp") {
  # run name is a prefix to inclide in the name of saved files

  if ( !(from %in% colnames(region_data))) return(print("error, provide a existing column with point geometries to 'from'"))
  
  if(is_null(to)) {to <- from} else if ( !(to %in% colnames(region_data))) return(print("error, provide a existing column with point geometries to 'from'"))
  
  registerDoParallel(cores = n_cores)
  
  tryCatch({
    if (time == TRUE) {
      dir.create(paste0(run_name,"_time"))
      directory <- paste0(run_name,"_time")
    } else {
      dir.create(run_name)
      directory <- run_name
    }

  },error = function(e) print(e))
  
  print("created directory")
  
  if (norm == 1) {
    centroid_distance <- norm_p(region_data[,..from] %>% st_as_sf(wkt = from, crs = 4326)
                                ,region_data[,..to] %>% st_as_sf(wkt = to, crs = 4326)
                                ,elementwise = FALSE
    ) %>% 
      set_units(NULL) %>%
      round()
  } else if (norm == 2) {
    centroid_distance <- region_data[,..from] %>%
      st_as_sf(wkt = from, crs = 4326) %>% 
      st_distance(region_data[,..to] %>% st_as_sf(wkt = to, crs = 4326)
                  ,by_element = FALSE) %>% 
      set_units(NULL)  %>%
      round()
  }
  
  d <- region_data %>% st_as_sf(wkt = "geometry",crs = 4326) %>% st_area() %>% sqrt() %>% set_units(NULL) 
  
  centroid_distance <- `diag<-`(centroid_distance
                                ,d)
  
  print("distance matrix computed")
  
  # if(is.null(graph) & is.null(graph_dist_matrix)) {
  #   stop("provide either a graph on which to compute shortest paths, or a matrix of distances")
  #   }
  
  if(!is.null(graph) | !is.null(graph_dist_matrix)) {
    
    if(is.null(graph_dist_matrix)) {
      
      centr_node_id_from <- paste0(from,"_id")
      region_data[,centr_node_id_from] <- find_nearest_node_on_graph(graph = graph
                                                                     ,region_data[,..from] %>% st_as_sf(wkt = from, crs = 4326)
                                                                    
      )
      
      if(!is_null(to)) {
        if((from != to)) {
          centr_node_id_to <- paste0(to,"_id")
          region_data[,centr_node_id_to] <- find_nearest_node_on_graph(graph = graph
                                                                       ,region_data[,..to] %>% st_as_sf(wkt = to, crs = 4326)
                                                                       
          )
        } 
        else if (from == to) {
          centr_node_id_to <- centr_node_id_from
        }
        
      }
      print("nearest nodes found")

     
      graph_dist_matrix <- get_distance_matrix(Graph = graph
                                               ,from = region_data[,..centr_node_id_from][[1]]
                                               ,to = region_data[,..centr_node_id_to][[1]]
                                               ,allcores = TRUE) %>% 
        round()
    } 
    
    # graph_dist_matrix <- `diag<-`(graph_dist_matrix, d)
    
    # graph_dist_matrix %>% list.save(paste0(directory,"/","graph_dist_matrix",".rds"))
    # 
    # print("computed and saved distance matrix")
    
    links_of_interest <- which(centroid_distance < 15000, arr.ind = TRUE)
    
    # links_of_interest %>% list.save("links_of_interest.rds")
    
    distances_graph <- graph_dist_matrix[links_of_interest] %>% unlist
    
    print("computed pair distances of paths of interest")
    
    centr_dist <- centroid_distance[links_of_interest] %>% unlist
    
    print("computed pair distances between from")
    
    # x <- sample(1:nrow(links_of_interest),3000)
    
    jpeg(paste0(directory,"/",run_name,"_dists_all.jpg")
         ,height = 5.83
         ,width = 8.27
         ,quality = 80
         ,units = "in"
         ,res = 150)
    
    smoothScatter(centr_dist#[x]
         ,distances_graph#[x]
         # ,col = "navyblue"
         # ,pch = 20
         # #,log = "y"
         # ,cex = 0.3
         # ,cex.lab = 1.2
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
    # legend(x = "bottomright"
    #        #,y = 1000
    #        ,legend = c(
    #          # "f(x) = 1.414x"
    #          "f(x) = x"
    #          # ,"f(x) = 1.236x"
    #        )
    #        ,cex = 1.3
    #        ,col = c(
    #          # "orange"
    #          "darkred"
    #          # ,"darkgreen"
    #        )
    #        ,lwd = 2)
    dev.off()
    #### Mapping the difficultly accessible locations across london based on the network ####
    try({
      outliers_of_interest <- which((distances_graph/centr_dist)>sqrt(2))
      
      #### interlude to check consistency :
      #### 
      # outliers_of_interest <- which((distances_graph/centr_dist)<1)
      # 
      # (distances_graph/centr_dist) %>% hist()
      # 
      # tmap_mode("view")
      # 
      # region_data[links_of_interest[outliers_of_interest,1],"geometry"] %>% qtm() +
      #   (graph$coords[graph$coords$node_id %in% region_data[links_of_interest[outliers_of_interest,2],"geometry_id"][[1]]] %>%
      #      st_as_sf(coords = c("X","Y")
      #               ,crs=4326) %>% qtm(dots.col = "red"))

      node_outliers <-
        region_data[links_of_interest[outliers_of_interest,1],..centr_node_id_to][
          ,.(acces_diff=.N/nrow(region_data)),by = centr_node_id_to] 

      # node_outliers <-
      #   region_data[links_of_interest[outliers_of_interest,1],..centr_node_id_to] %>%
      #   group_by({{centr_node_id_to}}) %>%
      #   mutate(acces_diff=dplyr::n()/nrow(region_data))
      
      cols <- c("geo_code","geo_name","geometry",centr_node_id_to)
      node_outliers <- merge(region_data[,..cols]
                             ,node_outliers
                             ,by = centr_node_id_to
                             ,all = TRUE)
      
      access_difficulty_map <- node_outliers %>%
        st_as_sf(crs = 4326, wkt = "geometry") %>%
        tm_shape() + tm_polygons(col = "acces_diff"
                             ,size = .3
                             ,palette = "viridis"
                             ,style = "pretty"
                             ,border.col = "black"
                             ,title = "Access difficulty"
                             #,contrast = c(.2,1)
                             ,colorNA = "dimgray") +
        tm_layout(main.title = "Difficulty to access"
                  ,bg.color = "white"
                  ,legend.outside = TRUE
                  
        )
      access_difficulty_map %>% tmap_save(paste0(directory,"/",run_name,"_access_difficulty_map.pdf"))
    }
    ,silent = FALSE)
    
  } else if(is.null(graph) & is.null(graph_dist_matrix)) { graph_dist_matrix <- centroid_distance }
  
  #### GRAVITY MODEL STUFF
  
  # key values : for an exponential cost function, the range of beta values to look at is (0,2) with a best fit around  0.025 is the smaller study area
  # for time in the cost function, 
  
  # ameliorate the following part
  # options : convrging which, for with specified values
  
  if (time) {
    # converting distance to kilometres
    graph_dist_matrix <- graph_dist_matrix/(1000*14)
    range_i <- 0:120
  } else if (!time) { 
    # converting distances to time
    range_i <- 1:30
    graph_dist_matrix <- graph_dist_matrix/1000
  }
  
  # maximised <- FALSE
  # beta <- 2
  # alpha <- 0.01
  # i <- 0
  # r2_ref <- 0
  # e_sor <- c()
  # 
  # while (!maximised) {
  #   # beta <- beta+alpha
  #   print(paste0("calibrating for beta = ",beta))
  #   run <- run_model(flows = flows_matrix
  #                    ,distance = graph_dist_matrix
  #                    ,beta = beta
  #                    ,type = "exp"
  #                    ,cores = n_cores
  #   )
  #   print(paste0("R2 = ",run$r2))
  #   e_sor <- append(e_sor,run$e_sor)
  #   if (run$r2 > r2_ref) {
  #     beta <- beta - alpha
  #     r2_ref <- run$r2
  #   } else if(run$r2 <= r2_ref)  {
  #     maximised <- TRUE 
  #     beta <- beta + alpha
  #     }
  #   i <- i+1
  # }
  # 
  # beta_best_fit <- beta
  
  beta_calib <- foreach::foreach(i = range_i
                                 ,.combine = rbind) %dopar% {
                                   beta <- (0.03*i)+beta_offset
                                   print(paste0("RUNNING MODEL FOR beta = ",beta))
                                   model_run <- run_model(flows = flows_matrix
                                                    ,distance = graph_dist_matrix
                                                    ,beta = beta
                                                    ,type = "exp"       
                                   )
                                   
                                   cbind(beta, model_run$r2,model_run$e_sor)
                                 }
  
  # selecting the best beta
  beta_best_fit <- beta_calib[which.max(beta_calib[,2]),1] %>% as.double()
  
  jpeg(paste0(directory,"/",run_name,"beta_calib.jpg")
       ,height = 5.83
       ,width = 5.83
       ,quality = 80
       ,units = "in"
       ,res = 150)
  
  plot(beta_calib[,1]
       ,beta_calib[,3]
       ,xlab = "beta value"
       ,ylim = c(.2,1)
       ,ylab = "quality of fit"
       ,main = "influence of beta on the goodness of fit"
       ,pch = 19
       ,cex = 0.5
       ,type = "b")
  
  points(beta_calib[which.max(beta_calib[,2]),1]
         ,beta_calib[which.max(beta_calib[,2]),2]
         ,pch = 15)
  
  points(beta_calib[which.max(beta_calib[,3]),1]
         ,beta_calib[which.max(beta_calib[,3]),3]
         ,pch = 15)
  
  lines(beta_calib[,1]
        ,beta_calib[,2]
        ,lwd = 2
        ,col = "darkred")
  
  legend(x = "bottomleft"
         ,legend = c("Sorensen I","r_2")
         ,pch = c(20,NA)
         ,lwd = c(2,2)
         ,lty = c(8,1)
         ,col = c("black","darkred")
         )
  dev.off()
  
  # running the fit for the best beta.
  run_best_fit <- run_model(flows = flows_matrix
                            ,distance = graph_dist_matrix
                            ,beta = beta_best_fit
                            ,type = "exp"
  )

  list("best_fit" = run_best_fit$values
       ,"beta_calib" = beta_calib
       ) %>% list.save(paste0(directory,"/",run_name,"_best_fit.rds"))
  
  
  # x <- sample(1:length(run_best_fit$values),3000)
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
    ,log = "xy"
    ,pch = 19
    ,cex = 0.5
  )
  lines(seq_len(max(run_best_fit$values))
        ,seq_len(max(run_best_fit$values))
        ,col = "darkred"
        ,lwd = 2
  )
  dev.off()
  
  stopImplicitCluster()
  #
}
