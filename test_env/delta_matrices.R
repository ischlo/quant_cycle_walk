# this script creates delta matrices that are used to avoid 0 values when centroids of areas match to the same node in the network

if(!file.exists('test_env/delta_matrices.rds')){
  
  if(file.exists('Benchmarks/cppr_networks/networks.rds')){
  
  library(rlist)
  library(Btoolkit)
  library(data.table)
  
  london_msoa <- rlist::list.load('data/london_msoa.rds')
  
  networks <- rlist::list.load('Benchmarks/cppr_networks/networks.rds')
  
  centroids <- rlist::list.load('Benchmarks/centroids.rds')
  
  #### 
  delta_mat <- function(centroids_id,nodes,from,to=NULL){
    
    if(is.null(to)){
      
      n <- nodes[match(centroids_id$from,osmid),as.matrix(.SD[,.(x,y)])]
      # n_to <- nodes[match(centroids_id$to,osmid),.(x,y)]
      
      c <- as_geo(from) |> sf::st_coordinates()
      
      d_1 <- Btoolkit::fdistance(coord1=c,coord2=n,coords = 'unprojected',one_to_one = TRUE) |> round()
      
      # return dist in meters
      return((matrix(data = d_1,ncol = length(d_1),nrow = length(d_1),byrow = TRUE) + 
               matrix(data = d_1,ncol = length(d_1),nrow = length(d_1),byrow = FALSE))/1000)
      
    } else {
      n <- nodes[match(centroids_id$from,osmid),as.matrix(.SD[,.(x,y)])]
      n_to <- nodes[match(centroids_id$to,osmid),as.matrix(.SD[,.(x,y)])]
      
      c <- as_geo(from) |> sf::st_coordinates()
      c_to <-  as_geo(to) |> sf::st_coordinates()
      
      d_1 <- Btoolkit::fdistance(coord1=c,coord2=n,coords = 'unprojected',one_to_one = TRUE) |> round()
      d_2 <- Btoolkit::fdistance(coord1=c_to,coord2=n_to,coords = 'unprojected',one_to_one = TRUE) |> round()
      
      return((matrix(data = d_1,ncol = length(d_1),nrow = length(d_1),byrow = TRUE) + 
               matrix(data = d_2,ncol = length(d_2),nrow = length(d_2),byrow = FALSE))/1000)
      
    }
    
  }
  
  
  names(networks)
  names(centroids)
  
  delta_mat(centroids_id = centroids$os_all_geom
            ,nodes = networks$os_all$coords
            ,from = london_msoa[,'centr_geom']
            # ,to= london_msoa[,'workplace_centr']
            )
  
  lapply(names(centroids),FUN = \(n) strsplit(n,'_') |> unlist())
  
  delta_matrices <- mapply(names(centroids),centroids,SIMPLIFY = FALSE,FUN = \(name,centroid) {
    n <- strsplit(name,'_') |> unlist()
    # print(n)
    net_name <- paste(n[1],n[2],sep = '_') |> trimws()
    cat('Working on',net_name,'.\n')
    # print(centroid)
    
    if(n[3]=='geom'){
      # print(n[3])
      res <- delta_mat(centroids_id = centroid
                       ,nodes =  networks[[net_name]]$coords
                       ,from=london_msoa[,'centr_geom'])
    } else if(n[3]=='commute'){
      # print(n[3])
      res <- delta_mat(centroids_id =  centroid
                       ,nodes = networks[[net_name]]$coords
                       ,from=london_msoa[,'pop_weight_geom']
                       ,to=london_msoa[,'workplace_centr'])
    } else if(n[3]=='net'){
      # print(n[3])
      res <- delta_mat(centroids_id = centroid
                       ,nodes = networks[[net_name]]$coords
                       ,from=london_msoa[,'net_centr'])
    }
    return(res)
  })
  
  
 rlist::list.save(delta_matrices,'test_env/delta_matrices.rds')
  
  } else cat('run network and routing setups first to produce the networks and london_msoa object.') 
  } else cat('delta_matrices already exist, delete first to re create')


