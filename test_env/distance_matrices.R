#  creating distance matrices

if(!file.exists('test_env/distance_matrices.rds')) {
  
  library(rlist)
  library(data.table)
  library(sf)
  library(cppRouting)
  library(Btoolkit)
  
  ####
  
  networks_ch <- rlist::list.load('Benchmarks/cppr_networks/networks_ch.rds')
  
  delta_matrices <- rlist::list.load('test_env/delta_matrices.rds')
  
  centroids <- rlist::list.load('Benchmarks/centroids.rds')
  
  
  distance_matrix <- function(network, centroid, delta){
    
    dist_mat <- cppRouting::get_distance_matrix(network
                                                ,from = centroid$from
                                                ,to = centroid$to
                                                ,algorithm = 'mch') |> 
      round()
    
    return(dist_mat/1000+delta)
  }
  
  overlap(names(delta_matrices)
          ,names(centroids))
  
  # distance_matrix(network = networks_ch$os_all
  #                 ,centroid = centroids$os_all_commute
  #                 ,delta = delta_matrices$os_all_commute)
  
  
  distance_matrices <- mapply(names(centroids),delta_matrices,centroids,SIMPLIFY = FALSE,FUN = \(name,delta_mat,centroid) {
    n <- strsplit(name,'_') |> unlist()
    # print(n)
    net_name <- paste(n[1],n[2],sep = '_') |> trimws()
    cat('Working on',net_name,'.\n')
    # print(centroid)
    
    return(distance_matrix(network = networks_ch[[net_name]]
                           ,centroid = centroid
                           ,delta = delta_mat))
    
  })
  
  rlist::list.save(distance_matrices,'test_env/distance_matrices.rds')
  
} else cat('distance_matrices file exists, erase to recreate.\n')