library(data.table)
library(rlist)
library(sf)

#### find centroids here.

# DATA
if(file.exists('benchmarks/centroids.rds')){
  cat('centroids already exist\n'
      ,'Erase first to re create.\n')
} else{
  
  london_msoa <- rlist::list.load('data/london_msoa.rds')
  
  # class(london_msoa)
  
  networks <- c('osm_all'=list(rlist::list.load('benchmarks/cppr_networks/osm_all.rds'))
                ,'osm_cycle'=list(rlist::list.load('benchmarks/cppr_networks/osm_cycle.rds'))
                ,'osm_walk'=list(rlist::list.load('benchmarks/cppr_networks/osm_walk.rds'))
                ,'dodgr_walk'=list(rlist::list.load('benchmarks/cppr_networks/dodgr_walk.rds'))
                ,'dodgr_cycle'=list(rlist::list.load('benchmarks/cppr_networks/dodgr_cycle.rds'))
                ,'os'=list(rlist::list.load('benchmarks/cppr_networks/os_graph.rds'))
  )
  
  centroids <- list('geom'='centr_geom'
                    ,'net'='net_centr'
                    ,'commute'=c('pop_weight_geom','workplace_centr'))
  
  ###
  
  centroids_list <- list()
  
  # centr <- 'geom'
  
  for(net in names(networks)) {
    for (centr in names(centroids)){
      print(paste(net,centr,sep='_'))
      # 
      if(centr=='commute'){
        centr_temp <- find_nearest_node_on_graph(networks[[net]],london_msoa[,.SD,.SDcols = centroids[[centr]][1]],crs=4326,wkt=1)
        centr_temp_2 <- find_nearest_node_on_graph(networks[[net]],london_msoa[,.SD,.SDcols = centroids[[centr]][2]],crs=4326,wkt=1)
        n <- paste(net,centr,sep = '_')
        centroids_list[[n]] <- data.frame(from=centr_temp,to=centr_temp_2)
      } else{
        centr_temp <- find_nearest_node_on_graph(networks[[net]],london_msoa[,.SD,.SDcols = centroids[[centr]]],crs=4326,wkt=1)
        n <- paste(net,centr,sep = '_')
        centroids_list[[n]] <- data.frame(from=centr_temp,to=centr_temp)
      }
    }
  }
  
  centroids_list |> rlist::list.save('benchmarks/centroids.rds')
}


