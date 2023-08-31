### OSM walk sim the new way
library(foreach)
library(cppSim)
source('test_env/run_network_sim.R')

#### DATA
london_msoa <- rlist::list.load('data/london_msoa.rds')

london_msoa[,intra_dist:=intra_dist/1000]
### flows
flows_mat <- rlist::list.load('test_env/flow_mat/flows_mat.rds')

#### networks
source('test_env/networks.R')

#### centroids
centroids <- rlist::list.load('benchmarks/centroids.rds')

### dist mat

osm_mat <- list('geom'=NULL
               ,'commute'=NULL
               ,'net'=NULL)

networks$osm_walk <- networks$osm_walk |> cppRouting::cpp_contract()

osm_mat$geom <- cppRouting::get_distance_matrix(networks$osm_walk
                                               ,from = centroids$osm_walk_geom$from
                                               ,to=centroids$osm_walk_geom$to
                                               ,algorithm = 'mch')

osm_mat$net <- cppRouting::get_distance_matrix(networks$osm_walk
                                              ,from = centroids$osm_walk_net$from
                                              ,to=centroids$osm_walk_net$to
                                              ,algorithm = 'mch')

osm_mat$commute <- cppRouting::get_distance_matrix(networks$osm_walk
                                                  ,from = centroids$osm_walk_commute$from
                                                  ,to=centroids$osm_walk_commute$to
                                                  ,algorithm = 'mch')

osm_mat <- lapply(osm_mat, \(mat) mat/1000)

diag(osm_mat$geom) <- london_msoa[,intra_dist]
diag(osm_mat$commute)[which(diag(osm_mat$commute)==0)] <- london_msoa[which(diag(osm_mat$commute)==0),intra_dist]
diag(osm_mat$net) <- london_msoa[,intra_dist]

lapply(osm_mat,FUN = \(mat) which(mat==0,arr.ind = TRUE))

osm_walk_sim <- run_network_sim(osm_mat
                          ,flows_mat = flows_mat$walk)


osm_walk_sim |> rlist::list.save('test_env/osm_walk_sim.rds')






