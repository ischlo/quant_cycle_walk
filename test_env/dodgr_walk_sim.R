library(cppSim)
library(data.table)
library(foreach)
library(Btoolkit)
#### Flows

london_msoa <- rlist::list.load('data/london_msoa.rds')

london_msoa[,intra_dist:=intra_dist/1000]

### flows

flows_mat <- rlist::list.load('test_env/flow_mat/flows_mat.rds')

#### networks
source('test_env/networks.R')

#### centroids
centroids <- rlist::list.load('benchmarks/centroids.rds')

#### dist matrices

dodgr_dist_mat_walk <- list('geom'=NULL
                            ,'commute'=NULL
                            ,'net'=NULL)
  
networks$dodgr_walk <- networks$dodgr_walk |> cppRouting::cpp_contract()

RcppParallel::setThreadOptions(numThreads = 6)
dodgr_dist_mat_walk$geom <- cppRouting::get_distance_matrix(networks$dodgr_walk
                                                            ,from = centroids$dodgr_walk_geom$from
                                                            ,to=centroids$dodgr_walk_geom$to
                                                            ,algorithm = 'mch')

dodgr_dist_mat_walk$commute <- cppRouting::get_distance_matrix(networks$dodgr_walk
                                                            ,from = centroids$dodgr_walk_commute$from
                                                            ,to=centroids$dodgr_walk_commute$to
                                                            ,algorithm = 'mch')

dodgr_dist_mat_walk$net <- cppRouting::get_distance_matrix(networks$dodgr_walk
                                                            ,from = centroids$dodgr_walk_net$from
                                                            ,to=centroids$dodgr_walk_net$to
                                                            ,algorithm = 'mch')
RcppParallel::defaultNumThreads()

dodgr_dist_mat_walk <- lapply(dodgr_dist_mat_walk, \(mat) mat/1000)

diag(dodgr_dist_mat_walk$geom) <- london_msoa[,intra_dist]
diag(dodgr_dist_mat_walk$commute)[which(diag(dodgr_dist_mat_walk$commute)==0)] <- london_msoa[which(diag(dodgr_dist_mat_walk$commute)==0),intra_dist]
diag(dodgr_dist_mat_walk$net) <- london_msoa[,intra_dist]

lapply(dodgr_dist_mat_walk,FUN = \(mat) which(mat==0,arr.ind = TRUE))

#### setup for sim

## dodgr network walk
dodgr_walk_sim <- run_network_sim(dodgr_dist_mat_walk
                                  ,flows_mat = flows_mat$walk)

dodgr_walk_sim |> rlist::list.save('test_env/dodgr_walk_sim.rds')
