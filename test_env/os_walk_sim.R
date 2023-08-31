# OS network sim

london_msoa <- rlist::list.load('data/london_msoa.rds')

london_msoa[,intra_dist:=intra_dist/1000]

#### centroids
centroids <- rlist::list.load('benchmarks/centroids.rds')

### flows

flows_mat <- rlist::list.load('test_env/flow_mat/flows_mat.rds')

#### networks
source('test_env/networks.R')

### dist mat

os_mat <- list('geom'=NULL
               ,'commute'=NULL
               ,'net'=NULL)

networks$os <- networks$os |> cppRouting::cpp_contract()

os_mat$geom <- cppRouting::get_distance_matrix(networks$os
                                               ,from = centroids$os_geom$from
                                               ,to=centroids$os_geom$to
                                               ,algorithm = 'mch')

os_mat$net <- cppRouting::get_distance_matrix(networks$os
                                              ,from = centroids$os_net$from
                                              ,to=centroids$os_net$to
                                              ,algorithm = 'mch')

os_mat$commute <- cppRouting::get_distance_matrix(networks$os
                                                  ,from = centroids$os_commute$from
                                                  ,to=centroids$os_commute$to
                                                  ,algorithm = 'mch')

###
os_mat <- lapply(os_mat, \(mat) mat/1000)

diag(os_mat$geom) <- london_msoa[,intra_dist]
diag(os_mat$commute)[which(diag(os_mat$commute)==0)] <- london_msoa[which(diag(os_mat$commute)==0),intra_dist]
diag(os_mat$net) <- london_msoa[,intra_dist]

lapply(os_mat,FUN = \(mat) which(mat==0,arr.ind = TRUE))

os_sim <- run_network_sim(os_mat
                          ,flows_mat = flows_mat$walk)


os_sim |> rlist::list.save('test_env/os_walk_sim.rds')







