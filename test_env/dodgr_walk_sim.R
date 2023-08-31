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

## dodgr network walk
# # this code served as the base to create the run_network_sim function
# beta_init <- .7
# step <- .03
# res <- list('dodgr_geom'=NULL
#             ,'dodgr_commute'=NULL
#             ,'dodgr_net'=NULL
#             )
# ## geom 
# intermediate_res_geom <- matrix(nrow = 0,ncol = 2)
# intermediate_res_commute <- matrix(nrow = 0,ncol = 2)
# intermediate_res_net <- matrix(nrow = 0,ncol = 2)
# 
# for(i in 1:30) {
#   if(i==1) beta <- beta_init
#   beta <- beta+step
#   run <- list()
#   run_geom <- run_model(flows=flows_mat_walk
#                         ,distance = dodgr_dist_mat_walk$geom
#                         ,beta = beta)
#   run_commute <- run_model(flows=flows_mat_walk
#                    ,distance = dodgr_dist_mat_walk$commute
#                    ,beta = beta)
#   run_net <- run_model(flows=flows_mat_walk
#                    ,distance = dodgr_dist_mat_walk$net
#                    ,beta = beta)
# 
#   
#   intermediate_res_geom <- rbind(intermediate_res_geom
#                                  ,c(beta
#                                     ,cor(run_geom[[1]] |> as.numeric()
#                                          ,flows_mat_walk |> as.numeric())^2
#                                  ))
#   intermediate_res_commute <- rbind(intermediate_res_commute
#                                  ,c(beta
#                                     ,cor(run_commute[[1]] |> as.numeric()
#                                          ,flows_mat_walk |> as.numeric())^2
#                                  ))
#   intermediate_res_net <- rbind(intermediate_res_net
#                                  ,c(beta
#                                     ,cor(run_net[[1]] |> as.numeric()
#                                          ,flows_mat_walk |> as.numeric())^2
#                                  ))
#                                
# }
# 
# beta_max <- intermediate_res_geom[which.max(intermediate_res_geom[,2]),1]
# best_res <- run_model(flows=flows_mat_walk
#                       ,distance = dodgr_dist_mat_walk$geom
#                       ,beta = beta_max)
# dodgr_walk_geom <- list('best_fit'=best_res[[1]]
#                         ,'beta_calib'=intermediate_res_geom)
# 
# beta_max <- intermediate_res_commute[which.max(intermediate_res_commute[,2]),1]
# best_res <- run_model(flows=flows_mat_walk
#                       ,distance = dodgr_dist_mat_walk$commute
#                       ,beta = beta_max)
# dodgr_walk_commute <- list('best_fit'=best_res[[1]]
#                         ,'beta_calib'=intermediate_res_commute)
# 
# beta_max <- intermediate_res_net[which.max(intermediate_res_net[,2]),1]
# best_res <- run_model(flows=flows_mat_walk
#                       ,distance = dodgr_dist_mat_walk$net
#                       ,beta = beta_max)
# dodgr_walk_net <- list('best_fit'=best_res[[1]]
#                         ,'beta_calib'=intermediate_res_net)
# 
# # 
# # identical(dodgr_walk_geom,dodgr_walk_commute)
# # identical(dodgr_walk_geom,dodgr_walk_net)
# # identical(dodgr_walk_commute,dodgr_walk_net)
# 
# list('geom'=dodgr_walk_geom
#      ,'commute'=dodgr_walk_commute
#      ,'net'=dodgr_walk_net) |> rlist::list.save('test_env/dodgr_walk_sim.rds')
