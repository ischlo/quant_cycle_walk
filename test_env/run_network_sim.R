

run_network_sim <- function(dist_mat
                            ,flows_mat
                            ,beta_init = .7
                            ,step = .03
                            ,n_iter = 30){
  if(inherits(dist_mat,'list')){
  
  intermediate_res <- lapply(dist_mat,FUN = \(mat) matrix(nrow = 0,ncol = 2))
  
  for(i in 1:n_iter) {
    if(i==1) beta <- beta_init
    beta <- beta+step
    
    runs <- lapply(dist_mat,FUN =  \(mat) cppSim::run_model(flows=flows_mat
                                                            ,distance = mat
                                                            ,beta = beta))
    
    intermediate_res <- mapply(runs,intermediate_res,FUN = \(run,res) {
      # run[[1]]
      rbind(res
            ,c(beta
               ,cor(run[[1]] |> as.numeric()
                    ,flows_mat |> as.numeric())^2
            ))
    },SIMPLIFY = FALSE)
  }
  
  beta_max <- lapply(intermediate_res,FUN = \(res) res[which.max(res[,2]),1])
  best_res <- mapply(beta_max,dist_mat,SIMPLIFY = FALSE,FUN=\(bet,dist) cppSim::run_model(flows=flows_mat
                                                                 ,distance = dist
                                                                 ,beta = bet))
  
  return(mapply(intermediate_res,best_res,SIMPLIFY = FALSE,FUN=\(beta_calib,res) list('best_fit'=res[[1]]
                                                                      ,'beta_calib'=beta_calib)))
  
  } else if (inherits(dist_mat,'matrix')){
    
    return(run_network_sim_1(dist_mat = dist_mat
                      ,flows_mat = flows_mat
                      ,beta_init = beta_init
                      ,step = step
                      ,n_iter = n_iter))
  }
}


# test_net |> str()


run_network_sim_1 <- function(dist_mat
                            ,flows_mat
                            ,beta_init = .7
                            ,step = .03
                            ,n_iter = 30){
  
  intermediate_res <- matrix(nrow = 0,ncol = 2)
  
  for(i in 1:n_iter) {
    if(i==1) beta <- beta_init
    beta <- beta+step
    
    run <- cppSim::run_model(flows=flows_mat
                              ,distance = dist_mat
                              ,beta = beta)
    
    intermediate_res <- rbind(intermediate_res
                              ,c(beta
                                 ,cor(run[[1]] |> as.numeric()
                                      ,flows_mat |> as.numeric())^2
                              ))
  }
  
  beta_max <- intermediate_res[which.max(intermediate_res[,2]),1]
  # print(beta_max)
  best_res <-cppSim::run_model(flows=flows_mat
                               ,distance = dist_mat
                               ,beta = beta_max)

  return(list('best_fit'=best_res[[1]]
              ,'beta_calib'=intermediate_res))
}






