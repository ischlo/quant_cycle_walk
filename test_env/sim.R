# running the sims to produce estimates.

if(!file.exists('test_env/sim.rds')){
  
  library(rlist)
  library(cppSim)
  library(data.table)
  
  source('test_env/run_network_sim.R')
  ####
  
  distance_matrices <- rlist::list.load('test_env/distance_matrices.rds')
  
  flows_mat <- rlist::list.load('test_env/flow_mat/flows_mat.rds')
  
  # lapply(distance_matrices, FUN = \(mat) which(mat))
  
  #### running sim 
  
  sim <- mapply(names(distance_matrices),distance_matrices,SIMPLIFY = FALSE, FUN = \(name,dist) {
    n <- strsplit(name,'_') |> unlist()
    print(name)
    if(n[2]=='all') {
      
      res_1 <- run_network_sim(dist_mat = dist
                               ,flows_mat = flows_mat[['at']]
                               ,beta_init = .4
                               ,step = .03)
      res_2 <- run_network_sim(dist_mat = dist
                              ,flows_mat = flows_mat[['cycle']]
                              ,beta_init = 0
                              ,step = .03)
      res_3 <- run_network_sim(dist_mat = dist
                               ,flows_mat = flows_mat[['walk']]
                               ,beta_init = .5
                               ,step = .03
                               ,n_iter = 40)
  
      res <- list('at'=res_1
                  ,'cycle'=res_2
                  ,'walk'=res_3)
      
    } else if (n[2]=='cycle'){
      
      res <- run_network_sim(dist_mat = dist
                             ,flows_mat = flows_mat[['cycle']]
                             ,beta_init = 0
                             ,step = .03)
      
    } else if (n[2]=='walk'){
      
      res <- run_network_sim(dist_mat = dist
                             ,flows_mat = flows_mat[['walk']]
                             ,beta_init = .5
                             ,step = .03
                             ,n_iter = 40)
      
    }
    
    return(res)
    
  })
  
  str(sim)
  
  
  
  rlist::list.save(sim,'test_env/sim.rds')
  
} else cat('sim file already exists, erase to recreate.')
