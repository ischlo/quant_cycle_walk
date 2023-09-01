
if(file.exists('test_env/sim.rds')) {
  
  cat('This will replace the images of quality of fit.')
  
  library(rlist)
  library(data.table)
  
#### results ####

  sim <- rlist::list.load('test_env/sim.rds')
  
  ### cycling
  osm_geom_result <- sim$osm_all_geom$cycle
  osm_commute_result <- sim$osm_all_commute$cycle
  osm_network_result <- sim$osm_all_net$cycle
  os_geom_result <-  sim$os_all_geom$cycle
  os_commute_result <- sim$os_all_commute$cycle
  os_network_result <- sim$os_all_net$cycle
  dodgr_geom_result <- sim$dodgr_cycle_geom
  dodgr_commute_result <- sim$dodgr_cycle_commute
  dodgr_network_result <- sim$dodgr_cycle_net
  
  results <- list("osm_geom" = osm_geom_result
                  ,"osm_commute" = osm_commute_result
                  ,"osm_net" = osm_network_result
                  ,"os_geom" = os_geom_result
                  ,"os_commute" = os_commute_result
                  ,"os_net" = os_network_result
                  ,"dodgr_geom" = dodgr_geom_result
                  ,"dodgr_commute" = dodgr_commute_result
                  ,"dodgr_network" = dodgr_network_result)
  
  lapply(results, FUN = function(res) res$beta_calib[which.max(res$beta_calib[,2]),c(1,2)])
  
  # beta_results <- cbind(beta = results[[1]]$beta_calib[,1]
  #                       ,lapply(results, FUN = function(res) res$beta_calib[,2]) |> as.data.frame()) |> 
  #   as.matrix()
  
  {
    coul <- 1:8
    p_types <- c(15,17,18)
    l_type <- c(1:4)
    
    jpeg("test_env/quality_fit_cycle.jpg"
         ,height = 5.83
         ,width = 5.83
         ,quality = 80
         ,units = "in"
         ,res = 150)
    
    par(mar = c(4,5,4,1))
    plot(results[[1]]$beta_calib[,c(1,2)]
         ,xlim = c(.1,.5)
         ,ylim = c(.6,.8)
         ,cex = 1
         ,type = "n"
         ,xlab = expression(beta)
         ,ylab = expression(r^2)
         ,cex.lab =1.2)
    
    title("Quality of fit, cycling"
          ,adj = 0
          ,line = 0.5)
    
    legend(x = "bottomleft"
           ,legend = names(results)
           ,col = coul
           ,lwd = 2
           ,pch = p_types
           ,lty = l_type)
    
    mapply(coul,p_types,l_type,results, FUN = function(col,p_t,l_t,res) lines(res$beta_calib[,c(1,2)]
                                                                              ,col = col
                                                                              ,lwd =2
                                                                              ,type = "b"
                                                                              ,lty = l_t
                                                                              ,pch = p_t))
    dev.off()
  }
  
  ### walking
  ## alternative, if running for specific modes of transport
  # os_walk_sim <- rlist::list.load('test_env/os_walk_sim.rds')
  # osm_walk_sim <- rlist::list.load('test_env/osm_walk_sim.rds')
  # dodgr_walk_sim <- rlist::list.load('test_env/dodgr_walk_sim.rds')
  
  
  results_walk <- list(
    "osm_geom" = sim$osm_walk_geom
    ,"osm_commute" = sim$osm_walk_commute 
    ,"osm_net" = sim$osm_walk_net 
    ,"os_geom" = sim$os_all_geom$walk 
    ,"os_commute" =  sim$os_all_commute$walk
    ,"os_net" =  sim$os_all_net$walk
    ,"dodgr_geom"= sim$dodgr_walk_geom
    ,"dodgr_commute"=  sim$dodgr_walk_commute
    ,"dodgr_net"=  sim$dodgr_walk_net
  )
  
  # beta_results_walk <- cbind(beta = results_walk[[1]]$beta_calib[,1]
  #                       ,lapply(results_walk, FUN = function(res) res$beta_calib[,2]) |> as.data.frame()) |> 
  #   as.matrix()
  
  {
    coul <- 1:8
    p_types <- c(15,17,18)
    l_type <- c(1:4)
    
    jpeg("test_env/quality_fit_walk.jpg"
         ,height = 5.83
         ,width = 5.83
         ,quality = 80
         ,units = "in"
         ,res = 150)
    
    par(mar = c(4,5,4,1))
    plot(results_walk[[1]]$beta_calib[,1]
         ,xlim = c(.7,1.6)
         ,ylim = c(.8,.95)
         ,cex = 1
         ,type = "n"
         ,xlab = expression(beta)
         ,ylab = expression(r^2)
         ,cex.lab =1.2)
    
    title("Quality of fit, walking"
          ,adj = 0
          ,line = 0.5)
    
    legend(x = "bottomleft"
           ,legend = names(results_walk)
           ,col = coul
           ,lwd = 2
           ,pch = p_types
           ,lty = l_type)
    
    mapply(coul,p_types,l_type,results_walk, FUN = function(col,p_t,l_t,res) lines(res$beta_calib[,c(1,2)]
                                                                                   ,col = col
                                                                                   ,lwd =2
                                                                                   ,type = "b"
                                                                                   ,lty = l_t
                                                                                   ,pch = p_t))
    dev.off()
  }
  
  
  ## active travel summed up:
  
  
  # currently reading the old results, but adapt to load something like sim_norm2.rds
  #  which should be the output of simulation_norm.R
  norm2_geom_results <- list.load("test_env/norm2_geom/norm2_geom_best_fit.rds")
  
  norm2_commute_results <- list.load("test_env/norm2_commute/norm2_commute_best_fit.rds")
  
  norm2_network_results <- list.load("test_env/norm2_network/norm2_network_best_fit.rds")
  
  results_at <- list("osm_geom" = sim$osm_all_geom$at
                     ,"osm_commute" = sim$osm_all_commute$at
                     ,"osm_net" = sim$osm_all_net$at
                     ,"os_geom" = sim$os_all_geom$at
                     ,"os_commute" = sim$os_all_commute$at
                     ,"os_net" = sim$os_all_net$at
                     ,"norm2_geom" = norm2_geom_results
                     ,"norm2_commute"=norm2_commute_results
                     ,"norm2_net"=norm2_network_results
  )
  
  
  beta_results_at <- cbind(beta = results_at[[1]]$beta_calib[,1]
                           ,lapply(results_at, FUN = function(res) res$beta_calib[,2]) |> as.data.frame()) |> 
    as.matrix()
  
  
  {
    coul <- 1:8
    p_types <- c(15,17,18)
    l_type <- c(1:4)
    
    jpeg("test_env/quality_fit_at.jpg"
         ,height = 5.83
         ,width = 5.83
         ,quality = 80
         ,units = "in"
         ,res = 150)
    
    par(mar = c(4,5,4,1))
    plot(results_at[[1]]$beta_calib[,c(1,2)]
         ,xlim = c(.4,1.4)
         ,ylim = c(.6,.9)
         ,cex = 1
         ,type = "n"
         ,xlab = expression(beta)
         ,ylab = expression(r^2)
         ,cex.lab =1.2)
    
    title("Quality of fit, active travel"
          ,adj = 0
          ,line = 0.5)
    
    legend(x = "bottomleft"
           ,legend = names(results_at)
           ,col = coul
           ,lwd = 2
           ,pch = p_types
           ,lty = l_type)
    
    mapply(coul,p_types,l_type,results_at, FUN = function(col,p_t,l_t,res) lines(res$beta_calib[,c(1,2)]
                                                                                 ,col = col
                                                                                 ,lwd =2
                                                                                 ,type = "b"
                                                                                 ,lty = l_t
                                                                                 ,pch = p_t))
    dev.off()
    }
  
} else cat('sim.rds not found, run previous steps of the analysis before this one.')
