# running the gravity model for the selected networks:

# loading the distance matrices:

#osm
osm_dist_geom <- list.load("/Users/ivann/Desktop/CASA/benchmarks/osm_dist_matrix_standard.rds")

osm_dist_commute <- list.load("/Users/ivann/Desktop/CASA/benchmarks/osm_dist_matrix_commute.rds")

osm_dist_network <- list.load("/Users/ivann/Desktop/CASA/benchmarks/osm_dist_matrix_network.rds")

# os
os_dist_geom <- list.load("/Users/ivann/Desktop/CASA/benchmarks/os_dist_matrix_geom.rds")

os_dist_commute <- list.load("/Users/ivann/Desktop/CASA/benchmarks/os_dist_matrix_commute.rds")

os_dist_network <- list.load("/Users/ivann/Desktop/CASA/benchmarks/os_dist_matrix_network.rds")


# dodgr

dodgr_dist_geom <- list.load("/Users/ivann/Desktop/CASA/benchmarks/dodgr_dists_geom.rds")

dodgr_dist_commute <- list.load("/Users/ivann/Desktop/CASA/benchmarks/dodgr_dists_commute.rds")

dodgr_dist_network <- list.load("/Users/ivann/Desktop/CASA/benchmarks/dodgr_dists_network.rds")

# grouping into a list

dist_matrices <- list(osm_dist_geom
                      ,osm_dist_commute
                      ,osm_dist_network
                      ,os_dist_geom
                      ,os_dist_commute
                      ,os_dist_network
                      ,dodgr_dist_geom
                      ,dodgr_dist_commute
                      ,dodgr_dist_network
                      )

# flows_matrix

flows <- list.load("../data/flows_london.rds") %>% as.data.table() #%>% 
  
## for cycling
flows_mat <- foreach(i = london_msoa[,id]
                     ,.combine = rbind
                     ,.final = as.matrix) %do%
  {
    x <- rep_len(0,nrow(london_msoa))
    d <- flows[from_id == i,.(bike,to_id)] # flows
    x[d[,to_id]] <- d[,bike]
    x
  }

## for walking
flows_mat_walk <- foreach(i = london_msoa[,id]
                     ,.combine = rbind
                     ,.final = as.matrix) %do%
  {
    x <- rep_len(0,nrow(london_msoa))
    d <- flows[from_id == i,.(foot,to_id)] # flows
    x[d[,to_id]] <- d[,foot]
    x
  }

# active travel 
flows_mat_at <- (flows_mat + flows_mat_walk)

# identical(flows_mat_at
#           ,flows_mat+flows_mat_walk)

# doing the gravity model runs:
run_names <- c("osm_dist_geom"
               ,"osm_dist_commute"
               ,"osm_dist_network"
               ,"os_dist_geom"
               ,"os_dist_commute"
               ,"os_dist_network"
               # ,"dodgr_dist_geom"
               # ,"dodgr_dist_commute"
               # ,"dodgr_dist_network"
               )

run_names_walk <- paste0(run_names,"_walk")

run_names_at <- paste0(run_names,"_at")

# do not chanhge for different mode of transport
origs <- c("centr_geom"
           ,"pop_weight_geom"
           ,"net_centr"
           ,"centr_geom"
           ,"pop_weight_geom"
           ,"net_centr"
           # ,"centr_geom"
           # ,"pop_weight_geom"
           # ,"net_centr"
           )

dests <- c("centr_geom"
           ,"workplace_centr"
           ,"net_centr"
           ,"centr_geom"
           ,"workplace_centr"
           ,"net_centr"
           # ,"centr_geom"
           # ,"workplace_centr"
           # ,"net_centr"
           )

# run_names,origs,dest,dist_matrices

lapply(dist_matrices, function(x) x %>% as.numeric() %>% summary())


# k <- 1

for(k in 1:length(run_names_walk)) { # check length of run names
  # system.time(
  simulation(flows_matrix = flows_mat_walk # change for different modes
             ,region_data = london_msoa
             ,run_name = run_names_walk[k] # change for different modes
             ,graph = NULL
             ,from = origs[k]
             ,to = dests[k]
             ,graph_dist_matrix = dist_matrices[[k]]
             ,beta_offset = .7 # offset of beta calibration for different modes: cycle: 0; at: .6
             ,norm = 2
             ,time = FALSE
             ,n_cores = 3
             ,cost_fun = "exp")
  # )
}

#### results ####

### cycling
osm_geom_result <- list.load("/Users/ivann/Desktop/CASA/test_env/osm_dist_geom/osm_dist_geom_best_fit.rds")

osm_commute_result <- list.load("/Users/ivann/Desktop/CASA/test_env/osm_dist_commute/osm_dist_commute_best_fit.rds")

osm_network_result <- list.load("/Users/ivann/Desktop/CASA/test_env/osm_dist_network/osm_dist_network_best_fit.rds")

os_geom_result <- list.load("/Users/ivann/Desktop/CASA/test_env/os_dist_geom/os_dist_geom_best_fit.rds")

os_commute_result <- list.load("/Users/ivann/Desktop/CASA/test_env/os_dist_commute/os_dist_commute_best_fit.rds")

os_network_result <- list.load("/Users/ivann/Desktop/CASA/test_env/os_dist_network/os_dist_network_best_fit.rds")

dodgr_geom_result <- list.load("/Users/ivann/Desktop/CASA/test_env/dodgr_dist_geom/dodgr_dist_geom_best_fit.rds")

dodgr_commute_result <- list.load("/Users/ivann/Desktop/CASA/test_env/dodgr_dist_commute/dodgr_dist_commute_best_fit.rds")

dodgr_network_result <- list.load("/Users/ivann/Desktop/CASA/test_env/dodgr_dist_network/dodgr_dist_network_best_fit.rds")


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

beta_results <- cbind(beta = results[[1]]$beta_calib[,1]
                      ,lapply(results, FUN = function(res) res$beta_calib[,2]) %>% as.data.frame()) %>% 
  as.matrix()

{
  coul <- 1:8
  p_types <- c(15,17,18)
  l_type <- c(1:4)
  
  jpeg("quality_fit_cycle.jpg"
       ,height = 5.83
       ,width = 5.83
       ,quality = 80
       ,units = "in"
       ,res = 150)
  
  par(mar = c(4,5,4,1))
  plot(results[[1]]$beta_calib[,c(1,2)]
       ,xlim = c(0,.6)
       ,ylim = c(.55,.76)
       ,cex = 1
       ,type = "n"
       ,xlab = TeX("\\beta")
       ,ylab = TeX("$r^2$")
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

osm_geom_result_walk <- list.load("/Users/ivann/Desktop/CASA/test_env/osm_dist_geom_walk/osm_dist_geom_walk_best_fit.rds")

osm_commute_result_walk <- list.load("/Users/ivann/Desktop/CASA/test_env/osm_dist_commute_walk/osm_dist_commute_walk_best_fit.rds")

osm_network_result_walk <- list.load("/Users/ivann/Desktop/CASA/test_env/osm_dist_network_walk/osm_dist_network_walk_best_fit.rds")

os_geom_result_walk <- list.load("/Users/ivann/Desktop/CASA/test_env/os_dist_geom_walk/os_dist_geom_walk_best_fit.rds")

os_commute_result_walk <- list.load("/Users/ivann/Desktop/CASA/test_env/os_dist_commute_walk/os_dist_commute_walk_best_fit.rds")

os_network_result_walk <- list.load("/Users/ivann/Desktop/CASA/test_env/os_dist_network_walk/os_dist_network_walk_best_fit.rds")

results_walk <- list("osm_geom" = osm_geom_result_walk
                ,"osm_commute" = osm_commute_result_walk
                ,"osm_net" = osm_network_result_walk
                ,"os_geom" = os_geom_result_walk
                ,"os_commute" = os_commute_result_walk
                ,"os_net" = os_network_result_walk
                )

beta_results_walk <- cbind(beta = results_walk[[1]]$beta_calib[,1]
                      ,lapply(results_walk, FUN = function(res) res$beta_calib[,2]) %>% as.data.frame()) %>% 
  as.matrix()

{
  coul <- 1:6
  p_types <- c(15,17,18)
  l_type <- c(1:4)
  
  jpeg("quality_fit_walk.jpg"
       ,height = 5.83
       ,width = 5.83
       ,quality = 80
       ,units = "in"
       ,res = 150)
  
  par(mar = c(4,5,4,1))
  plot(results_walk[[1]]$beta_calib[,c(1,2)]
       ,xlim = c(.7,1.6)
       ,ylim = c(.8,1)
       ,cex = 1
       ,type = "n"
       ,xlab = TeX("\\beta")
       ,ylab = TeX("$r^2$")
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


# active travel summed up:

osm_geom_result_at <- list.load("/Users/ivann/Desktop/CASA/test_env/osm_dist_geom_at/osm_dist_geom_at_best_fit.rds")

osm_commute_result_at <- list.load("/Users/ivann/Desktop/CASA/test_env/osm_dist_commute_at/osm_dist_commute_at_best_fit.rds")

osm_network_result_at <- list.load("/Users/ivann/Desktop/CASA/test_env/osm_dist_network_at/osm_dist_network_at_best_fit.rds")

os_geom_result_at <- list.load("/Users/ivann/Desktop/CASA/test_env/os_dist_geom_at/os_dist_geom_at_best_fit.rds")

os_commute_result_at <- list.load("/Users/ivann/Desktop/CASA/test_env/os_dist_commute_at/os_dist_commute_at_best_fit.rds")

os_network_result_at <- list.load("/Users/ivann/Desktop/CASA/test_env/os_dist_network_at/os_dist_network_at_best_fit.rds")

results_at <- list("osm_geom" = osm_geom_result_at
                     ,"osm_commute" = osm_commute_result_at
                     ,"osm_net" = osm_network_result_at
                     ,"os_geom" = os_geom_result_at
                     ,"os_commute" = os_commute_result_at
                     ,"os_net" = os_network_result_at
)

beta_results_at <- cbind(beta = results_at[[1]]$beta_calib[,1]
                           ,lapply(results_at, FUN = function(res) res$beta_calib[,2]) %>% as.data.frame()) %>% 
  as.matrix()

{
  coul <- 1:6
  p_types <- c(15,17,18)
  l_type <- c(1:4)
  
  jpeg("quality_fit_at.jpg"
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
       ,xlab = TeX("\\beta")
       ,ylab = TeX("$r^2$")
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


