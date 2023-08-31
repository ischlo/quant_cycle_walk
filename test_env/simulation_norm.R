# simulation of the model without networks
# the basic case of runnung a SIM with simple crow fly distances is also considered and shown in the results. 

# source("/Users/ivann/Desktop/CASA/efficient_distance.R")
norm2_geom <- london_msoa[,"centr_geom"] %>% 
  st_as_sf(wkt =1, crs = 4326) %>% 
  st_distance(by_element = FALSE)

norm2_commute <- london_msoa[,"pop_weight_geom"] %>% 
  st_as_sf(wkt =1, crs = 4326) %>% 
  st_distance(london_msoa[,"workplace_centr"] %>% 
                  st_as_sf(wkt =1, crs = 4326)
                ,by_element = FALSE)

norm2_network <- london_msoa[,"net_centr"] %>% 
  st_as_sf(wkt =1, crs = 4326) %>% 
  st_distance(by_element = FALSE)


norm2 <- list(
  "geom" = norm2_geom
  ,"commute" = norm2_commute
  ,"network" = norm2_network
)


lapply(norm2,FUN = function(x) which(is.na(x),arr.ind = TRUE))

flows_mat_at

runs_norm <- c("norm2_geom"
               ,"norm2_commute"
               ,"norm2_network")

origs_norm2 <- c("centr_geom"
           ,"pop_weight_geom"
           ,"net_centr"
)

dests_norm2 <- c("centr_geom"
           ,"workplace_centr"
           ,"net_centr"
)

for(k in 1:length(runs_norm)) { # check length of run names
  
  simulation(flows_matrix = flows_mat_at # change for different modes
             ,region_data = london_msoa
             ,run_name = runs_norm[k] # change for different modes
             ,graph = NULL
             ,from = origs_norm2[k]
             ,to = dests_norm2[k]
             ,graph_dist_matrix = NULL
             ,beta_offset = .7 # offset of beta calibration for different modes: cycle: 0; at: .4
             ,norm = 2
             ,time = FALSE
             ,n_cores = 3
             ,cost_fun = "exp")

}


#### analysis of results, see simulation_selected.R ####

norm2_geom_results <- list.load("/Users/ivann/Desktop/CASA/test_env/norm2_geom/norm2_geom_best_fit.rds")

norm2_commute_results <- list.load("/Users/ivann/Desktop/CASA/test_env/norm2_commute/norm2_commute_best_fit.rds")

norm2_network_results <- list.load("/Users/ivann/Desktop/CASA/test_env/norm2_network/norm2_network_best_fit.rds")
