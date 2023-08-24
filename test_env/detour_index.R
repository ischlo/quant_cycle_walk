# this produces the image that was not used in the end comparing the quality of fit with the detour. 

library(rlist)
library(data.table)
library(sf)
library(Btoolkit)
library(cppSim)
library(foreach)

## 

london_msoa <-  rlist::list.load('../data/london_msoa.rds') |>
  Btoolkit::factor_to_character() |> 
  as.data.table()

######
# reading in flows

flows <- list.load("../data/flows_london.rds") %>% as.data.table()

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

flows_mat <- `mode<-`(flows_mat,"integer")

######
# # crow fly distances

norm2_geom <- london_msoa[,"centr_geom"] %>% 
  st_as_sf(wkt =1, crs = 4326) %>% 
  st_distance(by_element = FALSE) |>
  units::set_units(NULL)

norm2_commute <- london_msoa[,"pop_weight_geom"] %>% 
  st_as_sf(wkt =1, crs = 4326) %>% 
  st_distance(london_msoa[,"workplace_centr"] %>% 
                st_as_sf(wkt =1, crs = 4326)
              ,by_element = FALSE) |>
  units::set_units(NULL)

norm2_network <- london_msoa[,"net_centr"] %>% 
  st_as_sf(wkt =1, crs = 4326) %>% 
  st_distance(by_element = FALSE) |>
  units::set_units(NULL)


norm2 <- list(
  "geom" = norm2_geom
  ,"commute" = norm2_commute
  ,"network" = norm2_network
)


#####
# detour index for cycling:
# distance matrices: 
#osm
osm_dist_geom <- list.load("../benchmarks/osm_dist_matrix_standard.rds")

osm_dist_commute <- list.load("../benchmarks/osm_dist_matrix_commute.rds")

osm_dist_network <- list.load("../benchmarks/osm_dist_matrix_network.rds")

# os
os_dist_geom <- list.load("../benchmarks/os_dist_matrix_geom.rds")

os_dist_commute <- list.load("../benchmarks/os_dist_matrix_commute.rds")

os_dist_network <- list.load("../benchmarks/os_dist_matrix_network.rds")

# dodgr

dodgr_dist_geom <- list.load("../benchmarks/dodgr_dists_geom.rds")

dodgr_dist_commute <- list.load("../benchmarks/dodgr_dists_commute.rds")

dodgr_dist_network <- list.load("../benchmarks/dodgr_dists_network.rds")

# grouping into a list

dist_matrices <- list("osm_dist_geom"=osm_dist_geom
                      ,"osm_dist_commute"=osm_dist_commute
                      ,"osm_dist_network"=osm_dist_network
                      ,"os_dist_geom"=os_dist_geom
                      ,"os_dist_commute"=os_dist_commute
                      ,"os_dist_network"=os_dist_network
                      ,"dodgr_dist_geom"=dodgr_dist_geom
                      ,"dodgr_dist_commute"=dodgr_dist_commute
                      ,"dodgr_dist_network"=dodgr_dist_network
)

#####
# detour for cycling

detour_index <- mapply(dist_matrices
                       ,norm2
                       ,SIMPLIFY = FALSE
                       ,FUN = function(d,n) {
                         n <- `diag<-`(n,diag(d))
                         as.numeric(d/n)
                         })

lapply(detour_index, FUN = function(m) m[which(m != 1)] |> summary())

#### GRAVITY model with cppSim

sim <- lapply(dist_matrices, FUN = function(d) {cppSim::simulation(flows_matrix = flows_mat
                                                                   ,dist_matrix = d/1000) })


# correlation of cppSim results
lapply(sim, FUN = function(d) {cor(d[[1]] |> as.numeric()
                                   ,flows_mat |> as.numeric())^2})


## Gravity model results

osm_geom_result <- list.load("osm_dist_geom/osm_dist_geom_best_fit.rds")
osm_commute_result <- list.load("osm_dist_commute/osm_dist_commute_best_fit.rds")
osm_network_result <- list.load("osm_dist_network/osm_dist_network_best_fit.rds")
os_geom_result <- list.load("os_dist_geom/os_dist_geom_best_fit.rds")
os_commute_result <- list.load("os_dist_commute/os_dist_commute_best_fit.rds")
os_network_result <- list.load("os_dist_network/os_dist_network_best_fit.rds")
dodgr_geom_result <- list.load("dodgr_dist_geom/dodgr_dist_geom_best_fit.rds")
dodgr_commute_result <- list.load("dodgr_dist_commute/dodgr_dist_commute_best_fit.rds")
dodgr_network_result <- list.load("dodgr_dist_network/dodgr_dist_network_best_fit.rds")

# grouping all the results
results <- list("osm_geom" = osm_geom_result
                ,"osm_commute" = osm_commute_result
                ,"osm_net" = osm_network_result
                ,"os_geom" = os_geom_result
                ,"os_commute" = os_commute_result
                ,"os_net" = os_network_result
                ,"dodgr_geom" = dodgr_geom_result
                ,"dodgr_commute" = dodgr_commute_result
                ,"dodgr_network" = dodgr_network_result)

cbind(lapply(results, FUN = function(res) res$beta_calib[which.max(res$beta_calib[,2]),c(2)])
      ,lapply(sim, function(l) {l[[2]]}))

lapply(detour_index, FUN = function(m) m[which(m != 1)] |> mean())

detour_qfit <- mapply(sim
                      ,detour_index
                      # ,SIMPLIFY = FALSE
                      ,FUN = function(res,m) {c(cor(res[[1]] |> as.numeric()
                                                    ,flows_mat |> as.numeric())^2
                                                ,mean(m[which(m != 1)]))}) |> as.matrix() |> t()

detour_qfit

pchs <- c(1,15,24)
cols <- c("darkred","darkred","darkred","navyblue","navyblue","navyblue","green","green","green")

# tutorial on how to annotate a la latex in R plots
# https://data.library.virginia.edu/mathematical-annotation-in-r/

plot(detour_qfit
     ,main = "Detour, quality of fit"
     ,pch = pchs
     ,col = cols
     ,xlab = expression(r^2)
     ,ylab = expression(delta)
     ,cex.lab = 1.4
     )
legend(x = .750
       ,y = 1.37
       ,title = "Centroid"
       ,pch = pchs
       ,legend = c("geom","commute","network")
       ,bty = "n"
       # ,trace = TRUE
       )
legend(x = .750
       ,y = 1.31
       ,title = "Network"
       ,legend = c("OSM","OS","Dodgr")
       # ,pch = 16
       ,fill = unique(cols)
       # ,lty = 1
       ,col = unique(cols)
       ,bty = "n"
       # ,trace = TRUE
       )

