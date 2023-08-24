# not an important script, it does some visualisations that did not end up used. 
library(rlist)
library(tidyverse)
library(microbenchmark)
library(sf)
library(tmap)
library(data.table)
library(units)
library(doParallel)
library(foreach)

graph <- list.load("../data/london_graph_simple.rds")

city <- list.load("../data/london_msoa.rds") %>% 
  factor_to_character() %>% 
  as.data.table() # %>% st_as_sf(crs = 4326)

flows <- list.load("../data/flows_london.rds") %>%
  as.data.table()

graph_dist_matrix <- list.load("../data/dist_matrix_london.rds")

flows_matrix <- list.load("../gravity_model_files/flows_matrix.rds")

flows_matrix %>% typeof

files <- function(name) {paste0(name,".rds")}

outputs <- c("norm2_commute/norm2_commute_best_fit"
             ,"norm2_geom/norm2_geom_best_fit"
             ,"norm2_network/norm2_network_best_fit"
             ,"osm_norm2_commute/osm_norm2_commute_best_fit"
             ,"osm_norm2_standard/osm_norm2_standard_best_fit"
             ,"osm_norm2_network/osm_norm2_network_best_fit")

names <- c("Euclid, Commute centroids"
           ,"Euclid, Geometric centroids"
           ,"Euclid, network centroids"
           ,"OSM, Euclid, Commute centroids"
           ,"OSM, Euclid, Geometric centroids"
           ,"OSM, Euclid, network centroids"
           )

fits <- lapply(outputs, function(x) {list.load(files(x))})

par(mfrow = c(2,3))
fits %>% mapply(names,FUN = function(fit_list,name)  {
  print(name)
  plot(fit_list$best_fit
       ,flows_matrix
       ,log = "xy"
       ,pch = 1
       ,cex = 0.5
       ,xlim = c(.5,150)
       ,ylim = c(.5,150)
       ,ylab = ""
       ,xlab = ""
       ,main = name
  )
  lines(1:max(fit_list$best_fit),1:max(fit_list$best_fit)
        ,col = "darkred")
})
par(mfrow = c(1,1))

best_fit <- list.load("osm_norm2_commute/osm_norm2_commute_best_fit.rds")

plot(best_fit$values
     ,flows_matrix
     ,log = "xy"
     ,cex = .7
     ,pch = 1)



o_m <- apply(best_fit$values, sum, MARGIN = 1)
o_o <- apply(flows_matrix, sum, MARGIN = 1)

d_m <- apply(best_fit$values, sum, MARGIN = 2)
d_o <- apply(flows_matrix, sum, MARGIN = 2)

flows_resid_destination <- (best_fit$values - flows_matrix) %>% apply(MARGIN = 2,FUN = sum)
flows_resid_origin <- (best_fit$values - flows_matrix) %>% apply(MARGIN = 1,FUN = sum)

city[,residuals_destination:=NULL]
city[,residuals_origin:=NULL]
city %>% st_as_sf(crs = 4326, wkt = "geometry") %>% tm_shape() + tm_polygons(col = c("residuals_destination","residuals_origin")
                                                        ,palette = "viridis"
                                                        ,style = "pretty")
# vectorizing the map plotting

# par(mfrow = c(6,2))

fits %>% mapply(names,FUN = function(fit_list,name)  {
  
  flows_resid_destination <- (fit_list$values - flows_matrix) %>% apply(MARGIN = 2,FUN = sum) %>% as.numeric()
  flows_resid_origin <- (fit_list$values - flows_matrix) %>% apply(MARGIN = 1,FUN = sum) %>% as.numeric()
  
  map <- data.frame(geometry = city[,geometry],residuals_destination=flows_resid_destination,residuals_origin=flows_resid_origin) %>% 
    st_as_sf(crs = 4326, wkt = "geometry") %>% 
    tm_shape() + tm_polygons(col = c("residuals_destination","residuals_origin")
                             ,palette = "viridis"
                             ,style = "pretty"
                             ) + 
    tm_layout(
      main.title = name
      #,title.snap.to.legend = TRUE
      # ,panel.show = TRUE
      )
  
  map %>% tmap_save(paste0(name,".pdf"))
})
# par(mfrow = c(1,1))

#### computing distances from nodes. 
# 
# # ref dist
# london_msoa <- list.load("/Users/ivann/Desktop/CASA/data/london_msoa.rds") %>% as.data.table()
# 
# euclid_dist <- london_msoa[,"centr_geom"] %>% 
#   st_as_sf(wkt = 1,crs = 4326) %>% 
#   st_distance() %>% round() %>% set_units(NULL)
# 
# euclid_dist <- `diag<-`(euclid_dist
#                         ,delta_matrix(osm$coords[match(osm_nodes,osmid),.(x,y)] %>% st_as_sf(coords = c(1,2),crs = 4326)
#                                       ,london_msoa[,"centr_geom"] %>% st_as_sf(wkt = 1,crs = 4326)
#                                       ,type = "vec")
# )
# 
# # data 
# osm <- list.load("../benchmarks/osm_all_graph.rds")
# osm_nodes <- list.load("../benchmarks/osm_all_nodes.rds")
# 
# os_data <- list.load("../benchmarks/os_network_data.rds")
# os <- os_data$cppr_graph
# os_nodes <- os$coords$osmid[os_data$nn_ind]
# 
# dodgr_cycle <- list.load("../benchmarks/dodgr_cycle_graph.rds")
# dodgr_cycle_nodes <- list.load("../benchmarks/dodgr_cycle_noi.rds")
# 
# osm_euclid_dist <- osm$coords[match(osm_nodes,osmid),.(x,y)] %>% 
#   st_as_sf(coords = c(1,2),crs = 4326) %>% 
#   st_distance() %>% round %>% set_units(NULL)
# 
# which(euclid_dist < 15000, arr.ind = TRUE) %>% nrow()
# 
# delta %>% hist()
# 
# comparison <- ((delta + osm_euclid_dist)/euclid_dist)
# 
# comparison %>% as.numeric() %>% hist()
# 
# ## looking at outliers
# outliers <- which(comparison<1,arr.ind = TRUE)
# 
# outliers_lines <- get_lines(london_msoa[outliers[,1],"centr_geom"] %>% st_as_sf(wkt = 1,crs = 4326)
#           ,to = london_msoa[outliers[,2],"centr_geom"] %>% st_as_sf(wkt = 1,crs = 4326)) 
# 
# tmap_mode("view")
# outliers_lines %>% qtm() + (london_msoa[outliers[,1],"centr_geom"] %>% st_as_sf(wkt = 1,crs = 4326) %>% qtm()) +
#   (osm$coords[match(osm_nodes,osmid),.(x,y)] %>% st_as_sf(coords = c(1,2),crs = 4326) %>% qtm(dots.col = "red"))
