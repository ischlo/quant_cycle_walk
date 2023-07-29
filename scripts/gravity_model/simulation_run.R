### libs
# gc()

library(tmap)
library(tmaptools)
library(tidyverse)
library(data.table)
library(cppRouting)
library(foreach)
library(doParallel)
library(parallel)
library(units)
library(reshape2)
library(rlist)
library(sf)
library(here)
library(parallel)


options(expressions=10000)

# R.version

#### source files 

# print("packs loaded")
# source(here::here("/lustre/home/ucfnisc/Scratch/RC_scripts/functions.R"))
# 
# source(here::here("/lustre/home/ucfnisc/Scratch/RC_scripts/workflow.R"))
# 
# source(here::here("/lustre/home/ucfnisc/Scratch/RC_scripts/gravity_model_functions.R"))
# 
# #### load data ####
# 
# city <- list.load(here::here("/lustre/home/ucfnisc/Scratch/RC_scripts/data/london_msoa.rds")) %>% 
#   as.data.table() # %>% st_as_sf(crs = 4326)
# 
# # geom_centr <- st_read(here::here("/lustre/home/ucfnisc/Scratch/RC_scripts/data/london_centr_geom.geojson"))%>% st_as_sf(crs = 4326)
# # 
# # pw_centr <- st_read(here::here("/lustre/home/ucfnisc/Scratch/RC_scripts/data/london_pw_centr.geojson"))%>% st_as_sf(crs = 4326)
# 
# graph <- list.load(here::here("/lustre/home/ucfnisc/Scratch/RC_scripts/data/london_graph_simple.rds"))
# 
# flows <- list.load(here::here("/lustre/home/ucfnisc/Scratch/RC_scripts/data/flows_london.rds")) %>%
#   as.data.table()
# 
# graph_dist_matrix <- list.load(here::here("/lustre/home/ucfnisc/Scratch/RC_scripts/data/dist_matrix_london.rds"))

# flows_mat <- list.load(here::here("/lustre/home/ucfnisc/Scratch/RC_scripts/data/flows_matrix.rds"))

#### files for local download

graph <- list.load("../data/london_graph_simple.rds")

city <- list.load("../data/london_msoa.rds") %>% as.data.table()# %>% st_as_sf(crs = 4326)

# geom_centr <- st_read("../data/london_centr_geom.geojson") %>% st_as_sf(crs = 4326)

# pw_centr <- st_read("../data/london_pw_centr.geojson") %>% st_as_sf(crs = 4326)

flows <- list.load("../data/flows_london.rds") %>%
  as.data.table()

#### Simulation ####

city <- city[grepl("Camden",city$geo_name) |
              grepl("Islington",city$geo_name) |
             grepl("City of London",city$geo_name) #|
             # grepl("Westminster",city$geo_name) |
             # grepl("Southwark",city$geo_name) |
             # grepl("Hackney",city$geo_name) |
             # grepl("Haringey", city$geo_name)
             ,]

city[,"id"] <- 1:nrow(city)

flows_london_sample <- flows[(workplace %in% city$geo_code) &
                                      (residence %in% city$geo_code),-c("from_id","to_id")]

flows_london_sample <- flows_london_sample %>% merge(city[,c("geo_code","id")] #%>% st_drop_geometry()
                                       ,by.x = "residence"
                                       ,by.y = "geo_code")

flows_london_sample <- flows_london_sample %>% merge(city[,c("geo_code","id")] #%>% st_drop_geometry()
                                       ,by.x = "workplace"
                                       ,by.y = "geo_code")

setnames(flows_london_sample
         ,old = c("id.x","id.y")
         ,new = c("from_id","to_id")
)

flows_mat <- foreach(i = city[,id]
                     ,.combine = rbind
                     ,.final = as.matrix) %do%
  {
    x <- rep_len(0,nrow(city))
    d <- flows_london_sample[from_id == i,.(bike,to_id)] # flows
    x[d[,to_id]] <- d[,bike]
    x
  }

cores <- 2

print("flows matrix computed locally")

# view(flows_matrix)

# test of the function is successful.
print(" finished all the preliminary steps, starting the simulation ")

# #### OSM, norm1, pop_weight ####
# 
run <- "osm_unfilt_norm1"
# centr <- "pop_weight_geom"

# if(centr == "pop_weight_geom") {
#   city <- pw_centr
# } else if(centr == "centr_geom") {
#   city <- geom_centr
# }

try({
  registerDoParallel(cores = cores)
  test_simu <- simulation(graph = graph
                          ,graph_dist_matrix = NULL
                          ,flows_matrix = flows_mat
                          ,region_data = city #%>% st_as_sf(sf_column_name = "geometry")
                          ,from = "pop_weight_geom"
                          ,to = "workplace_centr"
                          ,norm = 1
                          ,time = FALSE
                          ,run_name = run
                          #,centroids = centr
                          ,n_cores = cores
  )
  stopImplicitCluster()
})

# 
#### Parameters #### 

# run <- "osm_unfilt_norm2"
# centr <- "pop_weight_geom"

#### Simulation ####

#### NULL, norm1, geom_centr ####

# run <- "norm1"
# centr <- "centr_geom"
# 
# if(centr == "pop_weight_geom") {
#   city <- pw_centr
# } else if(centr == "centr_geom") {
#   city <- geom_centr
# }
# 
# try(
#   {
#     registerDoParallel(cores = cores)
#     test_simu <- simulation(graph = NULL
#                             ,flows_matrix = flows_mat
#                             ,city_data = city
#                             ,norm_p = 1
#                             ,time = FALSE
#                             ,run_name = run
#                             # ,centroids = centr
#                             ,n_cores = cores
#     )
#     stopImplicitCluster()
#   })

#### NULL, norm 2, centr_geom #### 

# run <- "norm2"
# 
# centr <- "centr_geom"
# 
# if(centr == "pop_weight_geom") {
#   city <- pw_centr
# } else if(centr == "centr_geom") {
#   city <- geom_centr
# }
# 
# try({
#   registerDoParallel(cores = cores)
#   test_simu <- simulation(graph = NULL
#                           ,flows_matrix = flows_mat
#                           ,city_data = city
#                           ,norm_p = 2
#                           ,time = FALSE
#                           ,run_name = run
#                           # ,centroids = centr
#                           ,n_cores = cores
#   )
#   stopImplicitCluster()
# })


#### NULL; norm2, pop_weight ####
# 
# run <- "norm2"
# 
# centr <- "pop_weight_geom"
# 
# if(centr == "pop_weight_geom") {
#   city <- pw_centr
# } else if(centr == "centr_geom") {
#   city <- geom_centr
# }
# 
# try({
#   registerDoParallel(cores = cores)
#   test_simu <- simulation(graph = NULL
#                           ,flows_matrix = flows_mat
#                           #,graph_dist_matrix = graph_dist_matrix
#                           ,city_data = city
#                           ,norm_p = 2
#                           ,time = FALSE
#                           ,run_name = run
#                           # ,centroids = centr
#                           ,n_cores = cores
#   )
#   stopImplicitCluster()
#   })

 #
# #### NULL, norm1, pop_weight ####
# run <- "norm1"
# 
# centr <- "pop_weight_geom"
# 
# if(centr == "pop_weight_geom") {
#   city <- pw_centr
# } else if(centr == "centr_geom") {
#   city <- geom_centr
# }
# 
# try({
#   registerDoParallel(cores = cores)
#   test_simu <- simulation(graph = NULL
#                           ,flows_matrix = flows_mat
#                           ,city_data = city
#                           #,graph_dist_matrix = graph_dist_matrix
#                           ,norm_p = 1
#                           ,time = FALSE
#                           ,run_name = run
#                           # ,centroids = centr
#                           ,n_cores = cores
#   )
#   stopImplicitCluster()
# })

# 
# #### OSM, norm2, pop_weight  ####

# 
# run <- "osm_unfilt_norm2"
# 
# centr <- "pop_weight_geom"
# 
# if(centr == "pop_weight_geom") {
#   city <- pw_centr
# } else if(centr == "centr_geom") {
#   city <- geom_centr
# }
# 
# try({
#   registerDoParallel(cores = cores)
#   test_simu <- simulation(graph = graph
#                           ,flows_matrix = flows_mat
#                           ,city_data = city
#                           ,norm_p = 2
#                           ,time = FALSE
#                           ,run_name = run
#                           # ,centroids = centr
#                           ,n_cores = cores
#   )
#   stopImplicitCluster()
# })
# 
# #### OSM, norm 1, geom_centr ####
# 
# run <- "osm_unfilt_norm1"
# 
# centr <- "centr_geom"
# 
# if(centr == "pop_weight_geom") {
#   city <- pw_centr
# } else if(centr == "centr_geom") {
#   city <- geom_centr
# }
# 
# try({
#   registerDoParallel(cores = cores)
#   test_simu <- simulation(graph = graph
#                           ,flows_matrix = flows_mat
#                           ,city_data = city
#                           ,norm_p = 1
#                           ,time = FALSE
#                           ,run_name = run
#                           # ,centroids = centr
#                           ,n_cores = cores
#   )
#   stopImplicitCluster()
# })