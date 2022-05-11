### libs

library(tmap)
library(tmaptools)
library(tidyverse)
library(data.table)
library(cppRouting)
library(foreach)
library(doParallel)
library(units)
library(reshape2)
library(rlist)
library(sf)
library(osmextract)
library(dodgr)
library(rbenchmark)
library(microbenchmark)
library(profvis)

#### source files 

source(here::here("function.R"))
source(here::here("gravity_model_functions.R"))
source(here::here("workflow.R"))

#### load data 

graph <- list.load(here::here("/lustre/home/ucfnisc/Scratch/RC_scripts/data/london_graph_simple.rds"))

city_data <- list.load(here::here("/lustre/home/ucfnisc/Scratch/RC_scripts/data/london_msoa.rds")) %>% 
  as.data.table()

flows_matrix <- list.load(here::here("/lustre/home/ucfnisc/Scratch/RC_scripts/data/flows_matrix.rds")) %>% 
  as.data.table()

flows <-  list.load(here::here("/lustre/home/ucfnisc/Scratch/RC_scripts/data/flows.rds")) %>% 
  as.data.table()

n_cores <- 12


#### Simulation ####

# graph <- london_graph_simple

city_data <- city_data[grep("Camden",geo_name),]

flows_london_sample <- flows[(workplace %in% city_data$geo_code) &
                                      (residence %in% city_data$geo_code),]

flows_matrix <- foreach(i = 1:length(unique(city_data[,id]))
                        ,.combine = rbind
                        ,.final = as.matrix) %do%
  {
    x <- rep_len(0,nrow(city_data))
    d <- flows_london_sample[from_id == (i + 160),.(bike,to_id)]
    x[d[,to_id]%% 160 ] <- d[,bike]
    x
  }

# view(flows_matrix)

run_name <- "osm_unfilt"
centroids <-  "centr_geom"

profvis({
  # test of the function is successful.
  registerDoParallel(cores = n_cores)
  test_simu <- simulation(london_graph_simple
                          ,flows_matrix = flows_matrix
                          ,city_data = city_data
                          ,run_name = run_name
                          ,centroids = centroids)
  stopImplicitCluster()
  
})


# simulation(graph = graph
#            ,city_data = city_data
#            ,flows_matrix = flows_matrix
#            ,run_name = "osm_unfilt"
#            ,centroids = "pop_weight_geom")


