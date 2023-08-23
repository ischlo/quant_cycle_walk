# this scripts sets up the msoa data, namely assigns all the centroids necessary for the routing

setup_area <- function(path = ''){
  
  # preprocess the London msoas
  source('scripts/network/msoa.R')
  
  # add the centroids and finish preprocessing
  source('scripts/network/msoa_centroids.R')
}

setup_area()

#### checks
# london_msoa <- rlist::list.load('data/london_msoa.rds')