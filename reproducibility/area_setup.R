# this scripts sets up the msoa data, namely assigns all the centroids necessary for the routing

setup_area <- function(path = ''){
  
  
  if(!file.exists('data/london_msoa.rds')){
    # preprocess the London msoas
    source('scripts/network/msoa.R')
    
    # add the centroids and finish preprocessing
    source('scripts/network/msoa_centroids.R') 
    
  } else cat('london_msoa file exists, erase before recreating.\n')
  
  # cleaning the flows data set to keep only london
  source('scripts/network/flows.R')
}

setup_area()

#### checks
# london_msoa <- rlist::list.load('data/london_msoa.rds')