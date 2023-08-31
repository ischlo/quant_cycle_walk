### Routing workflow, you need to have run the netowork setup before running this. 

# if the network initiation and area setup hasn't happened, uncomment.
# source("reproducibility/network_setup.R")
# source('reproducibility/area_setup.R')

source("scripts/network/cppr_network_setup.R")

# Before being able to do routing, find the nearest nodes in the graphs to the centroids of choice.
source('scripts/routing/centroids.R')

# analyse the flows data and produce summary statistics
# source("scripts/routing/flows.R")

# the benchmark of the different packages for speed. 
# source('benchmarks/routing_benchmarks.R')

# The routing of various profiles for one type of centroids. and first distance matrices. 
# source('benchmarks/distances_benchmarks.R')


