### Routing workflow, you need to have run the netowork setup before running this. 

# if the network initiation has not been done yet, uncomment here. 
# source("network_setup.R")

source("scripts/network/cppr_network_setup.R")

# analyse the flows data and produce summary statistics
# source("scripts/routing/flows.R")
# source("scripts/routing/routing.R")

# the benchmark of the different packages for speed. 
source('benchmarks/routing_benchmarks.R')

# The routing of various profiles for one type of centroids. 
source('benchmarks/distances_benchmarks.R')


