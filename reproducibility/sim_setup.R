## Setting up the modelling part and running

# preliminary things: generating flow matrices, contracting graphs and grouping them into one network object.
# techniically a list of lists. 
source('test_env/flows_mat.R')
source('test_env/networks.R')

# computing delta matrices
source('test_env/delta_matrices.R')

# computing distance matrices
source('test_env/distance_matrices.R')

#  NOT ALL THE FOLLOWING SCRIPTS DO THE RIGHT CHECKS before running, adjust.

# running the models
# network distance matrices
source('test_env/sim.R')

# norm2 distance matrices
source('/test_env/simulation_norm.R')

# generating images from sim results
source('test_env/simulation_selected.R')






