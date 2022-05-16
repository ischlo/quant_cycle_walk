Gravity model
================

``` r
flows_matrix <- list.load(here::here("gravity_model_files/flows_matrix.rds"))

dist_matrix_cwp <- list.load(here::here("RC_outputs/pop_weighted_centroids/dist_matrix_london.rds"))

msoa_graph_dist <- list.load(here::here("RC_outputs/london_all_run/dist_matrix_london.rds"))

msoa_graph_dist <- msoa_graph_dist / 1000 / 14 %>% round
```

This file will cover the process of building a local version of the
gravity model used to predict cycling and/or walking flows across
London.

# What model to use ?

The general version of the doubly constrained gravity model looks the
following way: \[ T_{ij} = A_iB_jO_iD_jf(c_{ij}) \] where \(O_i\) is the
working population of the origin, and \(D_j\) is the available
workplaces at the destination location: \[ O_i = \sum_j T_{ij} \]
\[ D_j = \sum_i T_{ij} \]

The terms \(A_i\),\(B_j\) are factors for each location. The derivation
of these factors is based on the relation:

\[A_i = [\sum_j B_jD_jf(c_{ij})]^{-1}\]

\[B_j = [\sum_i A_iO_if(c_{ij})]^{-1}\]

with the derivation made from a recursive chain with initial values 1.
Let’s refer to the parameters above as vectors
\(\vec{A}\),\(\vec{B}\),\(\vec{O}\),\(\vec{D}\), and to the cost
function and flow as matrices **F** and **T** such that
\(F_{ij}=f(c_{ij})\) and \(T_{ij}\) is a flow from i to j.

``` r
# creating the O, D vectors. 
O <- apply(flows_matrix,reduce, MARGIN = 2, sum) %>% c()

D <- apply(flows_matrix,reduce, MARGIN = 1, sum) %>% c()
```

``` r
F_c <- cost_function(msoa_graph_dist,1,type = "exp")
```

Next, we need to run the recursive procedure until the values stabilise.
We introduce the threshold at which we will stop running the recursion
\(\delta\). It corresponds to the rate of change of the parameter with
respect to the previous iteration.
