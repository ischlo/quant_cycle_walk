Gravity model
================

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

``` r
beta_calib <- foreach::foreach(i = 28:33
                               ,.combine = rbind) %do% {
                                 beta <- 0.1*(i-1)
                                 print(paste0("RUNNING MODEL FOR beta = ",beta))
                                 run <- run_model(flows = flows_matrix
                                                  ,distance = msoa_graph_dist
                                                  ,beta = beta
                                                  ,type = "exp"
                                                  ,cores = 3
                                 )
                             
                                 cbind(beta, run$r2,run$rmse)
                               }

# beta_calib
```

``` r
run_best_fit <- run_model(flows = flows_matrix
                 ,distance = msoa_graph_dist
                 ,beta = beta_best_fit
                 ,type = "exp"
                 ,cores = 3
                 )
```
