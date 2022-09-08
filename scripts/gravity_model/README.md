This file will cover the process of building a local version of the
gravity model used to predict cycling and/or walking flows across
London.

What model to use ?
===================

The general version of the doubly constrained gravity model looks the
following way:
*T*<sub>*i**j*</sub> = *A*<sub>*i*</sub>*B*<sub>*j*</sub>*O*<sub>*i*</sub>*D*<sub>*j*</sub>*f*(*c*<sub>*i**j*</sub>)
 where *O*<sub>*i*</sub> is the working population of the origin, and
*D*<sub>*j*</sub> is the available workplaces at the destination
location:
*O*<sub>*i*</sub> = ∑<sub>*j*</sub>*T*<sub>*i**j*</sub>
*D*<sub>*j*</sub> = ∑<sub>*i*</sub>*T*<sub>*i**j*</sub>

The terms *A*<sub>*i*</sub>,*B*<sub>*j*</sub> are factors for each
location. The derivation of these factors is based on the relation:

*A*<sub>*i*</sub> = \[∑<sub>*j*</sub>*B*<sub>*j*</sub>*D*<sub>*j*</sub>*f*(*c*<sub>*i**j*</sub>)\]<sup> − 1</sup>

*B*<sub>*j*</sub> = \[∑<sub>*i*</sub>*A*<sub>*i*</sub>*O*<sub>*i*</sub>*f*(*c*<sub>*i**j*</sub>)\]<sup> − 1</sup>

with the derivation made from a recursive chain with initial values 1.
Let’s refer to the parameters above as vectors *A⃗*,*B⃗*,*O⃗*,*D⃗*, and
to the cost function and flow as matrices **F** and **T** such that
*F*<sub>*i**j*</sub> = *f*(*c*<sub>*i**j*</sub>) and
*T*<sub>*i**j*</sub> is a flow from i to j.

    # creating the O, D vectors. 
    O <- apply(flows_matrix,reduce, MARGIN = 2, sum) %>% c()

    D <- apply(flows_matrix,reduce, MARGIN = 1, sum) %>% c()

    F_c <- cost_function(msoa_graph_dist,1,type = "exp")

Next, we need to run the recursive procedure until the values stabilise.
We introduce the threshold at which we will stop running the recursion
*δ*. It corresponds to the rate of change of the parameter with respect
to the previous iteration.

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

    run_best_fit <- run_model(flows = flows_matrix
                     ,distance = msoa_graph_dist
                     ,beta = beta_best_fit
                     ,type = "exp"
                     ,cores = 3
                     )
