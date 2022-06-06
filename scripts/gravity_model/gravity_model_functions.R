### this file contains the functions used to run the gravity model


cost_function <- function(d, beta, type = "exp") {
  if(type == "exp") {
    exp(-beta*d)
  } else if(type == "pow") {
    d^-beta
  } else {
    print("provide all the necessary")
  }
}

r_2 <- function(d, f) cor(d %>% c()
                          ,f %>% c())
r <- function(d, f) sqrt(r_2(d,f))

rmse <- function(d,f) sum((d-f)^2)


calibration <- function(cost_fun,O,D,delta = 0.1) {
  B <- rep_len(1,nrow(cost_fun))
  eps <- abs(sum(B))
  e <- NULL
  i <- 0
  while((eps > delta) & (i<50)) {
    A_new <- 1/(apply(cost_fun,function (x) sum(B*D*x),MARGIN = 2))
    B_new <- 1/(apply(cost_fun,function (x) sum(A_new*O*x),MARGIN = 1))
    eps <- abs(sum(B_new-B))
    e <- append(e,eps)
    A <- A_new
    B <- B_new
    i <- i+1
  }
  list(
    "A"=A
    ,"B" = B
    ,"e" = e
    )
}


run_model <- function(flows
                      ,distance,beta = 0.1
                      ,type = "exp"
                      #,cores = 3
                      ) {
  
  F_c <- cost_function(d = {{distance}},beta = {{beta}},type = type)
  print("cost function computed")
  O <- apply(flows,reduce, MARGIN = 2, sum) %>% c()
  D <- apply(flows,reduce, MARGIN = 1, sum) %>% c()
  A_B <- calibration(F_c,O,D,delta = 1)
  print("calibration: over")
  A <- A_B$A
  B <- A_B$B
  
  # registerDoParallel(cores = cores)
  flows_model <- foreach(i = c(1:nrow(F_c))
                         ,.combine = cbind) %dopar% {
                           round(A[i]*B*O[i]*D*F_c[i,])
                         }
  e_sor <- e_sorensen(flows,flows_model)
  # stopImplicitCluster()
  print("model run: over")
  r2 <- r_2(flows_model,flows)
  RMSE <- rmse(flows_model,flows)
  
  list("values" = flows_model
       ,"r2"=r2
       ,"rmse" = RMSE
       ,"calib" = A_B$e
       ,"e_sor" = e_sor)
  
}


### Validation


e_sorensen <- function(data, fit) {
  2*sum(apply(cbind(data %>% c
                    ,fit %>% c), MARGIN = 1, FUN = min))/(sum(data) + sum(fit))
}



