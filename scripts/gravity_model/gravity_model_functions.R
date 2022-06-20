### this file contains the functions used to run the gravity model


cost_function <- function(d, beta, type = "exp") {
  if(type == "exp") {
    exp(-beta*d)
  } else if(type == "pow") {
    d^-beta
  } else {
    print("provide a type of functino to compute")
  }
}

r_2 <- function(d, f) cor(d %>% as.numeric()
                          ,f %>% as.numeric())^2
r <- function(d, f) sqrt(r_2(d,f))

rmse <- function(d,f) sum((d-f)^2)

calibration <- function(cost_fun,O,D,delta = 0.05) {
  B <- rep_len(1,nrow(cost_fun))
  eps <- abs(sum(B))
  e <- NULL
  i <- 0
  while((eps > delta) & (i<50)) {
    A_new <- 1/(apply(cost_fun,function (x) sum(B*D*x),MARGIN = 1))
    B_new <- 1/(apply(cost_fun,function (x) sum(A_new*O*x),MARGIN = 2))
    eps <- abs(sum(B_new-B))
    e <- append(e,eps)
    A <- A_new
    B <- B_new
    i <- i+1
  }
  list(
    "A"= A
    ,"B" = B
    ,"e" = e
    )
}

run_model <- function(flows
                      ,distance
                      ,beta = 0.25
                      ,type = "exp"
                      #,cores = 3
                      ) {
  
  F_c <- cost_function(d = {{distance}},beta = {{beta}},type = type)
  print("cost function computed")
  O <- apply(flows,sum, MARGIN = 1) %>% as.integer()
  D <- apply(flows,sum, MARGIN = 2) %>% as.integer()
  A_B <- calibration(cost_fun = F_c
                     ,O=O
                     ,D=D
                     ,delta = .001)
  print("calibration: over")
  A <- A_B$A
  B <- A_B$B

  flows_model <- foreach(j = c(1:nrow(F_c))
                         ,.combine = rbind) %do% {
                           round(A[j]*B*O[j]*D*F_c[j,])
                         }
  
  print("model run: over")
  e_sor <- e_sorensen(flows,flows_model) %>% as.numeric()
  print(paste0("E_sor = ",e_sor))
  r2 <- r_2(flows_model,flows) %>% as.numeric()
  print(paste0("r2 = ",r2))
  RMSE <- rmse(flows_model,flows) %>% as.numeric()
  print(paste0("RMSE = ",RMSE))
  
  list("values" = flows_model
       ,"r2" = r2
       ,"rmse" = RMSE
       ,"calib" = A_B$e
       ,"e_sor" = e_sor)
  
}

### Validation

e_sorensen <- function(data, fit) {
  2*sum(apply(cbind(data %>% c
                    ,fit %>% c), MARGIN = 1, FUN = min))/(sum(data) + sum(fit))
}



