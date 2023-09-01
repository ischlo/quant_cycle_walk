## Make the flows matrices here 
library(foreach)
library(rlist)

if(!file.exists('test_env/flow_mat/flows_mat.rds')) {
  
  if(!dir.exists('test_env/flow_mat')) dir.create('test_env/flow_mat')
  
  ###
  
  london_msoa <- rlist::list.load('data/london_msoa.rds')
  london_msoa[,intra_dist:=intra_dist/1000]
  ###
  
  flows <- rlist::list.load("data/flows_london.rds")  |>  data.table::as.data.table()
  
  ## for cycling
  flows_mat_cycle <- foreach(i = 1:nrow(london_msoa)
                             ,.combine = rbind
                             ,.final = as.matrix) %do%
    {
      x <- rep_len(0,nrow(london_msoa))
      d <- flows[from_id == i,.(bike,to_id)] # flows
      x[d[,to_id]] <- d[,bike]
      x
    }
  
  ## for walking
  flows_mat_walk <- foreach(i = 1:nrow(london_msoa)
                            ,.combine = rbind
                            ,.final = as.matrix) %do%
    {
      x <- rep_len(0,nrow(london_msoa))
      d <- flows[from_id == i,.(foot,to_id)] # flows
      x[d[,to_id]] <- d[,foot]
      x
    }
  
  # active travel 
  flows_mat_at <- (flows_mat_cycle + flows_mat_walk)
  
  flows_mat <- list('cycle'=flows_mat_cycle
                    ,'walk'=flows_mat_walk
                    ,'at'=flows_mat_at)
  
  flows_mat <- lapply(flows_mat,FUN = \(mat) `mode<-`(mat,'integer'))
  
  rlist::list.save(flows_mat,'test_env/flow_mat/flows_mat.rds')
  
} else cat('Flows matrice already exists, erase to recreate.')
