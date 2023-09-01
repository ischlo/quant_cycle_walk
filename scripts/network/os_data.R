library(data.table)
library(sf)
library(dplyr)
library(rlist)

## OS data 

if(file.exists("benchmarks/cppr_networks/os_graph.rds")) {
  cat('OS all network already exists.\n'
      ,'If you want to regenerate, erase first\n')
} else {
  os_london <- st_read("data/OS_network/TQ_RoadLink.shp") |> st_transform(4326) |> as.data.table()
  
  london_msoa <- list.load("data/london_msoa.rds")
  
  os_london  |>  colnames()
  
  os_london <- os_london[,.(identifier,length,startNode,endNode,geometry)]
  
  os_lines <- os_london[,.(startNode,endNode,geometry)][,st_coordinates(geometry)] |>
    as_tibble() |> 
    dplyr::group_by(L1) |>
    dplyr::reframe(start_x = X[1]
                   ,start_y = Y[1]
                   ,end_x = X[n()]
                   ,end_y = Y[n()]) |> 
    as.data.table()
  
  os_london <- cbind(os_london,os_lines)
  
  os_nodes <- rbind(os_london[,.(osmid = startNode
                                 ,x = start_x
                                 ,y = start_y)]
                    ,os_london[,.(osmid = endNode
                                  ,x = end_x
                                  ,y = end_y)])
  
  os_nodes <- os_nodes[!duplicated(osmid),]
  
  data.table::setnames(os_london,old = c('startNode','endNode'), new = c('from','to'))
  
  os_cppr_simple <- make_cppr_net(edges = os_london
                                  ,nodes = os_nodes)
  
  rlist::list.save(os_cppr_simple,"benchmarks/cppr_networks/os_all.rds")
  
}
