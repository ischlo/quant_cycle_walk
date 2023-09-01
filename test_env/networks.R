# networks

library(rlist)
library(cppRouting)

if(!file.exists('Benchmarks/cppr_networks/networks.rds')) {
  cat('Generating a network file.')
  graphnames <- c('osm_all','osm_cycle','osm_walk'
                  ,'dodgr_cycle','dodgr_walk','os_all')
  
  file_paths <- paste0('Benchmarks/cppr_networks/',graphnames,'.rds')
  networks <- lapply(file_paths,FUN = \(path) rlist::list.load(path))
  networks <- `names<-`(networks,graphnames)
  networks |> rlist::list.save('Benchmarks/cppr_networks/networks.rds')
  
} else cat('network files exist. Erase first to re generate.\n')

if(!file.exists('Benchmarks/cppr_networks/networks_ch.rds')) {
  cat('Generating a network_ch file.\n')
  networks_ch <- lapply(networks,FUN = \(graph) cppRouting::cpp_contract(Graph = graph)) 
  networks_ch |> rlist::list.save('Benchmarks/cppr_networks/networks_ch.rds')
} else cat('network_ch files exist. Erase first to re generate.\n')
