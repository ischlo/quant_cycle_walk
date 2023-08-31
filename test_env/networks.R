# networks

graphnames <- c('osm_all','osm_bike','osm_walk'
  ,'dodgr_cycle','dodgr_walk','os_graph')

file_paths <- paste0('Benchmarks/cppr_networks/',graphnames,'.rds')

networks <- lapply(file_paths,FUN = \(path) rlist::list.load(path))

networks <- `names<-`(networks,graphnames)
