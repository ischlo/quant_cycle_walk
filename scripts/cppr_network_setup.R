
#### cppR network setup ####

# THE LONG WAY

# The MSOAs at until now were only those that have a santander cycle station in them.
# creating the cppRouting graph from london edges:
# reusing old variable names to save memory. rerun previous code if necessary. 

london_edges <- london_edges_dt[,st_coordinates(geom)] %>% as.data.table()

# london_edges <- london_edges[,.SD[c(1,.N)],by = L1]

## Parenthesis with performance test between data.table and tidyverse:
# london_edges_tidy <- london_edges %>% as_tibble()
# 
# benchmark(data.tab = london_edges[1:1000
#                                   ,.(X1 = .SD[1L,X]
#                                      ,Y1 = .SD[1L,Y]
#                                      ,X2 = .SD[.N,X]
#                                      ,Y2 = .SD[.N,Y])
#                                   ,by = L1]
#           ,tidy = london_edges_tidy[1:1000,] %>%
#             group_by(L1) %>%
#             summarise(res = cbind(X[1],Y[1],X[n()],Y[n()]))
#           ,replications = 5)
# parenthesis closed, tidy seems mode than 10 times faster...

# london_edges <- london_edges[,.(X1 = .SD[1L,X]
#                                 ,Y1 = .SD[1L,Y]
#                                 ,X2 = .SD[.N,X]
#                                 ,Y2 = .SD[.N,Y])
#                              ,by = L1]

london_edges <- london_edges %>% 
  as_tibble() %>%
  group_by(L1) %>%
  summarise(res = cbind(X[1],Y[1],X[n()],Y[n()])) 

# # set node ids manually to simplify usage. 
# # not sure if it makes sense to create nodes separately 
# node_ids <- c(london_edges_dt[,to],london_edges_dt[,from]) %>% unique()
# node_ids <- data.table(id = node_ids
#                 ,num_id = 1:length(node_ids))

# keep only the relevant column to construct the graph with cppRouting
london_edges[,.("from","to","length")] <- 
  london_edges_dt[,.("from","to","length")] %>%
  as.data.table()

london_edges <- london_edges %>% 
  as.data.table() %>%
  merge.data.table(node_ids
                   ,by.x = "from"
                   ,by.y = "id")

setnames(london_edges
         ,old = "num_id"
         ,new = "from_num")

london_edges <- london_edges %>% merge.data.table(node_ids
                                                  ,by.x = "to"
                                                  ,by.y = "id")

setnames(london_edges
         ,old = "num_id"
         ,new = "to_num")

# if needed to reassign the class
# london_edges <- london_edges %>% as.data.table()
# save the file locally
# london_edges %>% fwrite(file = "london_edges.csv")

# read in the file with edges of the london graph
# london_edges <- fread("data/london_edges.csv")

# getting the nodes from the edges with custon ids. 
london_nodes <- 
  rbind(london_edges[,.(node_ID = from_num,X = res.V1,Y = res.V2)]
        ,london_edges[,.(node_ID = to_num,X = res.V3,Y = res.V4)]) %>% 
  unique()

london_graph <- london_edges[,.(from = from_num,to = to_num,length)]

london_graph <- london_graph %>% makegraph(directed = FALSE
                                           ,coords = london_nodes)

#### THE SHORT WAY ####

# using the data tables imported from osmnx. 
london_graph <- 
  london_edges_dt[,.(from,to,length)] %>% 
  makegraph(directed = FALSE
            ,coords = london_nodes_dt[,.(osmid,x,y)])


# simplifying the graph to test
london_graph_simple <- 
  london_graph %>%
  cppRouting::cpp_simplify(rm_loop = TRUE
                           ,iterate = FALSE)


london_graph <- london_edges_dt[grep("motorway",highway,invert = TRUE),] %>% 
  make_cppr_net(nodes = london_nodes_dt)

# saving the graph
london_graph_simple %>% list.save("data/london_graph_simple.rds")

##### FUNCTION ####

make_cppr_lines <- function(paths, graph, crs = 4326) {
  paths %>% lapply(function(x) {graph$coords[match(x,node_id),.(X,Y)] %>% # get the coordinates of the lines 
      get_lines(to = NULL, crs = crs) # make the lines
  }) %>%
    st_sfc(crs = crs) %>%
    st_as_sf()
}

#
make_cppr_net <- function(edges,nodes, simple = TRUE) {
  # Provide a edges and nodes from osmnx exported graph.
  # crs 4326 is expected
  # it's good if the data has also the variables from and to 
  # designating nodes that are connected, but not essential
  edges <- edges %>% 
    as.data.table()
  nodes <- nodes %>% 
    as.data.table()
  # edges <- edges[,.(length = set_units(st_length(geom),NULL))]
  # edges_simp <- edges %>% 
  #   st_as_sf() %>%
  #   st_coordinates() %>% 
  #   as_tibble() %>%
  #   group_by(L1) %>% 
  #   summarise(res = cbind(X[1],Y[1],X[n()],Y[n()]))
  # 
  # edges_simp <- edges_simp[,.(length = edges[,length])] 
  # 
  # # only the endpoints of lines are accepted as nodes,
  # # so the following line extracts them from the set of edges. 
  # nodes <- NULL
  # if(c("from","to") %in% colnames(edges)){
  #   nodes <- rbind(edges[,.(node_ID = from,x = res.V1,y = res.V2)]
  #                  ,edges[,.(node_ID = to,x = res.V3,y = res.V4)]) %>% 
  #     unique() # store each node only once. 
  # } 
  # # write what else if there are no from to node ids else  {}
  # {
  #   nodes <- rbind(edges[,.(x = res.V1,y = res.V2)]
  #                  ,edges[,.(x = res.V3,y = res.V4)]) %>% 
  #     unique() # store each node only once. 
  #   nodes[,"id"] <- seq_along(nodes$x)
  #   # assign from to node to all the vertices. 
  # }
  # 
  graph <- edges[,.(from,to,length)]
  # we don't have enough info on the edges to make a directed graph, so the assumption 
  # is taken that you can cycle in both direction on any edge
  graph <- graph %>% makegraph(directed = FALSE
                               ,coords = nodes[,.(osmid,x,y)])
  if(simple == TRUE) {
    # simplifying the graph 
    graph <- graph %>%
      cppRouting::cpp_simplify(rm_loop = TRUE
                               ,iterate = FALSE) # iterating may cause changes that we don't want
  }
  
  graph
}


