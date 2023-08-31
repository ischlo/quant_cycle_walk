
#### cppR network setup ####

##### FUNCTION ####

make_cppr_lines <- function(paths, graph, crs = 4326) {
  paths |> lapply(function(x) {graph$coords[match(x,node_id),.(X,Y)] |> # get the coordinates of the lines 
      get_lines(to = NULL, crs = crs) # make the lines
  }) |>
    st_sfc(crs = crs) |>
    st_as_sf()
}

#### Very useful function to find the nearest node in the network to any geographic coordinate.
# Useful for all the routing tasks
find_nearest_node_on_graph <- function(graph, points_data = NULL,return_id = TRUE,...) {
  if(is.null(points_data)) stop("Provide sf spatial points to link to the network.")
  if(nrow(points_data)>1000) cat('Points data set is big, expect poor performance...')
  
  points_sf <- sf::st_as_sf(points_data,...)
  
  if(inherits(graph, c('data.table','data.frame'))){
    node_ind <- sf::st_nearest_feature(points_sf, graph |> sf::st_as_sf(coords=c('x','y'),crs = 4326))
    if(return_id) return(graph$osmid[node_ind])
    else return(node_ind)
    
  } else if (inherits(graph,'list')){
    node_ind <- sf::st_nearest_feature(points_sf, graph$coords |> sf::st_as_sf(coords=c('x','y'),crs = 4326))
    if(return_id) return(graph$coords$osmid[node_ind])
    else return(node_ind)
  }
  
}



#### extract the largest connected component from a set of nodes.

######
get_lcc <- function(ways, graph_mode = "weak") {
  
  # require("igraph")
  
  stopifnot("data.table" %in% class(ways), "from" %in% colnames(ways), "to" %in% colnames(ways))
  
  igraph_ways <- igraph::graph_from_data_frame(ways[,.(from,to)],directed = FALSE)
  
  if(igraph_ways |> igraph::is_connected(mode = graph_mode)) { 
    cat('Graph is connected') 
    return(ways) 
    }
  
  nodes_comp <- igraph::components(igraph_ways,mode = graph_mode)
  
  vert_ids <- igraph::V(igraph_ways)[nodes_comp$membership == which.max(nodes_comp$csize)]$name
  
  return(ways[from %in% vert_ids & to %in% vert_ids,])
}

#
make_cppr_net <- function(edges,nodes = NULL, simple = TRUE, directed = FALSE) {
  # Provide a edges and nodes from osmnx exported graph.
  # crs 4326 is expected
  # it's good if the data has also the variables from and to 
  # designating nodes that are connected, but not essential
  edges <- edges |> 
    as.data.table()
  try({nodes <- nodes |> 
    as.data.table()})
  
  edges <- get_lcc(edges)
  
  graph <- edges[,.(from,to,length)]
  # we don't have enough info on the edges to make a directed graph, so the assumption 
  # is taken that you can cycle in both direction on any edge
  graph <- graph |> cppRouting::makegraph(directed = directed
                               ,coords = nodes[,.(osmid,x,y)])
  if(simple == TRUE) {
    # simplifying the graph 
    graph <- graph |>
      cppRouting::cpp_simplify(rm_loop = TRUE
                               ,iterate = FALSE) # iterating may cause changes that we don't want
  }
  
  graph
}

#### Functions for manipulating cppr graphs:

# adding edges: 
# right now, the distance is provided manually, but could be computed from euclid distance automatically.
add_edge_cppr <-  function(graph
                           ,from 
                           ,to 
                           ,l) {
  # check if it does not already exist
  
  # find the codes of the node respectively
  from_id <- graph$dict[match(from,graph$dict$ref),"id"]
  to_id <- graph$dict[match(to,graph$dict$ref),"id"]
  
  stopifnot(is.numeric(from_id)
            ,is.numeric(to_id))
  
  graph$data <- rbind.data.frame(graph$data
                                 ,data.frame("from" = c(from_id,to_id)
                                             ,"to" = c(to_id,from_id)
                                             ,"dist"=c(l,l))) |> as.data.table()
  
  graph
  
}

plot_cppr_path <- function(graph, from_id, to_id){
  require(tmap)
  p <- get_path_pair(graph
                     ,from = from_id
                     ,to = to_id
  )
  
  graph$coords[match(p[[1]],osmid),.(x,y)] |> get_lines() |> st_sfc(crs = 4326) |> qtm()
}  

graph_debug_function <- function(graph,node,lines,buffer = 500){
  require(tmap)
  
  buf <- graph$coords[node,] |> 
    st_as_sf(crs = 4326,coords = c(2,3)) |> 
    st_buffer(dist = 500)
  
  pred <- buf |> st_intersects(graph$coords |> 
                                  st_as_sf(crs = 4326,coords = c(2,3)))
  
  pred_lines <- buf |> st_intersects(lines |> st_as_sf())
  
  qtm(lines[pred_lines[[1]],] |> st_as_sf()) +
    qtm(graph$coords[pred[[1]],] |> 
          st_as_sf(crs = 4326,coords = c(2,3))) +
    qtm(graph$coords[node,] |> st_as_sf(crs = 4326,coords = c(2,3))
        ,dots.col = "red")
}


network <- list('find_nearest_node_on_graph'=find_nearest_node_on_graph
                ,'get_lcc'=get_lcc)



