# 

all_nodes_dt <- st_read("data/haringey_all_network.gpkg", layer = "nodes") %>% data.table()
all_edges_dt <- st_read("data/haringey_all_network.gpkg", layer = "edges") %>% data.table()

all_edges_dt[,c("from","to")] <- 
  all_edges_dt[, lapply(.SD, as.character), .SDcols = c("from","to")]

all_nodes_dt[,"osmid"] <- 
  all_nodes_dt[, lapply(.SD, as.character), .SDcols = c("osmid")]

# this was obtined with osmnx
st_layers("data/haringey_all_network.gpkg")

haringay_walk_nodes <- st_read("data/haringey_walk_network.gpkg", layer = "nodes")
haringay_walk_edges <- st_read("data/haringey_walk_network.gpkg", layer = "edges")


#### 

haringey_edges_dt <- data.table(haringay_walk_edges)
haringay_nodes_dt <- data.table(haringay_walk_nodes)

haringey_edges_dt[,c("from","to")] <- 
  haringey_edges_dt[, lapply(.SD, as.character), .SDcols = c("from","to")]

haringay_nodes_dt[,"osmid"] <- haringay_nodes_dt[, lapply(.SD, as.character), .SDcols = c("osmid")]

# sf Network ####

haringay_network <- sfnetworks::sfnetwork(nodes = haringay_nodes_dt
                                          ,edges = haringey_edges_dt
                                          ,node_key = "osmid"
                                          ,directed = FALSE
                                          ,edges_as_lines = TRUE
)

haringay_all_net <- sfnetworks::sfnetwork(nodes = all_nodes_dt
                                          ,edges = all_edges_dt
                                          ,node_key = "osmid"
                                          ,directed = FALSE
                                          ,edges_as_lines = TRUE
)

# takes a while 
# haringay_network %>% plot()

# activate nodes

nodes <- haringay_network %>%
  activate(nodes) %>%
  st_as_sf(crs = 4326)

nodes_all <- haringay_all_net %>%
  activate(nodes) %>%
  st_as_sf(crs = 4326)

# activate edges

edges <- haringay_network %>% activate(edges) %>%
  st_as_sf(crs = 4326)

edges_all <- haringay_all_net %>% activate(edges) %>%
  st_as_sf(crs = 4326)


# plotting ####

tm_shape(nodes) + tm_dots(col = "grey"
                          ,size = .2
) + 
  tm_shape(edges) + tm_lines(col = "black"
                             ,alpha = 0.5
  ) +
  tm_shape(nodes_all) + tm_dots(col = "green"
                                ,size = .05
  ) +
  tm_shape(edges_all) + tm_lines(col = "orange"
                                 ,alpha = 0.5
  )

