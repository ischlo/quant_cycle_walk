# function

or <- function(x,y) x | y

node_density <- function(nodes, areas) {
  nodes_intersection_predicate <- nodes %>% 
    st_as_sf() %>% 
    st_intersects(areas %>% st_as_sf())
  
  nodes[,"intersect_msoa"] <- nodes_intersection_predicate %>% as.numeric()
  
  # removes this variable cause it's big and useless now
  nodes_intersection_predicate <- NULL
  
  ans <- nodes[,.N,by = intersect_msoa]
  
  areas[ans$intersect_msoa,"nodes_cycle"] <- ans$N
  
  areas[,"area"] <- areas[,.(as.numeric(st_area(geometry)))]
  
  areas[,"nodes_cylce_dens"] <- areas[,nodes_cycle/area*1E6]
  
  areas#[,.(nodes_cycle,area,nodes_cylce_dens)]
  
}


node_density_map <- function(areas, bbox_areas) {
  cycle_node_density_map <- areas %>% 
    st_as_sf() %>% tm_shape(bbox = bbox_areas) + 
    tm_polygons(col = "nodes_cylce_dens"
                ,style = "fisher"
                ,n = 5
                ,palette = "viridis"
                ,title = "Density (nodes/km2)"
                ,legend.hist = TRUE
    ) +
    tm_layout(main.title = "Cycle nodes density"
              ,legend.outside = TRUE
              ,legend.hist.width = 1
              ,legend.hist.size = .5
              ,inner.margins= 0.1
              ,outer.margins = c(0,0,0,0)
              ,asp=NA
              ,scale=.8
              ,frame = TRUE
              ,fontfamily = "Helvetica"
    ) +
    tm_scale_bar(breaks = c(0,5,10)) 
  # +
  #   tmap_design_mode()
}


# London network summary
gc()

# source(msoa.R)

london_zones %>% colnames()
main_variables_for_zones <- c("geo_code","geo_name","lad11cd","lad_name","geometry")

london_msoa <- london_zones[,..main_variables_for_zones]

london_zones

nodes_intersection_predicate <- london_nodes_dt %>% 
  st_as_sf() %>% 
  st_intersects(london_msoa %>% st_as_sf())

nodes_intersection_predicate %>% as.numeric()

london_nodes_dt[,"intersect_msoa"] <- nodes_intersection_predicate %>% as.numeric()

# removes this variable cause it's big and useless now
nodes_intersection_predicate <- NULL

london_nodes_dt[is.na(intersect_msoa),] %>% st_as_sf() %>% qtm()

ans <- london_nodes_dt[,.N,by = intersect_msoa]

london_msoa[ans$intersect_msoa,"nodes_cycle"] <- ans$N

london_msoa[,"area"] <- london_msoa[,.(as.numeric(st_area(geometry)))]

london_msoa[,"nodes_cylce_dens"] <- london_msoa[,nodes_cycle/area*1E6]

london_cycle_node_density_hist <- london_msoa[,nodes_cylce_dens] %>% 
  hist(freq = FALSE
        ,main = "Distribution of nodes density, London"
        ,ylab = "Density of nodes"
        ,xlab = "Nodes per km2")

plot(london_cycle_node_density_hist)

london_cycle_node_density_map <- london_msoa %>% 
  st_as_sf() %>% tm_shape() + 
  tm_polygons(col = "nodes_cylce_dens"
              ,style = "fisher"
              ,n = 5
              ,palette = "viridis"
              ,title = "Density (nodes/km2)"
              ,legend.hist = TRUE
              ) +
  tm_layout(main.title = "Cycle nodes density"
            ,legend.outside = TRUE
            ,legend.hist.width = 1
            ,legend.hist.size = .5
            ,inner.margins= 0.1
            ,outer.margins = c(0,0,0,0)
            ,asp=NA
            ,scale=.8
            ,frame = TRUE
            ,fontfamily = "Helvetica"
            ) +
  tm_scale_bar(breaks = c(0,5,10)) +
  tmap_design_mode()

tmap_save(london_cycle_node_density_map,"images/london_cycle_node_density_map.pdf")

# edges density

edges_num <- london_edges_dt %>% nrow()

selected_boroughs <- c("Islington","Camden", "City of London")

london_selected_msoa <- london_msoa[lad_name %in% selected_boroughs,]

london_selected_msoa %>% class()

predicate <- london_edges_dt %>% 
  st_as_sf() %>%
  st_intersects(london_selected_msoa[,geometry] %>% st_as_sf()
                ,sparse = FALSE)

x <- predicate %>% 
  apply(FUN = function(x) reduce(x,or)
        ,MARGIN = 1)

london_selected_edges <- london_edges_dt %>% st_as_sf() %>% filter(x)

ans <- foreach(i = 1:dim(predicate)[2], .combine=rbind) %do% {
  dist <- london_edges_dt[predicate[,i],geom] %>% 
    st_length() %>% sum()
  cbind(london_selected_msoa[i,.(geo_code,geo_name,lad11cd,lad_name)],dist)
}

london_selected_msoa[,"dist"] <- as.numeric(ans$dist)

london_selected_msoa[,"typ_dist"] <- london_selected_msoa[,sqrt(as.numeric(st_area(geometry)))]

london_selected_msoa[,"edge_dens"] <- london_selected_msoa[,dist/typ_dist]

plot(london_selected_msoa[,edge_dens],london_selected_msoa[,nodes_cylce_dens])

ggplot(data = london_selected_msoa, mapping = aes(x=edge_dens,y = nodes_cylce_dens)) +
  geom_point()

london_edges_overlay_map <- 
  tm_shape(london_selected_edges %>% st_as_sf()) + 
  tm_lines(col = "black"
           ,lwd = 0.3
           ,alpha = 0.7) +
  tm_shape(london_selected_msoa %>% st_as_sf()) + 
  tm_polygons(col = "edge_dens"
              ,style = "fisher"
              ,breaks = 5
              ,palette = "Purples"
              ,title = "Density"
              ,alpha = 0.5
              ,border.alpha = 0.5
              ,border.col = "white"
  ) + 
  tm_layout(legend.outside = TRUE
            #,title = "Density"
            ,main.title = "Density of cycling roads, London"
            #,bg.color = "navyblue"
  ) + 
  tm_scale_bar(breaks = c(0,1,2),position = c("left","bottom"))

tmap_save(london_edges_overlay_map
          ,"images/london_edges_overlay_map.pdf")

##### brighton 

brighton_zones <- node_density(brighton_nodes_dt,brighton_zones)

# brighton_zones %>% nrow()

brighton_node_density_map <- brighton_zones %>% node_density_map(bbox_areas = brighton_bbox)

tmap_save(brighton_node_density_map,"images/brighton_node_density_map.pdf")

# edges density

test <- brighton_edges_dt %>% 
  st_as_sf() %>% 
  st_intersects(brighton_zones[,"geometry"] %>% st_as_sf(),sparse = FALSE)

# test %>% dim()

ans <- foreach(i = 1:dim(test)[2], .combine=rbind) %do% {
  dist <- brighton_edges_dt[test[,i],geom] %>% 
    st_length() %>% sum()
  cbind(brighton_zones[i,.(geo_code,geo_name,lad11cd,lad_name)],dist)
}

brighton_zones[,"dist"] <- as.numeric(ans[,dist])

brighton_zones[,"typ_dist"] <- brighton_zones[,sqrt(as.numeric(st_area(geometry)))]

brighton_zones[,"edge_dens"] <- brighton_zones[,dist/typ_dist]

plot(brighton_zones[,.(edge_dens,nodes_cylce_dens)])

brighton_edges_density_map <- brighton_zones %>% st_as_sf() %>% tm_shape(bbox = brighton_bbox) +
  tm_polygons(col = "edge_dens"
              ,style = "fisher"
              ,breaks = 5
              ,palette = "Purples"
              ,title = "Density"
              ) + 
  tm_layout(legend.outside = TRUE
            #,title = "Density"
            ,main.title = "Density of cycling roads, Brighton"
            #,bg.color = "navyblue"
            ) + 
  tm_scale_bar(breaks = c(0,5,10))

tmap_save(brighton_edges_density_map,paste0(image_folder,"brighton_edges_dens_map",".pdf"))

brighton_edges_overlay_map <- tm_shape(brighton_edges_dt %>% st_as_sf()) + 
  tm_lines(col = "black"
          ,lwd = 0.3
          ,alpha = 0.7) +
  tm_shape(brighton_zones %>% st_as_sf()) + 
  tm_polygons(col = "edge_dens"
              ,style = "fisher"
              ,breaks = 5
              ,palette = "Purples"
              ,title = "Density"
              ,alpha = 0.5
              ,border.alpha = 0.5
              ,border.col = "white"
              ) + 
  tm_layout(legend.outside = TRUE
            #,title = "Density"
            ,main.title = "Density of cycling roads, Brighton"
            #,bg.color = "navyblue"
  ) + 
  tm_scale_bar(breaks = c(0,5,10))

tmap_save(brighton_edges_overlay_map,"images/brighton_edges_overlay_map.pdf")

