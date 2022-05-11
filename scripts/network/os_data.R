## OS data 

os_folder <- "data/oproad_essh_gb/data"


# from: http://lle.gov.wales/catalogue/item/MiddleLayerSuperOutputAreasMSOA/?lang=en
msoas_link <- "http://lle.gov.wales/catalogue/item/MiddleLayerSuperOutputAreasMsoasWales2011.json"


tr_road_node <- sf::st_read(file.path(os_folder,"TR_RoadNode.shp"))

hu_road_node <- sf::st_read(file.path(os_folder,"HU_RoadNode.shp"))

tr_road_node %>% qtm()

hu_road_node %>% qtm()


ta_road_link <- sf::st_read(file.path(os_folder,"TA_RoadLink.shp"))

ta_road_link %>% qtm()

highways <- sf::st_read(file.path("data"
                                  ,"highways-network-roads-gml"
                                  ,"data"
                                  ,"Highways_Roads_RoadLink_FULL_001.gml"))


highways %>% qtm()


msoas <- sf::st_read(msoas_link)