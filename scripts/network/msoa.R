#### MSOAs summary stats for england ####
# gb_bound_file <-  file.path(data_folder
#                             ,"bdline_gml3_gb"
#                             ,"INSPIRE_AdministrativeUnit.gml")
# 
# 
# # the gpkg file is huuuuuge. before loading, inspect layers 
# st_layers(file.path(data_folder,"bdline_gb.gpkg"))
# 
# os_bound <- sf::st_read(file.path(data_folder,"bdline_gb.gpkg"))

# postcode_la <- fread(file.path(data_folder,"postcodetola.csv"))
# 
# postcode_la %>% head()
# 
# postcode_la <- NULL

# gb_sectors <- sf::st_read(file.path(data_folder,"Distribution","Sectors.shp"))
# 
# gb_sectors$name

# regions <- pct_regions_lookup %>%
#   filter(region_name == "london") %>%
#   pull(lad16nm)
# 
# england_zones <- get_pct(purpose = "commute"
#                          ,geography = "msoa"
#                          ,layer = "z"
#                          ,national = TRUE
# )
# 
# england_zones <- england_zones %>% st_make_valid()
# 
# england_zones <-england_zones %>% as.data.table()
# 
# england_zones %>% colnames()
# 
# england_centroids <- england_zones[,.(geo_code,geo_name,geom = st_centroid(geometry))]

# get msoa bounndaries data from pct

london_zones <- get_pct(region = "london"
                        ,purpose = "commute"
                        ,geography = "msoa"
                        ,layer = "z"
                        #,national = TRUE
)

london_zones <- london_zones %>% st_make_valid() 

london_zones <- london_zones %>% data.table()

london_zones |> rlist::list.load('data/london_msoa.rds')

# london_zones[,"area"] <- london_zones[, lapply(.SD, st_area), .SDcols = c("geometry")]
# 
# london_zones[,"typ_dist"] <- london_zones[, .(typ_dist = sqrt(area))]
# 
# london_zones %>% colnames()

# # england
# typ_dist_hist <- london_zones[,typ_dist] %>% hist(breaks = 100
#                                                   ,main = "Typical distance distribution, MSOA level"
#                                                   ,xlab = "meters"
#                                                   ,ylab = "density"
#                                                   ,freq = FALSE)
# 
# tmap_mode("plot")
# 
# transport_use_map <- london_zones %>% st_as_sf() %>% tm_shape() + 
#   tm_polygons(col = c("bicycle","car_driver","train_tube","bus")
#               ,title = ""
#               ,n=7
#               ,palette = "viridis"
#               ,border.alpha = .1
#   ) + 
#   tm_layout(main.title = "Transport mode use"
#             ,title = "User number"
#             ,panel.show = TRUE
#             ,panel.labels = c("bicycle","car","train/tube","bus")
#             #,legend.frame = "black"
#             ,legend.show = TRUE
#             ,legend.outside = TRUE
#             ,bg.color = "white"
#             ,aes.palette = "viridis"
#             ,fontfamily = "Helvetica"
#   ) +
#   tm_facets(free.scales = FALSE
#             ,free.scales.fill = FALSE
#             ,as.layers =TRUE)
# 
# tmap_save(transport_use_map, "transport_use_map.pdf")