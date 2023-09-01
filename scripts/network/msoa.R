require(pct)
require(sf)
require(data.table)
require(rlist)


if(!file.exists('data/london_msoa.rds')){

# get msoa bounndaries data from pct

london_zones <- pct::get_pct(region = "london"
                        ,purpose = "commute"
                        ,geography = "msoa"
                        ,layer = "z"
                        #,national = TRUE
)

london_zones <- london_zones |> sf::st_make_valid() 

london_zones <- london_zones |> data.table()

london_zones <- london_zones[,.(geo_code,geo_name,geometry = sf::st_geometry(geometry))]

london_zones |> rlist::list.save('data/london_msoa.rds')

} else cat('london_msoa file exists, erase before recreating.')

# london_zones[,"area"] <- london_zones[, lapply(.SD, st_area), .SDcols = c("geometry")]
# 
# there seem to be bugs down the way when calculating areas, mayber do it now:
# london_zones[,"typ_dist"] <- london_zones[, .(typ_dist = sqrt(area))]
# 
# london_zones |> colnames()

# # england
# typ_dist_hist <- london_zones[,typ_dist] |> hist(breaks = 100
#                                                   ,main = "Typical distance distribution, MSOA level"
#                                                   ,xlab = "meters"
#                                                   ,ylab = "density"
#                                                   ,freq = FALSE)
# 
# tmap_mode("plot")
# 
# transport_use_map <- london_zones |> st_as_sf() |> tm_shape() + 
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
