#### Libs ####
library(sf)
library(tidyverse)
library(data.table)
library(rlist)
library(units)


options(max.print = 50)

# install.packages('bikedata')
# devtools::install_github ("mpadge/bikedata")

##### 

london_msoa <- rlist::list.load('data/london_msoa.rds')

#### reading in the different files for the centroids

msoa_wcent_geom <- 
  st_read("data/msoa_wcent_geom/msoa_wcent_geom.shp")

london_centroid_geom <- 
  st_read("data/msoa_pop_weighted.geojson")

sample_node_within_geom <- 
  st_read("data/sample_node_within_geom/sample_node_within_geom.shp")

# centroids in the network
network_centroid <- sample_node_within_geom |> 
  dplyr::group_by(geo_code) |> 
  summarise(net_centr = geometry |> st_combine() |> st_centroid())

# creating the distance values for intra borough flows. 

typ_dist_intra_msoa <- sample_node_within_geom |> 
  dplyr::group_by(geo_code) |> 
  summarise(tip_dist = mean(st_distance(geometry)))

typ_dist_intra_msoa <- typ_dist_intra_msoa |> 
  mutate(tip_dist = tip_dist |> units::set_units(NULL))


london_msoa$intra_dist <- typ_dist_intra_msoa$tip_dist/1000 |> round(3)

london_msoa$net_centr <- sf::st_geometry(network_centroid)

london_msoa$pop_weight_geom <- sf::st_geometry(london_centroid_geom)

london_msoa$centr_geom <- london_msoa$geom |> st_centroid()

#### workplace zones centroids ####

workplace_zones <- sf::st_read('data/Workplace_Zones.json')

pred <- st_intersects(workplace_zones |> 
                        st_as_sf(crs = 4326)
                      ,london_msoa$geometry)

workplace_zones$msoa <- pred  |>  as.numeric()

workplace_centr <- workplace_zones |> 
  drop_na() |> 
  st_as_sf(crs = 4326) |> 
  st_transform(27700) |> 
  dplyr::group_by(msoa) |> 
  summarise(workplace_centr = st_centroid(st_combine(geometry))) |> 
  st_transform(4326)

london_msoa$workplace_centr <- workplace_centr$workplace_centr |> sf::st_geometry()

london_msoa[,c("geometry","centr_geom","pop_weight_geom","workplace_centr","net_centr")] <- 
  london_msoa[,c("geometry","centr_geom","pop_weight_geom","workplace_centr","net_centr")] |> 
  sapply(sf::st_as_text) |> as.data.frame()

london_msoa |> list.save("data/london_msoa.rds")

london_msoa |> readr::write_csv('data/london_msoa.csv')

