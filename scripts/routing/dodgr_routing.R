library(data.table)
library(dodgr)
library(RcppParallel)
library(cppRouting)


#### London network and flows aggregating
london_network <- dodgr::weight_streetnet(london_edges_dt %>% st_as_sf()
                                          ,wt_profile = "bicycle")

flows_london <- flows[ (residence %in% london_msoa$geo_code &
                          workplace %in% london_msoa$geo_code) & bike != 0,]


flows_graph_london <- dodgr_flows_aggregate(london_network
                                            ,from = flows_london$geom_from[1:3000] %>% 
                                              st_coordinates()
                                            ,to = flows_london$geom_to[1:3000] %>% 
                                              st_coordinates()
                                            ,flows = flows_london$bike[1:3000])

dodgr_flowmap(flows_graph_london, linescale = 10)

dodgr_flowmap(flows_graph)

#### Constructing the network and working with the flows in Brighton ####
# dodgr package
## Reading files for brithgon in the ped_network.R file.

brighton_network <- dodgr::weight_streetnet(brighton_edges_dt %>% st_as_sf()
                                            ,wt_profile = "bicycle")

#  selecting the biggest connected component
brighton_network <- brighton_network[brighton_network$component == 1, ]

brighton_centroid <- brighton_zones %>% 
  st_as_sf() %>% 
  st_centroid() %>% 
  st_coordinates()

brighton_euclid_dist <- brighton_zones %>% 
  st_as_sf() %>% 
  st_centroid() %>% 
  st_distance() %>%
  set_units(NULL) # units are in meters everywhere.

# set the max number of cores to use 
# in the dodgr function

RcppParallel::setThreadOptions(numThreads = 3)

brighton_network %>% head()

brighton_dists <- dodgr_dists(brighton_network
                              ,from = brighton_centroid
                              ,to = brighton_centroid
                              ,shortest = FALSE
                              ,quiet = FALSE
                              ,parallel = TRUE
)

brighton_dists %>% dim

which(is.na(brighton_dists), arr.ind = TRUE) %>% unique()

# the zones that have NA values for distances
brighton_zones %>% 
  st_as_sf() %>% 
  qtm(bbox = brighton_bbox) + 
  qtm(brighton_zones[c(16,36,44,47,63,112,130,98,99,63,44,47),] %>% 
        st_as_sf()
      ,fill = "red")

# not necessary
# colnames(brighton_dists) <- brighton_zones$geo_code
# rownames(brighton_dists) <- brighton_zones$geo_code
(brighton_dists/brighton_euclid_dist) %>% density(na.rm = TRUE) %>% plot(xlim = c(0,4))
abline(v = sqrt(2))

# the fraction of the computed trips that were more than 3 times greater
# than the Euclidean distance between the MSOA
# centroids is roughly 0.004, shall we consider this as negligible ?
length(which((brighton_dists/brighton_euclid_dist) > 3))/length(brighton_dists)

# these next lines will get the corner cases of path that are very big compared to 
# the euclidean distance between the points and plot them to check for consistency. 
corner_cases_brighton <- which((brighton_dists/brighton_euclid_dist) > 3,arr.ind = TRUE)
case <- corner_cases_brighton[sample(1:nrow(corner_cases_brighton),10),]

case_from <- brighton_centroid[case[,1],]
case_to <- brighton_centroid[case[,2],]

corner_paths <- dodgr::dodgr_paths(brighton_network
                                   ,from = case_from
                                   ,to = case_to
                                   ,vertices = FALSE
                                   ,pairwise = TRUE)

j <- sample(length(corner_paths)
            ,1)

# from <- brighton_network[corner_paths[[j]][[1]],c("from_lon","from_lat")]
# to <- brighton_network[corner_paths[[j]][[1]],c("to_lon","to_lat")] 

test <- get_lines(from = from
                  ,to = to)
#tmap_mode("view")
test %>% qtm(fill = "orange")

# from_ind <- which((brighton_dists-brighton_euclid_dist) > 20000,arr.ind = TRUE)[,1]
# to_ind <- which((brighton_dists-brighton_euclid_dist) > 20000,arr.ind = TRUE)[,2]
#  
# from <-  brighton_zones %>% st_as_sf() %>% st_centroid()
# to <-  brighton_zones %>% st_as_sf() %>% st_centroid()

flows_brighton <- flows[residence %in% brighton_zones$geo_code & 
                          workplace %in% brighton_zones$geo_code,]


# flows_brighton[residence == workplace,bike]

flows_graph <- dodgr_flows_aggregate(brighton_network
                                     ,from = flows_brighton$geom_from %>% st_coordinates()
                                     ,to = flows_brighton$geom_to %>% st_coordinates()
                                     ,flows = flows_brighton$bike)

flows_graph$flow <- log(1+flows_graph$flow)

flows_brighton

dodgr_flowmap(flows_graph, linescale = 30)

#### making the flow matrix for Brighton

# there is probably a more efficient way to do this, but it works... 
registerDoParallel(cores = 3)
brighton_flows_matrix <- foreach(i = 1:length(brighton_zones$geo_code)
                                 ,.combine = rbind) %dopar% {
                                   x <- rep_along(brighton_zones$geo_code,0)
                                   for (j in 1:length(brighton_zones$geo_code)) {
                                     
                                     n <- flows_brighton[residence == brighton_zones$geo_code[i] & 
                                                           workplace == brighton_zones$geo_code[j],bike]
                                     if (length(n)!=0){
                                       x[j] <- n
                                     }
                                   }
                                   x
                                 }
stopImplicitCluster()

# saved to send to Valentina
# list.save(brighton_zones$geo_code, "brighton_geocode.rds")

# attempt to make the flows matrix with dcast and melt was failed. 
# this is the more efficient way to do it. 
flows_brighton %>% acast(formula = residence ~ workplace
                         ,value.var = "bike"
                         ,fill = 0)

# plot(brighton_euclid_dist
#      ,brighton_flows_matrix
#      ,log = "xy"
#      )

isSymmetric.matrix(brighton_euclid_dist)
isSymmetric.matrix(brighton_flows_matrix)

# this plot helps visualize the deviation between the Euclidean distance and 
# the shortest path between all the pairs of MSOAs centroids.

jpeg("images/light/brighton_dists.jpg"
     ,height = 5.83
     ,width = 8.27
     ,quality = 80
     ,units = "in"
     ,res = 150)
plot(brighton_euclid_dist,brighton_dists
     #,log = "xy"
     ,col = "navyblue"
     # ,asp = 1
     ,pch = 20
     ,xlab = "crow-fly distance, m"
     ,ylab = "network distance, m"
     ,main = "network and crowfly distance comparison, Brighton and region"
     ,cex = 0.7
     # ,cex.lab = 1.2
)
lines(1:max(brighton_euclid_dist)
      ,col = "darkred"
      ,lwd = 2)
legend(x = 60000
       ,y = 2e4
       ,legend = c("y=x")
       ,cex = 1.3
       ,col = c("darkred")
       ,lwd = 2)
dev.off()
