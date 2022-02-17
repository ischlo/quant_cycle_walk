

flows_general <- data.table::fread(file.path(data_folder,"wu03ew_msoa.csv"))

# the order of the operation in the following lines is very inefficient, REDO

flows_general <- merge(flows_general,england_centroids[,.(geo_code,geom)]
                       ,by.x = "Area of residence"
                       ,by.y = "geo_code")

flows_general <- merge(flows_general,england_centroids[,.(geo_code,geom)]
                       ,by.x = "Area of workplace"
                       ,by.y = "geo_code")

flows_general[,"distance"] <- flows[,sf::st_distance(geom.x,geom.y,by_element = TRUE)]

flows_general <- NULL

flows_general %>% dim()
flows_general %>% head()
flows_general %>% colnames()

flows_columns_interest <- c("Area of residence"
                            ,"Area of workplace"
                            ,"All categories: Method of travel to work"
                            ,"Bicycle"
                            ,"On foot")

population <- flows_general[,sum(`All categories: Method of travel to work`)]

population_active_travel <- flows_active[,sum(bike+foot)]

population_concerned <- population_active_travel/population

head(flows_graph)

# flows_active <- flows[,.(residence = `Area of residence`
#                       ,workplace = `Area of workplace`
#                       ,total = `All categories: Method of travel to work`
#                       ,bike = `Bicycle`
#                       ,foot = `On foot`
#                       ,geom_from = geom.x
#                       ,geom_to = geom.y
#                       ,distance = distance)]
#
# flows_active <- flows_active[bike != 0 | foot != 0] 
# 
# flows_active %>% dim()
# 
# flows <- flows_active
# 
# flows_active <- NULL

# turning metres into kilometres
flows[,"distance"] <- flows[,as.numeric(distance)/1000]


den_foot <- density(flows[,distance]
                    ,weights = flows[,foot/sum(foot)])

den_bike <- density(flows[,distance]
                     ,weights = flows[,bike/sum(bike)])

#### Plotting ####
pdf("walk_cycle_dist_distribution.pdf")

plot(den_bike,col = "darkred"
     ,xlab = "distance, Km"
     ,xlim = c(0,30)
     ,ylim = c(0,0.2)
     ,lwd =2
     #,asp = 1
     ,main = "Distribution of distances")
lines(den_foot
      ,col = "darkblue"
      ,lwd = 2
      )
# lines(dens_flows
#       ,lwd = 2
#       ,col = "dimgrey")
legend(x = 20
       ,y =0.06
       ,legend = c("cycling","walking")
       ,col = c("darkred","darkblue")
       ,lty = 1
       ,lwd = 2
       #,bg = "grey"
       )

dev.off()

london_msoa %>% st_as_sf() %>% qtm()

dens_flows <- density(flows[,distance]
                      ,weights = flows[,(bike+foot)/sum(bike+foot)])

plot(dens_flows
     ,main = "Distance distribution of trips length"
     ,xlim = c(0,100)
     ,xlab = "distance, Km"
     ,lwd = 2
     ,col = "darkblue")

#### Flows in central london, preparing the scritps for running on the RC #### 

central_centroid %>% dim()

flows[match(flows$residence,central_centroid$geo_code),]

pred <- flows %>% 
   st_as_sf(sf_column_name = "geom_from") %>%
   st_intersects(bb)

central_flows <- flows[which(!is.na(pred %>% 
   as.numeric())),]
   
central_flows <- central_flows %>% filter(workplace %in% central_centroid$geo_code)

central_flows %>% dim()

central_flows_matrix <- central_flows %>% 
   dcast(formula = residence ~ workplace
         ,value.var = "bike"
         ,fill = 0)

#?dcast

london_central_msoa_dist %>% dim()

central_flows_matrix[,2:159] %>% dim()

central_flows_melt <- central_flows_matrix %>% melt()

london_central_msoa_melt <- london_central_msoa_dist %>% melt()

flows_dens_weighted <- density(london_central_msoa_melt[,3]
                      ,weights = central_flows_melt[,3]/sum(central_flows_melt[,3]))

dens_unw <- density(london_central_msoa_melt[,3]
                    #,weights = central_flows_melt[,3]/sum(central_flows_melt[,3])
                    )
plot(flows_dens_weighted)

### Attempting to make travel distance and cost functions for all of london.
(flows_london[,.(residence,workplace)] %>% unique() %>% nrow())/(
(flows_london[,.(residence)] %>% unique() %>% nrow())*
(flows_london[,.(workplace)] %>% unique() %>% nrow()))
# the above computes the approx fraction of links of all 
# the possible that are present in the network
# it is around 0.1

# there seems to be 52463 unique pairs of connections. 
flows_london[,.(residence,workplace)] %>% unique() %>% nrow()

# centroid_msoa$id <- 1:nrow(centroid_msoa)
# 
# centroid_msoa %>% sf::st_write("data/centroid_msoa.GeoJson"
#                                ,delete_dsn = TRUE)

flows_london <- flows_london %>% merge(london_msoa[,.(geo_code,id)]
                       ,by.x = "residence"
                       ,by.y = "geo_code")

flows_london <- flows_london %>% merge(london_msoa[,.(geo_code,id)]
                                       ,by.x = "workplace"
                                       ,by.y = "geo_code")
setnames(flows_london
         ,old = c("id.x","id.y")
         ,new = c("from_id","to_id")
)


flows_london %>% dim()

london_msoa <- london_msoa %>% st_as_sf()

flows_london_matrix <- dcast(flows_london
                             ,formula = from_id ~ to_id
                             ,value.var = "bike"
                             ,fill = 0)

# flow_matrix_col <- flows_london_matrix %>% colnames() %>% as.numeric() %>% na.exclude()
flows_groups <- flows_london %>% group_by(to_id) %>% summarise(from = list(from_id))

flows_groups$to_id

#i <- 980

london_msoa %>% dim()

registerDoParallel(cores = 3)
distances <- foreach(i = 1:nrow(flows_groups)
                     ,.combine = c
                     ) %dopar% {
                        st_distance(london_msoa[flows_groups$from[[i]],]
                           ,london_msoa[flows_groups$to_id[i],]) 
                     }
stopImplicitCluster()

routing_distances


distances %>% length()
summary(distances)

bike_flows_density <- density(distances
                              ,weights = (flows_london$bike/sum(flows_london$bike)))

print(bike_flows_density)

plot(bike_flows_density
     ,xlab = "distance, m"
     ,ylab = "density"
     ,main = "distribution of bike trip distances in London"
     ,lwd = 2)
abline(v=15000
       ,col = "navyblue"
       ,lwd = 2)

?density

# the matrix of all euclidean distances between msoas. 
london_msoa_dist <- london_msoa %>% 
   st_as_sf() %>% st_distance()

london_msoa_dist %>% dim()

### routing : 
