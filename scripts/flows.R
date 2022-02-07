

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


dens_flows <- density(flows[,distance]
                      ,weights = flows[,(bike+foot)/sum(bike+foot)])

plot(dens_flows
     ,main = "Distance distribution of trips length"
     ,xlim = c(0,100)
     ,xlab = "distance, Km"
     ,lwd = 2
     ,col = "darkblue")
