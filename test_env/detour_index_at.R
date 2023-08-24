#  detour specifically for active travel with the plot of the mean detour in distance bands with standard deviation. 

library(rlist)
library(data.table)
library(sf)
library(Btoolkit)
library(cppSim)
library(foreach)
library(weights)
library(plotly)

## 
##### Reading in the data

# crow fly distances
london_msoa <- list.load("../data/london_msoa.rds") %>% 
  factor_to_character() %>% 
  as.data.table()

# reading in flows

flows <- list.load("../data/flows_london.rds") |> as.data.table()

## for active transport
flows_mat_at <- foreach(i = london_msoa[,id]
                     ,.combine = rbind
                     ,.final = as.matrix) %do%
  {
    x <- rep_len(0,nrow(london_msoa))
    d <- flows[from_id == i,.(bike,foot,to_id)] # flows
    x[d[,to_id]] <- d[,bike+foot]
    x
  }

flows_mat_at <- `mode<-`(flows_mat_at,"integer")

# Calculating the crow fly distances -----

norm2_geom <- london_msoa[,"centr_geom"] %>% 
  st_as_sf(wkt =1, crs = 4326) %>% 
  st_distance(by_element = FALSE) |>
  units::set_units(NULL)

norm2_commute <- london_msoa[,"pop_weight_geom"] %>% 
  st_as_sf(wkt =1, crs = 4326) %>% 
  st_distance(london_msoa[,"workplace_centr"] %>% 
                st_as_sf(wkt =1, crs = 4326)
              ,by_element = FALSE) |>
  units::set_units(NULL)

norm2_network <- london_msoa[,"net_centr"] %>% 
  st_as_sf(wkt =1, crs = 4326) %>% 
  st_distance(by_element = FALSE) |>
  units::set_units(NULL)

norm2 <- list(
  "geom" = norm2_geom
  ,"commute" = norm2_commute
  ,"network" = norm2_network
)

lapply(norm2, FUN = function(m) summary(as.numeric(m)))

# distance matrices at ----

#osm
osm_dist_geom <- list.load("../benchmarks/osm_dist_matrix_standard.rds")

osm_dist_commute <- list.load("../benchmarks/osm_dist_matrix_commute.rds")

osm_dist_network <- list.load("../benchmarks/osm_dist_matrix_network.rds")

# os
os_dist_geom <- list.load("../benchmarks/os_dist_matrix_geom.rds")

os_dist_commute <- list.load("../benchmarks/os_dist_matrix_commute.rds")

os_dist_network <- list.load("../benchmarks/os_dist_matrix_network.rds")


dist_matrices_at <- list("osm_dist_geom"=osm_dist_geom
                      ,"osm_dist_commute"=osm_dist_commute
                      ,"osm_dist_network"=osm_dist_network
                      ,"os_dist_geom"=os_dist_geom
                      ,"os_dist_commute"=os_dist_commute
                      ,"os_dist_network"=os_dist_network
)


# detour active travel ----

detour_index_at <- mapply(dist_matrices_at
                       ,norm2
                       ,SIMPLIFY = FALSE
                       ,FUN = function(d,n) {
                         n <- `diag<-`(n,diag(d))
                         as.numeric(d/n)
                       })

lapply(detour_index_at,FUN = function(m) summary(as.numeric(m)))

lapply(detour_index_at, FUN = function(m) m[which(m != 1)] |> summary())

### detour vs distance plot

# have a function that takes a detour matrix, and bands, 
# and returns the mean and sd values for the circuity in the provided intervals.
bin <-  function(detour,dist, x, y) {
  
  m = mean(detour[dist>x & dist <=y])
  sd = sd(detour[dist>x & dist <=y])
  n = length(detour[dist>x & dist <=y])
  return(c(m,sd,(x+y)/2,n))
}  

bins <-  10
d_max <- 50000
intervals <- 0:bins*(d_max/bins)
res <- NULL

for (i in 1:bins) {
  res <- rbind(res,bin(detour_index_at$osm_dist_commute,norm2$commute,intervals[i],intervals[i+1]))
}

# res=res[-1,]
res[,3] <- res[,3]/1000
# 

{
  # jpeg(filename = 'detour_bands_2.jpeg'
  #      ,height = 5.8
  #      ,width = 5.8)
  par(cex.lab=1.3
      ,mar = c(5,5,4,1))

  plot(res[,3]
       ,res[,1]
       ,xlab = 'Euclidean distance (Km)'
       ,ylab = expression('Mean detour,'~delta)
       ,type = 'b'
       ,pch = 19
       ,ylim = c(1,1.45)
       ,xlim = c(0,51)
       ,main = 'Mean detour by distance band'
       ,lwd = 2
  )
  
  segments(res[,3],res[,1]-res[,2]
           ,res[,3],res[,1]+res[,2]
           ,lwd = 1.5)
  
  # little bars at the ends
  eps <- 0.5
  segments(res[,3]-eps,res[,1]-res[,2]
           ,res[,3]+eps,res[,1]-res[,2]
           ,lwd = 1)
  segments(res[,3]-eps,res[,1]+res[,2]
           ,res[,3]+eps,res[,1]+res[,2]
           ,lwd = 1)
  # dev.off()
}

# results ----
# 
# osm_geom_result_at <- list.load("osm_dist_geom_at/osm_dist_geom_at_best_fit.rds")
# 
# osm_commute_result_at <- list.load("osm_dist_commute_at/osm_dist_commute_at_best_fit.rds")
# 
# osm_network_result_at <- list.load("osm_dist_network_at/osm_dist_network_at_best_fit.rds")
# 
# os_geom_result_at <- list.load("os_dist_geom_at/os_dist_geom_at_best_fit.rds")
# 
# os_commute_result_at <- list.load("os_dist_commute_at/os_dist_commute_at_best_fit.rds")
# 
# os_network_result_at <- list.load("os_dist_network_at/os_dist_network_at_best_fit.rds")
# 
# results_at <- list("osm_geom" = osm_geom_result_at
#                    ,"osm_commute" = osm_commute_result_at
#                    ,"osm_net" = osm_network_result_at
#                    ,"os_geom" = os_geom_result_at
#                    ,"os_commute" = os_commute_result_at
#                    ,"os_net" = os_network_result_at
# )

# SIM ----
#  gravity models with cppSim
# sim_at <- lapply(dist_matrices_at, FUN = function(d) {cppSim::simulation(flows_matrix = flows_mat_at
#                                                                    ,dist_matrix = d/1000) })
# 
# # correlation of cppSim results
# lapply(sim_at, FUN = function(d) {cor(d[[1]] |> as.numeric()
#                                    ,flows_mat_at |> as.numeric())^2})

#####
#  comparing the cppSim results with the old code results
# 
# cbind(lapply(results_at, FUN = function(res) res$beta_calib[which.max(res$beta_calib[,2]),c(2)])
#       ,lapply(sim_at, function(l) {l[[2]]}))
# 
# lapply(detour_index_at, FUN = function(m) m[which(m != 1)] |> mean())


# observed detour weighted by flows ----

detour_commute <-  (dist_matrices_at$osm_dist_commute/norm2_commute)

detour_commute_os <- (dist_matrices_at$os_dist_commute/norm2_commute)

# detour_commute |> as.numeric() |> hist()
# which(detour_commute<1,arr.ind = TRUE)
# which(detour_commute>5,arr.ind = TRUE)
# diagonal elements are problematic. 

detour_commute <- `diag<-`(detour_commute,0)
detour_commute_os <- `diag<-`(detour_commute_os,0)

# weights <- (flows_mat_at[detour_commute != 0]/sum(flows_mat_at[detour_commute != 0]))

detour_dt <- data.table('detour' = as.numeric(detour_commute[detour_commute != 0])
                        ,'flow' = as.numeric(flows_mat_at[detour_commute != 0])
                        ,'crow_fly' = as.numeric(norm2_commute[detour_commute != 0]))

detour_dt_os <- data.table('detour' = as.numeric(detour_commute_os[detour_commute_os != 0])
                           ,'flow' = as.numeric(flows_mat_at[detour_commute_os != 0])
                           ,'crow_fly' = as.numeric(norm2_commute[detour_commute_os != 0]))


detour_dt[crow_fly > 15000 & flow !=0]

# rounding values
detour_dt[,`:=`(detour=round(detour,3)
                ,crow_fly=round(crow_fly,0))]

# network distance
detour_dt[,net:=round(crow_fly*detour,-2)]

# creating bins with the round function.
detour_dt[,`:=`(detour_bin=round(detour,1)
                ,crow_fly_bin=round(crow_fly,-2))]

# grouping by bins
detours_binned <- detour_dt[,.(flow=sum(flow)
                               ,N=.N),by = .(net,crow_fly_bin)]

fig <- plot_ly(detours_binned
               ,x = ~crow_fly_bin
               , y = ~net
               ,z = ~log1p(flow), type = "heatmap") |> 
  add_lines(x=c(0,50000)
            ,y=c(0,50000)
            ,line = list(color = "red",size = 5),inherit = FALSE) |> 
  layout(xaxis = list(type = "log")
         ,yaxis = list(type = "log")
  )

fig

# detour_dt[,plot(sum(flow)),keyby=crow_fly]

# 
# plot(detour_dt[flow!=0,flow]
#      ,detour_dt[flow!=0,detour]
#      ,col = detour_dt[flow!=0,crow_fly]
#      ,log = 'x')
# 
# ggplot(detour_dt[crow_fly <= 15000 & flow!=0], aes(x = flow, y = crow_fly, colour = detour)) +
#   geom_point() + coord_trans(y="log2",x="log2")


######
# Detour vs quality of fit ----
detour_qfit_at <- mapply(sim_at
                         ,detour_index_at
                         # ,SIMPLIFY = FALSE
                         ,FUN = function(res,m) {c("r_2" = cor(res[[1]] |> as.numeric()
                                                               ,flows_mat_at |> as.numeric())^2
                                                   ,"detour" = mean(m[which(m != 1)]))}) |> as.matrix() |> t()

detour_qfit_at

# Plots ----
# plots
pchs <- c(1,15,24)
cols <- c("darkred","darkred","darkred","navyblue","navyblue","navyblue")

# tutorial on how to annotate a la latex in R plots
# https://data.library.virginia.edu/mathematical-annotation-in-r/
{
  par(mar = c(5,5,4,1))
  plot(detour_qfit_at
       ,main = "Detour, quality of fit"
       ,pch = pchs
       ,col = cols
       ,xlab = expression(r^2)
       ,ylab = expression(delta)
       ,cex.lab = 1.4
       ,ylim = c(1.155,1.3)
       ,cex.axis = 1.2
       ,cex = 1.4
  )
  legend(x = .859
         ,y = 1.285
         ,title = "Centroid"
         ,pch = pchs
         ,legend = c("geom","commute","network")
         ,bty = "n"
  )
  legend(x = .859
         ,y = 1.24
         ,title = "Network"
         ,legend = c("OSM","OS")
         ,fill = unique(cols)
         ,col = unique(cols)
         ,bty = "n"
  )
  # legend(y = 1.21
  #        ,x = 0.859
  #        ,legend = "mean detour"
  #        ,lty = 2
  #        ,lwd = 2
  #        ,bty = "n"
  #        )
  
  abline(h =  c(1.1896,1.2918)
         ,col = c("darkred","navyblue")
        ,lty = 2
        ,lwd = 2)
}
