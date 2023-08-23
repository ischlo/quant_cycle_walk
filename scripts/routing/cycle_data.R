#  THIS IS A KIND OF SIDE SCRIPT THAT WAS USED TO VALIDATE THE CYCLING DATA. 
# IT USES DATA FROM SANTANDER CYCLE TRIPS AVAILABLE THOROUGH THE BIKEDATA PACKAGE TO CHECK SOME SUMMARY STATISTICS.
#  IT IS SLIGHLTY OURTDATED AND WILL MOST LIKELY NOT RUN IN ONE GO. 

library(bikedata)
gc()

#### Here some real world cycle data is imported and explored
# witht the use of the bike data package: 
# https://docs.ropensci.org/bikedata/articles/bikedata.html#downloading-data 

bike_cities()

# this creates locally a temporary database and allows sending queries.
bikedb <- file.path(tempdir(), "bikedb.sqlite")
dl_bikedata(city = "lo",dates = 2019,data_dir = tempdir())
store_bikedata(city = "lo", bikedb = bikedb, quiet = FALSE)
db <- dplyr::src_sqlite(bikedb, create=F)
dplyr::src_tbls(db)

# indexing the data 
index_bikedata_db(bikedb = bikedb)
bike_db_totals(bikedb = bikedb)

# stations:
stations <- bike_stations(bikedb = bikedb) %>% 
  st_as_sf(coords = c("longitude","latitude"), remove = FALSE, crs = 4326)

tmap_mode("view")
stations %>% qtm()

london_centroids <- london_msoa %>% st_as_sf() %>% st_centroid()

predicate <- stations %>% st_intersects(london_msoa %>% st_as_sf()
                           ,sparse = TRUE)

# tmap_mode("view")
# stations[332,] %>% qtm() + qtm(london_msoa %>% st_as_sf())
# 
# which(predicate %>% 
#   as.numeric() %>% is.na())
#   
# predicate %>% 
#   as.numeric() %>%
#   .[332]

central_centroid <- london_centroids[predicate %>% 
                                       as.numeric() %>% 
                                       unique() %>% sort(),]

# assign the index of the msoa to each bike station
stations$msoa <- predicate %>% as.numeric()
# 1 stations is outside the msoa polygons because it's near the river.
# remove it. 
stations <- stations %>% drop_na(msoa)

match(bike_trips$start_station_id,stations$stn_id)

# loading the number of trips for all the existing pairs of msoa in the database
# a bit time consuming
bike_trips <- bike_tripmat(bikedb = bikedb
                           ,start_time = 8
                           ,end_time = 10
                           ,long = TRUE)

# done with dplyr, but data.table could work to. 
# assign to every orig-dest pairs a msoa id. 
bike_trips <- bike_trips %>% merge(stations[,c("stn_id","msoa")] # %>% st_drop_geometry()
                                   ,by.x = "start_station_id", by.y = "stn_id")
bike_trips <- bike_trips %>% mutate(start_msoa = msoa,msoa =NULL)

bike_trips <- bike_trips %>% merge(stations[,c("stn_id","msoa")] # %>% st_drop_geometry()
                                   ,by.x = "end_station_id", by.y = "stn_id")
bike_trips <- bike_trips %>% mutate(end_msoa = msoa,msoa =NULL)

# group the trips and find the number of travels between msoas.
bike_trips <- bike_trips %>%
  group_by(start_msoa,end_msoa) %>%
  summarise(trips = sum(numtrips))

bike_trips <- bike_trips %>% 
  drop_na()

bike_trips$start_msoa %>% unique() %>% length()

#### data cleaning ####

# bike_trips

#### ####
# making a flows matrix
bike_trips_mat <- bike_trips %>% acast(start_msoa ~ end_msoa, value.var = "trips")

# checking that the dimensions of matrices correspond.
bike_trips_mat %>% dim()
london_central_msoa_dist %>% dim()

# melting the two matrices together. 
bike_dist <- cbind(dist_matrix_london %>% melt(value.name = "distance")
                   ,bike_trips_mat %>% melt(value.name = "trips")) %>% 
  as.data.table()

bike_dist %>% class()

bike_dist_dens <- density(bike_dist[,distance]
                          ,weights = (bike_dist[,trips]/bike_dist[,sum(trips)]))

london_msoa_typ_dist <- london_msoa[unique(as.numeric(predicate)) %>% na.exclude()
                                    ,mean(sqrt(st_area(geometry)))]

plot(bike_dist_dens
     ,xlab = "distance, m"
     ,ylab = "Trip density"
     ,main = "Trip density"
     ,xlim =c(0,23000)
     ,lwd =2)
abline(v = london_msoa_typ_dist
       ,col = "darkred"
       ,lwd = 2)

# plot(dist_matrix_london[which(dist_matrix_london > 0,arr.ind = TRUE)]
#      ,bike_trips_mat[which(dist_matrix_london > 0,arr.ind = TRUE)]
#      ,ylab = "# bike trips"
#      ,xlab = "distance, m"
#      #,log = "xy"
#      )

#### trips duration ####

# time and memory consuming operation
trips_time <- dplyr::collect(dplyr::tbl (db, 'trips'))
trips_time <- trips_time %>% as.data.table()

# filtering the trips that correspond to commute:
# taking only those that start between 8-10 am , and 4-6pm

trips_time[,start_time] %>% head() %>%
  str_split(pattern = " "
            ,simplify = TRUE) %>% .[,2]

trips_time[,start_time := str_split(start_time
                                    ,pattern = " "
                                    ,simplify = TRUE) %>% .[,2]]

trips_time[,stop_time := str_split(stop_time
                                    ,pattern = " "
                                    ,simplify = TRUE) %>% .[,2]]


trips_time[,':='(city=NULL
              #,start_time=NULL
              #,stop_time=NULL
              ,bike_id=NULL
              ,user_type=NULL
              ,birth_year=NULL
              ,gender=NULL)]

trips_time <- trips_time[(start_station_id != end_station_id),]

# careful with the copies (deep vs shallow)
# read https://rdatatable.gitlab.io/data.table/articles/datatable-reference-semantics.html 
# for details

test <- trips_time[1:10000,start_time = as.ITime(start_time)]

test[1:10,start_time] %>% class()

test[hour(start_time) <= 10 & hour(start_time) >= 8,]

trips_time$trip_duration %>% hist(#xlim = c(0.1,2e4)
                                  #,breaks = 150
                                  )

(trips_time$start_station_id %>% unique()) %in% stations$stn_id 

# stations <- stations %>% st_drop_geometry() %>% as.data.table()

## 

number_trips <- trips_time[,.N,by=.(start_station_id,end_station_id)]

or <- "lo307"
dest <- "lo404"

trips_time[start_station_id == or & end_station_id == dest,trip_duration] %>% 
  hist(breaks = 100)

trips_time[start_station_id == or & end_station_id == dest,median(trip_duration)]

number_trips[N == max(N),]

####

trip_time_stations <- trips_time[,.(mean_duration = round(mean(trip_duration))
                                    ,med_duration = median(trip_duration))
                                      ,by=.(start_station_id,end_station_id)]

# trip_time_stations[,mean_duration := round(mean_duration)]

trip_time_stations <- merge(trip_time_stations
                             ,stations[,c("stn_id","msoa")]
                             ,by.x = "start_station_id"
                             ,by.y = "stn_id")

# mean_trip_time_stations$start_station_id %>% is.na() %>% summary()

setnames(trip_time_stations
         ,old = "msoa"
         ,new = "start_msoa")

trip_time_stations <- merge(trip_time_stations
                                 ,stations[,c("stn_id","msoa")]
                                 ,by.x = "end_station_id"
                                 ,by.y = "stn_id")

setnames(trip_time_stations
         ,old = "msoa"
         ,new = "end_msoa")


# mean_trip_time_msoa <- NULL
# med_trip_time <- NULL
# not needed if smart enough to assign the name initially. 
# setnames(mean_trip_time_msoa
#          ,new="trip_duration"
#          ,old= "V1")
                        
# check that the length is 158 to compare to the distances matrix computed earlier. 
dist_matrix <- london_centroids[trip_time_msoa$start_msoa %>% 
                   unique() %>% 
                   sort(),] %>% st_distance() %>%
  set_units(NULL)

trip_time_msoa$start_msoa %>% unique() %>% sort()

trip_time_msoa <- trip_time_stations[,.(mean_duration = mean(med_duration))
                                     ,by = .(start_msoa,end_msoa)]

# matrix of mean trip times between MSOAs
mean_trip_time_matrix <- trip_time_msoa %>% 
  dcast(start_msoa ~ end_msoa
        ,value.var = "mean_duration"
        ,fill = 0) %>% as.matrix()

mean_trip_time_matrix %>% dim

# # matrix of median trip times
# med_trip_time_matrix <- trip_time_msoa %>%
#   dcast(start_msoa ~ end_msoa
#         ,value.var = "med_duration"
#         ,fill = 0) %>% as.matrix()
# 
# med_trip_time_matrix %>% dim()
# mean_trip_time_matrix %>% view

dist_matrix %>% dim

# dividing distance by time and multiplying by 3.6 allows to see average speed in km/h
((dist_matrix/mean_trip_time_matrix[,2:158])*3.6) %>% hist()

(mean_trip_time_matrix[,2:158])

outliers_index <- which(mean_trip_time_matrix[,2:158]>5e4,arr.ind = TRUE)

# plotting the trip 

outliers_lines <- foreach(i = 1:nrow(outliers_index)
                              ,.combine = rbind
                          ) %do% {
  get_lines(
    central_centroid[outliers_index[i,1],]
    ,central_centroid[outliers_index[i,2],])
}

i <- 9
x <- paths_centroid_london[[outliers_index[i,1]]][[outliers_index[i,2]]]

registerDoParallel(cores = 3)
path_corner_case <- foreach(i = 1:nrow(outliers_index)
                            ,.combine = c
                            ,.final = st_as_sf
                            ) %dopar% {
                              x <- paths_centroid_london[[outliers_index[i,1]]][[outliers_index[i,2]]]
                              coords[match(x,node_ID),.(X,Y)] %>%
                                    get_lines(to = NULL) %>%
                                    st_sfc(crs = 4326)
                              }
stopImplicitCluster()

# london_msoa %>% st_as_sf() %>% qtm(fill.alpha = 0.3)
# 
# central_centroid %>% st_as_sf() %>% qtm()

# path_corner_case %>% st_length()
# corner_cases_lines[3,] %>% st_length()
tmap_mode("view")
i <- 1
path_corner_case[i,] %>% qtm(lines.col = "red") + qtm(outliers_lines[i,]
                                                      ,lines.col = "black")

path_corner_case %>% qtm(lines.col = "red") + qtm(outliers_lines
                                                  ,lines.col = "black")
  qtm(central_centroid %>% st_as_sf())

plot(
  london_central_msoa_dist[outliers_index] 
  ,mean_trip_time_matrix[outliers_index]
)


#### MSOA vs LSOA vs OA ####
# illustrate the differences between various spatial subdivisions. 



