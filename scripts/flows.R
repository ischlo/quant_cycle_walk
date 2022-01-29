

flows <- data.table::fread(file.path(data_folder,"wu03ew_msoa.csv"))


flows %>% dim()
flows %>% head()
flows %>% colnames()

flows_columns_interest <- c("Area of residence"
                            ,"Area of workplace"
                            ,"All categories: Method of travel to work"
                            ,"Bicycle"
                            ,"On foot")

population <- flows[,sum(`All categories: Method of travel to work`)]

population_active_travel <- flows_active[,sum(bike+foot)]

population_concerned <- population_active_travel/population

flows_active <- flows[,.(residence = `Area of residence`
                      ,workplace = `Area of workplace`
                      ,total = `All categories: Method of travel to work`
                      ,bike = `Bicycle`
                      ,foot = `On foot`)]


flows_active[1:10,residence]