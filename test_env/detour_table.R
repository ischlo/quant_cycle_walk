
# investigating the detour within distance bands and producing a table with t test of the significance of the different means
# for weighted and un-weighted distances
# this data is used to generate the table in the report and to draw conclusions
# on the significance of the differences in observed commuting mean detours


library(rlist)
library(data.table)
library(sf)
library(Btoolkit)
library(cppSim)
library(foreach)
library(weights)

# OSM

# helper functions ----
detour_bin_val <- function(left, right) {
  detour_dt[crow_fly <= right & crow_fly > left,
            weighted.mean(detour
                          ,w = flow/sum(flow))]
}

detour_bin_val_2 <- function(left, right) {
  detour_dt[crow_fly <= right & crow_fly > left,
            mean(detour)]
}


# l_wd <- 2

values <- c(2:50)*300

# PLOTTING THE DETOURS FOR DIFFERENT DISTANCE INTERVALS, WEIGHTED AND NON WEIGHTED. 
{ 
  res_osm <- data.frame('detour_c'= double(),'detour_n'=double(),'t-test'=double())
 
  cat('Flow weighted av. detour',detour_bin_val(0,values[1]),'\n')

  cat('Av. detour',detour_bin_val_2(0,values[1]),'\n')
  dat <- detour_dt[crow_fly <= values[1] & crow_fly > 0,]
  cat(nrow(dat),' observations in that interval.\n')
  t_val <- wtd.t.test(x = dat$detour,weight = (dat$flow/sum(dat$flow))
                      ,y = dat$detour, weighty = rep_len(1,nrow(dat)))$coefficients[c(1,3)]
  cat('T-test: ',t_val,'\n')
  res_osm <- rbind(res_osm,c(detour_bin_val(0,values[1]),detour_bin_val_2(0,values[1]),t_val))

  for (j in 1:(length(values)-1)) {
    
    detour_c <- detour_bin_val(values[j],values[j+1])
    cat('Flow weighted av. detour',detour_c,'\n')
    
    detour_n <- detour_bin_val_2(values[j],values[j+1])
    cat('Av. detour',detour_n,'\n')
    
    dat = detour_dt[crow_fly <= values[j+1] & crow_fly > values[j],]
    cat(nrow(dat[flow!=0,]),' observations in that interval.\n')
    t_val <- wtd.t.test(x = dat$detour,weight = (dat$flow/sum(dat$flow))
                        ,y = dat$detour, weighty = rep_len(1,nrow(dat))
                        ,bootse = TRUE)$coefficients[c(1,3)]
  
    cat('T-test: ',t_val,'\n')
    res_osm <- rbind(res_osm,c(detour_c,detour_n,t_val))
  }
  print(head(res_osm))
  
  res_osm <- `colnames<-`(res_osm,c('detour_c','detour_n','t-test','p-val'))
  # res_osm
  values_range <- list()
  values_km <- round(values/1000,2)
  for (i in 1:length(values_km)) {
    if(i==1) values_range <- append(values_range,paste0('0 - ',values_km[i]))
    else values_range <- append(values_range,paste0(values_km[i-1],' - ',values_km[i]))
  }
  res_osm$interval <- values_range
}

res_osm

{
  res_os <- data.frame('detour_c'= double(),'detour_n'=double(),'t-test'=double())

  cat('Flow weighted av. detour',detour_os_bin_val(0,values[1]),'\n')
  cat('Av. detour',detour_os_bin_val_2(0,values[1]),'\n')
  dat <- detour_dt_os[crow_fly <= values[1] & crow_fly > 0,]
  cat(nrow(dat),' observations in that interval.\n')
  t_val <- wtd.t.test(x = dat$detour,weight = (dat$flow/sum(dat$flow))
                      ,y = dat$detour, weighty = rep_len(1,nrow(dat)))$coefficients[1]
  cat('T-test: ',t_val,'\n')
  # par(new =)
  res_os <- rbind(res_os,c(detour_os_bin_val(0,values[1]),detour_os_bin_val_2(0,values[1]),t_val))
  
  for (j in 1:(length(values)-1)) {
    
    #
    cat('Flow weighted av. detour',detour_os_bin_val(values[j],values[j+1]),'\n')
    detour_c <- detour_os_bin_val(values[j],values[j+1])
    #
    cat('Av. detour',detour_os_bin_val_2(values[j],values[j+1]),'\n')
    detour_n <- detour_os_bin_val_2(values[j],values[j+1])
    #
    dat = detour_dt_os[crow_fly <= values[j+1] & crow_fly > values[j],]
    cat(nrow(dat[flow!=0,]),' observations in that interval.\n')
    t_val <- wtd.t.test(x = dat$detour,weight = (dat$flow/sum(dat$flow))
                        ,y = dat$detour, weighty = rep_len(1,nrow(dat)))$coefficients[1]
    cat('T-test: ',t_val,'\n')
    res_os <- rbind(res_os,c(detour_c,detour_n,t_val))
  }
  
  res_os <- `colnames<-`(res_os,c('detour_c','detour_n','t-test'))
}

detour_significance_table <- cbind(res_os,res_osm)


detour_significance_table




