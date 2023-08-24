# producing the images with distributions of weighted vs non weighted detour values that are used in the final paper. 

library(rlist)
library(data.table)
library(sf)
library(Btoolkit)
library(cppSim)
library(foreach)
library(weights)


##

# source the script that produces the detour_dt data set
source('')

##

# plotting summary statistics for the detour distribution 
# by distance intervals that can be manually changed. 

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

detour_bin_plot <- function(left, right, col = 'black',...) {
  detour_dt[crow_fly <= right & crow_fly > left,
            lines(density(detour
                          ,weights = flow/sum(flow))
                  ,col = col
                  ,...)]
}

detour_bin_plot_2 <- function(left, right,col = 'darkred',...) {
  detour_dt[crow_fly <= right & crow_fly > left,
            lines(density(detour)
                  ,lty = 5
                  ,col = col
                  ,...)]
}

col <- c('pink','darkgreen','orange','darkblue')  
l_wd <- 1.5
values <- c(3000,6000,9000,12000,15000)
par(cex.lab=1.3
    ,mar = c(5,5,4,1))

# PLOTTING THE DETOURS FOR DIFFERENT DISTANCE INTERVALS, WEIGHTED AND NON WEIGHTED. 
{ 
  # alphas <- c(.1,.3,.5,.7,.9)
  res_osm <- data.frame('detour_c'= double(),'detour_n'=double(),'t-test'=double())
  
  plot(x = 1
       ,xlim=c(1,2)
       ,xlab = expression(delta)
       ,ylim=c(0,11)
       ,ylab = expression(P(delta))
       ,type = "n"
       ,main='Detour by distance interval, OSM network')
  detour_bin_plot(0,values[1],lwd = l_wd,col = 'darkred')
  cat('Flow weighted av. detour',detour_bin_val(0,values[1]),'\n')
  #
  detour_bin_plot_2(0,values[1], lwd = l_wd)
  cat('Av. detour',detour_bin_val_2(0,values[1]),'\n')
  dat <- detour_dt[crow_fly <= values[1] & crow_fly > 0,]
  cat(nrow(dat),' observations in that interval.\n')
  t_val <- wtd.t.test(x = dat$detour,weight = (dat$flow/sum(dat$flow))
                      ,y = dat$detour, weighty = rep_len(1,nrow(dat)))$coefficients[1]
  cat('T-test: ',t_val,'\n')
  res_osm <- rbind(res_osm,c(detour_bin_val(0,values[1]),detour_bin_val_2(0,values[1]),t_val))
  # par(new =)
  for (j in 1:(length(values)-1)) {
    detour_bin_plot(values[j],values[j+1], col = col[j],lwd=l_wd,alpha = alphas[j])
    
    detour_c <- detour_bin_val(values[j],values[j+1])
    cat('Flow weighted av. detour',detour_c,'\n')
    #
    detour_bin_plot_2(values[j],values[j+1], col = col[j],lwd=l_wd,alpha = alphas[j])
  
    detour_n <- detour_bin_val_2(values[j],values[j+1])
    cat('Av. detour',detour_n,'\n')
    
    dat = detour_dt[crow_fly <= values[j+1] & crow_fly > values[j],]
    cat(nrow(dat[flow!=0,]),' observations in that interval.\n')
    t_val <- wtd.t.test(x = dat$detour,weight = (dat$flow/sum(dat$flow))
                        ,y = dat$detour, weighty = rep_len(1,nrow(dat)))$coefficients[1]
    cat('T-test: ',t_val,'\n')
    res_osm <- rbind(res_osm,c(detour_c,detour_n,t_val))
  }
  
  legend(x=1.5
         ,y=10.5
         # ,cex = 1.2
         ,legend = c('weighted','non-weighted')
         ,lty = c(1,2)
         ,lwd = l_wd
         ,col = 'grey')
  legend(x=1.5
         ,y=8
         # ,cex = 1.2
         ,col = c('darkred',col)
         ,lwd = l_wd
         ,lty = c(1)
         ,legend = c('0-3','3-6','6-9','9-12','12-15')
         ,title = 'Distance (km)')
  
  res_osm <- `colnames<-`(res_osm,c('detour_c','detour_n','t-test'))
  # res_osm
}

lapply(res_osm,print)

# OS ----
# Doing the same thing for the OS data set. 

# helper functions ----
detour_os_bin_val <- function(left, right) {
  detour_dt_os[crow_fly <= right & crow_fly > left,
            weighted.mean(detour
                          ,w = flow/sum(flow))]
}

detour_os_bin_val_2 <- function(left, right) {
  detour_dt_os[crow_fly <= right & crow_fly > left,
            mean(detour)]
}

detour_os_bin_plot <- function(left, right, col = 'black',...) {
  detour_dt_os[crow_fly <= right & crow_fly > left,
            lines(density(detour
                          ,weights = flow/sum(flow))
                  ,col = col
                  ,...)]
}

detour_os_bin_plot_2 <- function(left, right,col = 'darkred',...) {
  detour_dt_os[crow_fly <= right & crow_fly > left,
            lines(density(detour)
                  ,lty = 5
                  ,col = col
                  ,...)]
}


{ 
  # col <- c('pink','darkgreen','orange','darkblue')  
  # values <- c(3000,6000,9000,12000,15000)
  # l_wd <- 1.5
  
  res_os <- data.frame('detour_c'= double(),'detour_n'=double(),'t-test'=double())
  
  plot(x = 1
       ,xlim=c(1,2)
       ,xlab = expression(delta)
       ,ylim=c(0,11)
       ,ylab = expression(P(delta))
       ,type = "n"
       ,main='Detour by distance interval, OS network')
  detour_os_bin_plot(0,values[1],lwd = l_wd,col = 'darkred')
  cat('Flow weighted av. detour',detour_os_bin_val(0,values[1]),'\n')
  #
  detour_os_bin_plot_2(0,values[1], lwd = l_wd)
  cat('Av. detour',detour_os_bin_val_2(0,values[1]),'\n')
  dat <- detour_dt_os[crow_fly <= values[1] & crow_fly > 0,]
  cat(nrow(dat),' observations in that interval.\n')
  t_val <- wtd.t.test(x = dat$detour,weight = (dat$flow/sum(dat$flow))
                      ,y = dat$detour, weighty = rep_len(1,nrow(dat)))$coefficients[1]
  cat('T-test: ',t_val,'\n')
  # par(new =)
  res_os <- rbind(res_os,c(detour_os_bin_val(0,values[1]),detour_os_bin_val_2(0,values[1]),t_val))
  
  for (j in 1:(length(values)-1)) {
    detour_os_bin_plot(values[j],values[j+1], col = col[j],lwd=l_wd)
    #
    cat('Flow weighted av. detour',detour_os_bin_val(values[j],values[j+1]),'\n')
    detour_c <- detour_os_bin_val(values[j],values[j+1])
    #
    detour_os_bin_plot_2(values[j],values[j+1], col = col[j],lwd=l_wd)
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
  
  legend(x=1.5
         ,y=10.5
         # ,cex = 1.3
         ,legend = c('weighted','non-weighted')
         ,lty = c(1,2)
         ,lwd = l_wd
         ,col = 'grey')
  legend(x=1.5
         ,y=8
         # ,cex = 1.3
         ,col = c('darkred',col)
         ,lwd = l_wd
         ,lty = c(1)
         ,legend = c('0-3','3-6','6-9','9-12','12-15')
         ,title = 'Distance (km)')
  
  res_os <- `colnames<-`(res_os,c('detour_c','detour_n','t-test'))
}

res_osm
res_os

# latex table used to summarise this
# 
# % \usepackage{graphicx}
# \begin{table}[]
# \centering
# \resizebox{\textwidth}{!}{%
#   \begin{tabular}{ccccccc}
#   & \multicolumn{3}{c}{OSM}                       & \multicolumn{3}{c}{OS}                       \\ \cline{2-7} 
#   \multicolumn{1}{c|}{band (km)} &
#     \multicolumn{1}{c|}{\delta_{commute}} &
#     \multicolumn{1}{c|}{\delta_{network}} &
#     \multicolumn{1}{c|}{T-test} &
#     \multicolumn{1}{c|}{\delta_{commute}} &
#     \multicolumn{1}{c|}{\delta_{network}} &
#     \multicolumn{1}{c|}{T-test} \\ \hline
#   \multicolumn{1}{|c|}{0-3}   & 1.337 & 1.330 & \multicolumn{1}{c|}{3.624}    & 1.667 & 1.580 & \multicolumn{1}{c|}{24.298}  \\ \cline{1-1}
#   \multicolumn{1}{|c|}{3-6}   & 1.195 & 1.233 & \multicolumn{1}{c|}{-52.651}  & 1.324 & 1.366 & \multicolumn{1}{c|}{-33.490} \\ \cline{1-1}
#   \multicolumn{1}{|c|}{6-9}   & 1.153 & 1.196 & \multicolumn{1}{c|}{-101.772} & 1.240 & 1.294 & \multicolumn{1}{c|}{-70.848} \\ \cline{1-1}
#   \multicolumn{1}{|c|}{9-12}  & 1.138 & 1.174 & \multicolumn{1}{c|}{-119.724} & 1.206 & 1.251 & \multicolumn{1}{c|}{-88.484} \\ \cline{1-1}
#   \multicolumn{1}{|c|}{12-15} & 1.130 & 1.159 & \multicolumn{1}{c|}{-124.320} & 1.185 & 1.222 & \multicolumn{1}{c|}{-95.923} \\ \hline
#   \end{tabular}%
# }
# \end{table}




