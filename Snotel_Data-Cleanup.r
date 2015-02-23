#--------------------------------
# Name:         Snotel_Data-Cleanup.r
# Purpose:      Play around with the SNOTEL data from the Sierra.
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2015/02/22
# R:            3.1.2
#--------------------------------

library(ggplot2)

setwd('~/Documents/SNOTEL/')

## read in the meta data csv and convert to decimal degrees
meta_data = read.csv('./site_metadata.csv')
meta_data$lon = with(meta_data, lon_deg + lon_min/60)
meta_data$lat = with(meta_data, lat_deg + lat_min/60)
lf = list.files(pattern='.txt$')

## read in the snotel .txt files as data.frames in a  list and 
## label the list with appropriate names
data_frame_list = lapply(lf, read.csv, skip=7, header=TRUE)
names = gsub('.txt', '', lf)
names = gsub('-', '_', names)
names(data_frame_list) = names


## Match the metadata with the snotel data and rename the columns
data_frame_list = lapply(1:length(data_frame_list), function(i){
  x = data_frame_list[[i]]
  elev = meta_data[meta_data$site == names[i], 'elev']
  lon = meta_data[meta_data$site == names[i], 'lon']
  lat = meta_data[meta_data$site == names[i], 'lat']
  
  x$site = rep(names[i], nrow(x))
  x$elev = elev
  x$lon = lon
  x$lat = lat
  
  names(x) = c('date', 'swe', 'ppt_acm', 
               'tmx', 'tmn', 'tav', 
               'ppt_inc', 'site', 'elev',
               'lon', 'lat')
  return(x)
})

## Hackish way to take the data.frames out of a list
snotel = data_frame_list[[1]]
for(i in 2:length(data_frame_list)){
  snotel = rbind(snotel, data_frame_list[[i]])
}

## Add year, month, doy and water yeaer to the data
snotel$date = as.Date(snotel$date)
snotel$site = as.factor(snotel$site)
snotel$year = as.numeric(format(snotel$date, '%Y'))
snotel$month = as.numeric(format(snotel$date, '%m'))
snotel$doy = as.numeric(format(snotel$date, '%j'))
snotel$wyear_flag = with(snotel, {
  ifelse(month >= 11, 1, 0)
})
snotel$wyear = with(snotel, year + wyear_flag) 


## Write out a nice, clean .csv file
write.csv(snotel, './snotel_clean.csv', row.names=FALSE)

# 
# tmxPlot = ggplot(snotel) +
#   geom_point(aes(date, tmx, color=site)) +
#   facet_wrap( ~ site)
# print(tmxPlot)
# 
# tmnPlot = ggplot(snotel) +
#   geom_point(aes(date, tmn, color=site)) +
#   facet_wrap( ~ site)
# print(tmnPlot)
# 
# cols = c('Minimum Temperature'='#4675A8',
#          'Maximum Temperature'='#6D0E0E')
# tPlot = ggplot(snotel) +
#   geom_point(aes(date, tmn, color='Minimum Temperature'), size=1, alpha=0.4) +
#   geom_point(aes(date, tmx, color='Maximum Temperature'), size=1, alpha=0.4) +
#   facet_wrap( ~ site) +
#   scale_color_manual(name='', values=cols) +
#   xlab('Date') +
#   ylab(expression(paste('Temperature (', degree, 'F)'))) +
#   theme_gray(base_size=18)
# print(tPlot)
# 
# pptAcmPlot = ggplot(snotel) + 
#   geom_line(aes(date, ppt_acm, color=site)) +
#   facet_wrap( ~ site) +
#   guides(fill=FALSE)
# print(pptAcmPlot)
# 
# 
# 
# pptIncPlot = ggplot(snotel) +
#   geom_point(aes(date, ppt_inc, color=site)) +
#   facet_wrap( ~ site) +
#   guides(fill=FALSE)
# print(pptIncPlot)
# 
# 
# 
# swePlot = ggplot(snotel) +
#   geom_line(aes(date, swe, color=site)) +
#   facet_wrap( ~ site) +
#   guides(fill=FALSE)
# print(swePlot)
