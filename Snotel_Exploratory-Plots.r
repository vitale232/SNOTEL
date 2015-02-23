#--------------------------------
# Name:         Snotel_Exploratory-Plots.r
# Purpose:      Play around with the SNOTEL data from the Sierra.
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2015/02/22
# R:            3.1.2
#--------------------------------
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

library(ggplot2)
library(ggmap)

setwd('~/Documents/SNOTEL/')

snotel = read.csv('./snotel_clean.csv')
snotel$date = as.Date(snotel$date)
snotel$wyear_factor = as.factor(snotel$wyear)
snotel$swe_meters = snotel$swe * 0.3048


fig1 = ggplot(snotel) + 
  geom_boxplot(aes(as.factor(elev), swe, fill=site), notch=TRUE) +
  theme_gray(base_size=20) +
  ylab('SWE (inches)') +
  xlab('Elevation (feet)') +
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'))
print(fig1)

ggsave('./Plots/swe-by-elev.pdf')

fig2 = ggplot(snotel) +
  geom_bar(aes(wyear_factor, swe_meters, fill=site), stat='identity') +
  xlab('Water Year') + 
  ylab('SWE (inches)') +
  theme_gray(base_size=17) +
  theme(axis.text.x=element_text(color='black', angle=45, vjust=0.8),
        axis.text.y=element_text(color='black'))
print(fig2)

ggsave('./Plots/swe-wyear-meters.pdf')

fig3 = ggplot(snotel) + 
  geom_bar(aes(wyear_factor, ppt_inc, fill=site), stat='identity') +
  theme_gray(base_size=20) +
  ylab('Precipitation Increment (inches)') +
  xlab('Water Year') +
  theme(axis.text.x=element_text(color='black', angle=45, vjust=0.8),
        axis.text.y=element_text(color='black'))
print(fig3)
ggsave('./Plots/ppt-inc_by_wyear.pdf')



#### make a map
map_data = get_map(location=c(lon=mean(snotel$lon), 
                              lat=mean(snotel$lat)),
                   color='color',
                   source='google',
                   maptype='terrain',
                   zoom=8)

snotel$site = as.character(snotel$site)
snotel$site = gsub('_', ' ', snotel$site)
snotel$site = sapply(snotel$site, simpleCap)

map = ggmap(map_data) +
  geom_point(aes(lon, lat, fill=site), pch=21, data=snotel, size=4) +
  theme_gray(base_size=20) +
  xlab('Longitude') +
  ylab('Latitude') +
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'))
print(map)
ggsave('./Plots/Map.pdf')
ggsave('./Plots/Map.png')

max_swe = aggregate(swe ~ wyear + site, 
                    snotel, max, na.rm=TRUE)
max_swe_1989 = max_swe[max_swe$wyear >= 1989, ]
max_swe_1989 = max_swe_1989[-which(max_swe$site == 'Carson Pass'), ]
lines_df = aggregate(swe ~ wyear, max_swe_1989, sum)

mswePlot = ggplot(max_swe_1989) + 
  geom_bar(aes(factor(wyear), swe, fill=site), stat='identity') +
  xlab('Water Year') +
  ylab('Maximum SWE (inches)') +
  ggtitle('Annual Maximum SWE by SNOTEL Site and Water Year') +
  theme_gray(base_size=20) + 
  theme(axis.text.x=element_text(color='black', angle=45, vjust=0.8),
        axis.text.y=element_text(color='black'))
#   theme(axis.text.x=element_text(angle=45, vjust=0.8))
print(mswePlot)

ggsave('./Plots/annual-max-swe-by-site-and-wyear.pdf')

tmnBoxplots = ggplot(snotel[which((snotel$site == 'Mt Rose' | 
                                  snotel$site == 'Virginia Lakes Ridge') &
                                  snotel$wyear >= 1982), ]) +
# tmnBoxplots = ggplot(snotel) +
  geom_boxplot(aes(factor(wyear), tmn, fill=site), notch=TRUE) +
  xlab('Water Year') +
  ylab(expression(paste('Minimum Temperature (', degree, 'F)'))) +
  ggtitle('SNOTEL Minimum Daily Temperature') +
  theme_gray(base_size=20) +
  theme(axis.text.x=element_text(color='black', angle=45, vjust=0.8),
        axis.text.y=element_text(color='black'))
print(tmnBoxplots)
ggsave('./Plots/tmn-boxplots.pdf')

cols = c('Mt Rose'='#DC143C',
         'Virginia Lakes Ridge'='#0033bf')
tmn_df = snotel[which((snotel$site == 'Mt Rose' | 
                         snotel$site == 'Virginia Lakes Ridge') &
                        snotel$wyear >= 1982), ]
tmn_df$site = factor(tmn_df$site)

tmnPlot = ggplot(tmn_df) +
  geom_point(aes(date, tmn, color=site), size=1) +
  scale_color_manual(name='Site', values=cols) +
  stat_smooth(aes(date, tmn), method='lm', color='#141414')
print(tmnPlot)
ggsave('./Plots/rose-virginia_tmn_mod.pdf')
