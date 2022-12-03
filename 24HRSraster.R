setwd("H:\\4.2\\PROJECT\\Hourly22")

library(rgdal)
library(raster)
library(sf)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(maptools)
library(viridis)
library(gstat )
library(sp)
library(dplyr)
library(spData)
library(spDataLarge)
library(spatstat)
library(tmap)
library(haven)
library(labelled)
library(ggspatial)

#load rasters
ppt <- list.files("H:\\4.2\\PROJECT\\Hourly",full.names = TRUE, pattern = '.*tif')

##stack rasters
ppt_stack<-stack(ppt)

##create weekly mean rasters from the raster stack
ppt_hour<-stackApply(ppt_stack, 1:24, mean)

#assign names to these rasters
names(ppt_hour) = c('OO:OOAM_OO:59AM', 'O1:OOAM_O1:59AM', 'O2:OOAM_O2:59AM', 'O3:OOAM_O3:59AM', 'O4:OOAM_O4:59AM', 'O5:OOAM_O5:59AM', 'O6:OOAM_O6:59AM', 'O7:OOAM_O7:59AM', 'O8:OOAM_O8:59AM', 'O9:OOAM_O9:59AM', 'IO:OOAM_IO:59AM', 'II:OOAM_II:59AM', 'I2:OOPM_I2:59PM', 'O1:OOPM_O1:59PM', 'O2:OOPM_O2:59PM', 'O3:OOPM_O3:59PM', 'O4:OOPM_O4:59PM', 'O5:OOPM_O5:59PM', 'O6:OOPM_O6:59PM', 'O7:OOPM_O7:59PM', 'O8:OOPM_O8:59PM', 'O9:OOPM_O9:59PM', 'IO:OOPM_IO:59PM', 'II:OOPM_II:59PM')
plot(ppt_hour)


##plot with ggplot
##convert rasters to dataframe
ppt.df<-as.data.frame(ppt_hour, xy=TRUE)%>%
  melt(id.vars=c('x','y'))

#plot
ggplot()+
  geom_raster(data = ppt.df, aes(x=x, y=y, fill = value))+
  facet_wrap(~variable)+
  labs(x='x', y='y')

## add colour gradient
ggplot()+
  geom_raster(data = ppt.df, aes(x=x, y=y, fill = value))+
  facet_wrap(~variable)+
  labs(x='x', y='y')+
  scale_fill_gradientn(colors = turbo(10))+
  ggtitle('WEEKLY VARIATION OF EMS DEMAND')


ggplot()+
  geom_raster(data = ppt.df, aes(x=x, y=y, fill = value))+
  facet_wrap(~variable)+
  labs(x='x', y='y')+
  scale_fill_gradientn(name = "calls(per 10 Km2)",
                       colors = brewer.pal(12, 'Paired'))+
  ggtitle('WEEKLY VARIATION OF EMS DEMAND')


ggplot()+
  geom_raster(data = ppt.df, aes(x=x, y=y, fill = value))+
  facet_wrap(~variable)+
  labs(x='x', y='y')+
  scale_fill_gradientn(name = "calls(per 10 Km2)",
                       colors = turbo(10))
















