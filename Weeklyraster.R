setwd("H:\\4.2\\PROJECT\\Week")

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
library(spatstat)

#load rasters
ppt <- list.files("H:/4.2/PROJECT/work_R/New folder",full.names = TRUE, pattern = '.*tif')

##stack rasters
ppt_stack<-stack(ppt)

##create weekly mean rasters from the raster stack
ppt_week<-stackApply(ppt_stack, 1:7, mean)

#assign names to these rasters
names(ppt_week) <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
plot(ppt_week)


##plot with ggplot
##convert rasters to dataframe
ppt.df<-as.data.frame(ppt_week, xy=TRUE)%>%
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
                       colors = turbo(10))+
  ggtitle('WEEKLY VARIATION OF EMS DEMAND')













