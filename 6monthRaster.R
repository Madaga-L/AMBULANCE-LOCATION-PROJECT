setwd("H:\\4.2\\PROJECT\\monthly\\Data")

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
ppt <- list.files("H:\\4.2\\PROJECT\\monthly\\Data",full.names = TRUE, pattern = '.*tif')

##stack rasters
ppt_stack<-stack(ppt)

##create monthly mean rasters from the raster stack
ppt_month<-stackApply(ppt_stack, 1:6, mean)

#assign names to these rasters
names(ppt_month) <- c('March', 'April', 'May', 'June', 'July', 'August')
plot(ppt_month)


##plot with ggplot
##convert rasters to dataframe
ppt.df<-as.data.frame(ppt_month, xy=TRUE)%>%
  reshape2::melt(id.vars=c('x','y'))

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
  scale_fill_gradientn(name = "EMS calls(per 10 Km2)",
                       colors = brewer.pal(12, 'Paired'))+
  ggtitle('Monthly VARIATION OF EMS DEMAND')


ggplot()+
  geom_raster(data = ppt.df, aes(x=x, y=y, fill = value))+
  facet_wrap(~variable)+
  labs(x='x', y='y')+
  scale_fill_gradientn(name = "calls(per 10 Km2)",
                       colors = turbo(10))

#scale and North arrow

pl <- ggplot()+
  geom_raster(data = ppt.df, aes(x=x, y=y, fill = value))+
  facet_wrap(~variable)+
  labs(x='x', y='y')+
  scale_fill_gradientn(name = "calls(per 10 Km2)",
                       colors = turbo(10))


# Add scale and North arrow
pl +
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )













