setwd("H:\\4.2\\PROJECT\\monthly\\Data")

library(rgdal)
library(raster)
library(spatstat)
library(maptools)
library(sf)
library(maptools)
library(raster)
library(RColorBrewer)
library(spatstat)
library(raster)
library(gstat )
library(sp)
library(ggplot2)
library(viridis)
library(tidyverse)
theme_set(theme_bw())
library(lubridate)


#March
# Load a March.shp point feature shapefile
March <- st_read("March.shp")

# convert to planar point pattern
March.ppp = as.ppp(March)
unitname(March.ppp)= c("metre", "metres")
March.ppp.km <- rescale(March.ppp, 10000, "km")


# observation window from shapefile
M_mask  <- st_read("Kiambu.shp")
win = as.owin(M_mask, unitname = "metre")
win.km <- rescale(win, 10000, "km")


# observation window from extent
Window(March.ppp.km)= win.km
March.ppp.km = unmark(March.ppp.km)

plot(layered(March.ppp.km, win.km), main = "EMS calls in March")

# kernel smoothed intensity
Mar = density.ppp(March.ppp.km)
plot(Mar)

#convert kernel smoothed intensity to raster
Mar_r = raster(Mar)
writeRaster(Mar_r, "1.tif", overwrite = T)

#convert raster to a dataframe
df_Mar = as.data.frame(Mar_r)


#April
# Load a April.shp point feature shapefile
April <- st_read("April.shp")

# convert to planar point pattern
April.ppp = as.ppp(April)
unitname(April.ppp)= c("metre", "metres")
April.ppp.km <- rescale(April.ppp, 10000, "km")

# observation window from shapefile
mask  <- st_read("Kiambu.shp")
win = as.owin(mask, unitname = "metre")
win.km <- rescale(win, 10000, "km")

# observation window from extent
Window(April.ppp.km)= win.km
April.ppp.km = unmark(April.ppp.km)

plot(layered(April.ppp.km, win.km), main = "EMS calls in April")

# kernel smoothed intensity
Apr = density.ppp(April.ppp.km)
plot(Apr)

#convert kernel smoothed intensity to raster
Apr_r = raster(Apr)
writeRaster(Apr_r, "2.tif", overwrite = T)

#convert raster to a dataframe
df_Apr = as.data.frame(Apr_r)


#May
# Load a May.shp point feature shapefile
May <- st_read("May.shp")

# convert to planar point pattern
May.ppp = as.ppp(May)
unitname(May.ppp)= c("metre", "metres")
May.ppp.km <- rescale(May.ppp, 10000, "km")

# observation window from shapefile
w_mask  <- st_read("Kiambu.shp")
win = as.owin(w_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")


# observation window from extent
Window(May.ppp.km)= win.km
May.ppp.km = unmark(May.ppp.km)

plot(layered(May.ppp.km, win.km), main = "EMS calls in May")

Mayy = density.ppp(May.ppp.km)
plot(Mayy)

#convert kernel smoothed intensity to raster
May_r = raster(Mayy)
writeRaster(May_r, "3.tif", overwrite = T)

#convert raster to a dataframe
df_May = as.data.frame(May_r)


#June
# Load a June.shp point feature shapefile
June <- st_read("June.shp")

# convert to planar point pattern
June.ppp = as.ppp(June)
unitname(June.ppp)= c("metre", "metres")
June.ppp.km <- rescale(June.ppp, 10000, "km")

# observation window from shapefile
Ju_mask  <- st_read("Kiambu.shp")
win = as.owin(Ju_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")

# observation window from extent
Window(June.ppp.km)= win.km
June.ppp.km = unmark(June.ppp.km)

plot(layered(June.ppp.km, win.km), main = "EMS calls in June")

# kernel smoothed intensity
Jun = density.ppp(June.ppp.km)
plot(Jun)

#convert kernel smoothed intensity to raster
June_r = raster(Jun)
writeRaster(June_r, "4.tif", overwrite=TRUE)

#convert raster to a dataframe
df_June = as.data.frame(June_r)


#July
# Load a July.shp point feature shapefile
July <- st_read("July.shp")

# convert to planar point pattern
July.ppp = as.ppp(July)
unitname(July.ppp)= c("metre", "metres")
July.ppp.km <- rescale(July.ppp, 10000, "km")

# observation window from shapefile
Ju_mask  <- st_read("Kiambu.shp")
win = as.owin(Ju_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")


# observation window from extent
Window(July.ppp.km)= win.km
July.ppp.km = unmark(July.ppp.km)


plot(layered(July.ppp.km, win.km), main = "EMS calls in July")

# kernel smoothed intensity
Jul = density.ppp(July.ppp.km)
plot(Jul)

#convert kernel smoothed intensity to raster
July_r = raster(Jul)
writeRaster(July_r, "5.tif", overwrite=TRUE)

#convert raster to a dataframe
df_July = as.data.frame(July_r)


#August
# Load a August.shp point feature shapefile
August <- st_read("August.shp")

# convert to planar point pattern
August.ppp = as.ppp(August)
unitname(August.ppp)= c("metre", "metres")
August.ppp.km <- rescale(August.ppp, 10000, "km")

# observation window from shapefile
A_mask  <- st_read("Kiambu.shp")
win = as.owin(A_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")

# observation window from extent
Window(August.ppp.km)= win.km
August.ppp.km = unmark(August.ppp.km)

plot(layered(August.ppp.km, win.km), main = "EMS calls in August")

# kernel smoothed intensity
Aug = density.ppp(August.ppp.km)
plot(Aug)

#convert kernel smoothed intensity to raster
August_r = raster(Aug)
writeRaster(August_r, "6.tif", overwrite=TRUE)

#convert raster to a dataframe
df_Aug = as.data.frame(August_r)



