setwd("H:\\4.2\\PROJECT\\Hourly22")

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
library(tmap)



#0000hrs
# Load a 0000hrs.shp point feature shapefile
oohrs <- st_read("0000hrs.shp")

# convert to planar point pattern
oohrs.ppp = as.ppp(oohrs)
unitname(oohrs.ppp)= c("metre", "metres")
oohrs.ppp.km <- rescale(oohrs.ppp, 10000, "km")


# observation window from shapefile
M_mask  <- st_read("Kiambu.shp")
win = as.owin(M_mask, unitname = "metre")
win.km <- rescale(win, 10000, "km")


# observation window from extent
Window(oohrs.ppp.km)= win.km
oohrs.ppp.km = unmark(oohrs.ppp.km)

plot(layered(oohrs.ppp.km, win.km), main = "EMS calls between 0000hrs - 0059hrs")

# kernel smoothed intensity
oooohrs = density.ppp(oohrs.ppp.km)
plot(oooohrs)

#convert kernel smoothed intensity to raster
oohrs_r = raster(oooohrs)
writeRaster(oohrs_r, "1.tif", overwrite = TRUE)

#convert raster to a dataframe
df_oohrs = as.data.frame(oohrs_r)


#0100hrs
# Load a 0100hrs.shp point feature shapefile
o1oohrs <- st_read("0100hrs.shp")

# convert to planar point pattern
o1oohrs.ppp = as.ppp(o1oohrs)
unitname(o1oohrs.ppp)= c("metre", "metres")
o1oohrs.ppp.km <- rescale(o1oohrs.ppp, 10000, "km")

# observation window from shapefile
mask  <- st_read("Kiambu.shp")
win = as.owin(mask, unitname = "metre")
win.km <- rescale(win, 10000, "km")

# observation window from extent
Window(o1oohrs.ppp.km)= win.km
o1oohrs.ppp.km = unmark(o1oohrs.ppp.km)

plot(layered(o1oohrs.ppp.km, win.km), main = "EMS calls between 0100hrs - 0059hrs")

# kernel smoothed intensity
o1oo = density.ppp(o1oohrs.ppp.km)
plot(o1oo)

#convert kernel smoothed intensity to raster
o1oo_r = raster(o1oo)
writeRaster(o1oo_r, "2.tif", overwrite = TRUE)

#convert raster to a dataframe
df_o1oo = as.data.frame(o1oo_r)


#0200hrs
# Load a 0200hrs.shp point feature shapefile
o2oohrs <- st_read("0200hrs.shp")

# convert to planar point pattern
o2oohrs.ppp = as.ppp(o2oohrs)
unitname(o2oohrs.ppp)= c("metre", "metres")
o2oohrs.ppp.km <- rescale(o2oohrs.ppp, 10000, "km")

# observation window from shapefile
w_mask  <- st_read("Kiambu.shp")
win = as.owin(w_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")


# observation window from extent
Window(o2oohrs.ppp.km)= win.km
o2oohrs.ppp.km = unmark(o2oohrs.ppp.km)

plot(layered(o2oohrs.ppp.km, win.km), main = "EMS calls between 0200hrs - 0259hrs")

o2oo = density.ppp(o2oohrs.ppp.km)
plot(o2oo)

#convert kernel smoothed intensity to raster
o2oo_r = raster(o2oo)
writeRaster(o2oo_r, "3.tif", overwrite=TRUE)

#convert raster to a dataframe
df_o2oo = as.data.frame(o2oo_r)


#0300hrs
# Load a 0300hrs.shp point feature shapefile
o3oohrs <- st_read("0300hrs.shp")

# convert to planar point pattern
o3oohrs.ppp = as.ppp(o3oohrs)
unitname(o3oohrs.ppp)= c("metre", "metres")
o3oohrs.ppp.km <- rescale(o3oohrs.ppp, 10000, "km")

# observation window from shapefile
o3_mask  <- st_read("Kiambu.shp")
win = as.owin(o3_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")

# observation window from extent
Window(o3oohrs.ppp.km)= win.km
o3oohrs.ppp.km = unmark(o3oohrs.ppp.km)

plot(layered(o3oohrs.ppp.km, win.km), main = "EMS calls between 0300hrs - 0359hrs")

# kernel smoothed intensity
o3oo = density.ppp(o3oohrs.ppp.km)
plot(o3oo)

#convert kernel smoothed intensity to raster
o3oo_r = raster(o3oo)
writeRaster(o3oo_r, "4.tif", overwrite=TRUE)

#convert raster to a dataframe
df_o3oo = as.data.frame(o3oo_r)


#0400hrs
# Load a 0400hrs.shp point feature shapefile
o4oohrs <- st_read("0400hrs.shp")

# convert to planar point pattern
o4oohrs.ppp = as.ppp(o4oohrs)
unitname(o4oohrs.ppp)= c("metre", "metres")
o4oohrs.ppp.km <- rescale(o4oohrs.ppp, 10000, "km")

# observation window from shapefile
o4_mask  <- st_read("Kiambu.shp")
win = as.owin(o4_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")


# observation window from extent
Window(o4oohrs.ppp.km)= win.km
o4oohrs.ppp.km = unmark(o4oohrs.ppp.km)


plot(layered(o4oohrs.ppp.km, win.km), main = "EMS calls between 0400hrs - 0459hrs")

# kernel smoothed intensity
o4oo = density.ppp(o4oohrs.ppp.km)
plot(o4oo)

#convert kernel smoothed intensity to raster
o4oo_r = raster(o4oo)
writeRaster(o4oo_r, "5.tif", overwrite=TRUE)

#convert raster to a dataframe
df_o4oo = as.data.frame(o4oo_r)


#0500hrs
# Load a 0500.shp point feature shapefile
o5oohrs <- st_read("0500hrs.shp")

# convert to planar point pattern
o5oohrs.ppp = as.ppp(o5oohrs)
unitname(o5oohrs.ppp)= c("metre", "metres")
o5oohrs.ppp.km <- rescale(o5oohrs.ppp, 10000, "km")

# observation window from shapefile
o5_mask  <- st_read("Kiambu.shp")
win = as.owin(o5_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")

# observation window from extent
Window(o5oohrs.ppp.km)= win.km
o5oohrs.ppp.km = unmark(o5oohrs.ppp.km)

plot(layered(o5oohrs.ppp.km, win.km), main = "EMS calls between 0500hrs -0559hrs")

# kernel smoothed intensity
o5oo = density.ppp(o5oohrs.ppp.km)
plot(o5oo)

#convert kernel smoothed intensity to raster
o5oo_r = raster(o5oo)
writeRaster(o5oo_r, "6.tif", overwrite=TRUE)

#convert raster to a dataframe
df_o5oo = as.data.frame(o5oo_r)


#0600hrs
# Load a 0600hrs.shp point feature shapefile
o6oohrs <- st_read("0600hrs.shp")

# convert to planar point pattern
o6oohrs.ppp = as.ppp(o6oohrs)
unitname(o6oohrs.ppp)= c("metre", "metres")
o6oohrs.ppp.km <- rescale(o6oohrs.ppp, 10000, "km")


# observation window from shapefile
o6_mask  <- st_read("Kiambu.shp")
win = as.owin(o6_mask, unitname = "metre")
win.km <- rescale(win, 10000, "km")


# observation window from extent
Window(o6oohrs.ppp.km)= win.km
o6oohrs.ppp.km = unmark(o6oohrs.ppp.km)

plot(layered(o6oohrs.ppp.km, win.km), main = "EMS calls between 0600hrs - 0659hrs")

# kernel smoothed intensity
o6oo = density.ppp(o6oohrs.ppp.km)
plot(o6oo)

#convert kernel smoothed intensity to raster
o600_r = raster(o6oo)
writeRaster(o600_r, "7.tif", overwrite = TRUE)

#convert raster to a dataframe
df_o6hrs = as.data.frame(o600_r)


#0700hrs
# Load a 0700hrs.shp point feature shapefile
o7oohrs <- st_read("0700hrs.shp")

# convert to planar point pattern
o7oohrs.ppp = as.ppp(o7oohrs)
unitname(o7oohrs.ppp)= c("metre", "metres")
o7oohrs.ppp.km <- rescale(o7oohrs.ppp, 10000, "km")

# observation window from shapefile
mask  <- st_read("Kiambu.shp")
win = as.owin(mask, unitname = "metre")
win.km <- rescale(win, 10000, "km")

# observation window from extent
Window(o7oohrs.ppp.km)= win.km
o7oohrs.ppp.km = unmark(o7oohrs.ppp.km)

plot(layered(o7oohrs.ppp.km, win.km), main = "EMS calls between 0700hrs - 0759hrs")

# kernel smoothed intensity
o7oo = density.ppp(o7oohrs.ppp.km)
plot(o7oo)

#convert kernel smoothed intensity to raster
o7oo_r = raster(o7oo)
writeRaster(o7oo_r, "8.tif", overwrite = TRUE)

#convert raster to a dataframe
df_o7oo = as.data.frame(o7oo_r)


#0800hrs
# Load a 0800hrs.shp point feature shapefile
o8oohrs <- st_read("0800hrs.shp")

# convert to planar point pattern
o8oohrs.ppp = as.ppp(o8oohrs)
unitname(o8oohrs.ppp)= c("metre", "metres")
o8oohrs.ppp.km <- rescale(o8oohrs.ppp, 10000, "km")

# observation window from shapefile
o8_mask  <- st_read("Kiambu.shp")
win = as.owin(o8_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")


# observation window from extent
Window(o8oohrs.ppp.km)= win.km
o8oohrs.ppp.km = unmark(o8oohrs.ppp.km)

plot(layered(o8oohrs.ppp.km, win.km), main = "EMS calls between 0800hrs - 0859hrs")

o8oo = density.ppp(o8oohrs.ppp.km)
plot(o8oo)

#convert kernel smoothed intensity to raster
o8oo_r = raster(o8oo)
writeRaster(o8oo_r, "9.tif", overwrite=TRUE)

#convert raster to a dataframe
df_o8oo = as.data.frame(o8oo_r)


#0900hrs
# Load a 0900hrs.shp point feature shapefile
o9oohrs <- st_read("0900hrs.shp")

# convert to planar point pattern
o9oohrs.ppp = as.ppp(o9oohrs)
unitname(o9oohrs.ppp)= c("metre", "metres")
o9oohrs.ppp.km <- rescale(o9oohrs.ppp, 10000, "km")

# observation window from shapefile
o9_mask  <- st_read("Kiambu.shp")
win = as.owin(o9_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")

# observation window from extent
Window(o9oohrs.ppp.km)= win.km
o9oohrs.ppp.km = unmark(o9oohrs.ppp.km)

plot(layered(o9oohrs.ppp.km, win.km), main = "EMS calls between 0900hrs - 0959hrs")

# kernel smoothed intensity
o9oo = density.ppp(o9oohrs.ppp.km)
plot(o9oo)

#convert kernel smoothed intensity to raster
o9oo_r = raster(o9oo)
writeRaster(o9oo_r, "10.tif", overwrite=TRUE)

#convert raster to a dataframe
df_o9oo = as.data.frame(o9oo_r)


#1000hrs
# Load a 1000hrs.shp point feature shapefile
Iooohrs <- st_read("1000hrs.shp")

# convert to planar point pattern
Iooohrs.ppp = as.ppp(Iooohrs)
unitname(Iooohrs.ppp)= c("metre", "metres")
Iooohrs.ppp.km <- rescale(Iooohrs.ppp, 10000, "km")

# observation window from shapefile
Io_mask  <- st_read("Kiambu.shp")
win = as.owin(Io_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")


# observation window from extent
Window(Iooohrs.ppp.km)= win.km
Iooohrs.ppp.km = unmark(Iooohrs.ppp.km)


plot(layered(Iooohrs.ppp.km, win.km), main = "EMS calls between 1000hrs - 1059hrs")

# kernel smoothed intensity
Iooo = density.ppp(Iooohrs.ppp.km)
plot(Iooo)

#convert kernel smoothed intensity to raster
Iooo_r = raster(Iooo)
writeRaster(Iooo_r, "11.tif", overwrite=TRUE)

#convert raster to a dataframe
df_Iooo = as.data.frame(Iooo_r)


#1100hrs
# Load a 1100hrs.shp point feature shapefile
IIoohrs <- st_read("1100hrs.shp")

# convert to planar point pattern
IIoohrs.ppp = as.ppp(IIoohrs)
unitname(IIoohrs.ppp)= c("metre", "metres")
IIoohrs.ppp.km <- rescale(IIoohrs.ppp, 10000, "km")

# observation window from shapefile
II_mask  <- st_read("Kiambu.shp")
win = as.owin(II_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")

# observation window from extent
Window(IIoohrs.ppp.km)= win.km
IIoohrs.ppp.km = unmark(IIoohrs.ppp.km)

plot(layered(IIoohrs.ppp.km, win.km), main = "EMS calls between 1100hrs -1159hrs")

# kernel smoothed intensity
IIoo = density.ppp(IIoohrs.ppp.km)
plot(IIoo)

#convert kernel smoothed intensity to raster
IIoo_r = raster(IIoo)
writeRaster(IIoo_r, "12.tif", overwrite=TRUE)

#convert raster to a dataframe
df_IIoo = as.data.frame(IIoo_r)



#1200hrs
# Load a 1200hrs.shp point feature shapefile
I2OOhrs <- st_read("1200hrs.shp")

# convert to planar point pattern
I2OOhrs.ppp = as.ppp(I2OOhrs)
unitname(I2OOhrs.ppp)= c("metre", "metres")
I2OOhrs.ppp.km <- rescale(I2OOhrs.ppp, 10000, "km")


# observation window from shapefile
I2_mask  <- st_read("Kiambu.shp")
win = as.owin(I2_mask, unitname = "metre")
win.km <- rescale(win, 10000, "km")


# observation window from extent
Window(I2OOhrs.ppp.km)= win.km
I2OOhrs.ppp.km = unmark(I2OOhrs.ppp.km)

plot(layered(I2OOhrs.ppp.km, win.km), main = "EMS calls between 1200hrs - 1259hrs")

# kernel smoothed intensity
I2OO = density.ppp(I2OOhrs.ppp.km)
plot(I2OO)

#convert kernel smoothed intensity to raster
I2hrs_r = raster(I2OO)
writeRaster(I2hrs_r, "13.tif", overwrite = TRUE)

#convert raster to a dataframe
df_I2hrs = as.data.frame(I2hrs_r)


#1300hrs
# Load a 1300hrs.shp point feature shapefile
I3oohrs <- st_read("1300hrs.shp")

# convert to planar point pattern
I3oohrs.ppp = as.ppp(I3oohrs)
unitname(I3oohrs.ppp)= c("metre", "metres")
I3oohrs.ppp.km <- rescale(I3oohrs.ppp, 10000, "km")

# observation window from shapefile
mask  <- st_read("Kiambu.shp")
win = as.owin(mask, unitname = "metre")
win.km <- rescale(win, 10000, "km")

# observation window from extent
Window(I3oohrs.ppp.km)= win.km
I3oohrs.ppp.km = unmark(I3oohrs.ppp.km)

plot(layered(I3oohrs.ppp.km, win.km), main = "EMS calls between 1300hrs - 1359hrs")

# kernel smoothed intensity
I3oo = density.ppp(I3oohrs.ppp.km)
plot(I3oo)

#convert kernel smoothed intensity to raster
I3oo_r = raster(I3oo)
writeRaster(I3oo_r, "14.tif", overwrite = TRUE)

#convert raster to a dataframe
df_I3oo = as.data.frame(I3oo_r)


#1400hrs
# Load a 1400hrs.shp point feature shapefile
I4oohrs <- st_read("1400hrs.shp")

# convert to planar point pattern
I4oohrs.ppp = as.ppp(I4oohrs)
unitname(I4oohrs.ppp)= c("metre", "metres")
I4oohrs.ppp.km <- rescale(I4oohrs.ppp, 10000, "km")

# observation window from shapefile
I4_mask  <- st_read("Kiambu.shp")
win = as.owin(I4_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")


# observation window from extent
Window(I4oohrs.ppp.km)= win.km
I4oohrs.ppp.km = unmark(I4oohrs.ppp.km)

plot(layered(I4oohrs.ppp.km, win.km), main = "EMS calls between 1400hrs - 1459hrs")

I4oo = density.ppp(I4oohrs.ppp.km)
plot(I4oo)

#convert kernel smoothed intensity to raster
I4oo_r = raster(I4oo)
writeRaster(I4oo_r, "15.tif", overwrite=TRUE)

#convert raster to a dataframe
df_I4oo = as.data.frame(I4oo_r)


#1500hrs
# Load a 1500hrs.shp point feature shapefile
I5oohrs <- st_read("1500hrs.shp")

# convert to planar point pattern
I5oohrs.ppp = as.ppp(I5oohrs)
unitname(I5oohrs.ppp)= c("metre", "metres")
I5oohrs.ppp.km <- rescale(I5oohrs.ppp, 10000, "km")

# observation window from shapefile
I5_mask  <- st_read("Kiambu.shp")
win = as.owin(I5_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")

# observation window from extent
Window(I5oohrs.ppp.km)= win.km
I5oohrs.ppp.km = unmark(I5oohrs.ppp.km)

plot(layered(I5oohrs.ppp.km, win.km), main = "EMS calls between 1500hrs - 1559hrs")

# kernel smoothed intensity
I5oo = density.ppp(I5oohrs.ppp.km)
plot(I5oo)

#convert kernel smoothed intensity to raster
I5oo_r = raster(I5oo)
writeRaster(I5oo_r, "16.tif", overwrite=TRUE)

#convert raster to a dataframe
df_I5oo = as.data.frame(I5oo_r)


#1600hrs
# Load a 1600hrs.shp point feature shapefile
I6oohrs <- st_read("1600hrs.shp")

# convert to planar point pattern
I6oohrs.ppp = as.ppp(I6oohrs)
unitname(I6oohrs.ppp)= c("metre", "metres")
I6oohrs.ppp.km <- rescale(I6oohrs.ppp, 10000, "km")

# observation window from shapefile
I6_mask  <- st_read("Kiambu.shp")
win = as.owin(I6_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")


# observation window from extent
Window(I6oohrs.ppp.km)= win.km
I6oohrs.ppp.km = unmark(I6oohrs.ppp.km)


plot(layered(I6oohrs.ppp.km, win.km), main = "EMS calls between 1600hrs - 1659hrs")

# kernel smoothed intensity
I6oo = density.ppp(I6oohrs.ppp.km)
plot(I6oo)

#convert kernel smoothed intensity to raster
I6oo_r = raster(I6oo)
writeRaster(I6oo_r, "17.tif", overwrite=TRUE)

#convert raster to a dataframe
df_I6oo = as.data.frame(I6oo_r)


#1700hrs
# Load a 1700.shp point feature shapefile
I7oohrs <- st_read("1700hrs.shp")

# convert to planar point pattern
I7oohrs.ppp = as.ppp(I7oohrs)
unitname(I7oohrs.ppp)= c("metre", "metres")
I7oohrs.ppp.km <- rescale(I7oohrs.ppp, 10000, "km")

# observation window from shapefile
I7_mask  <- st_read("Kiambu.shp")
win = as.owin(I7_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")

# observation window from extent
Window(I7oohrs.ppp.km)= win.km
I7oohrs.ppp.km = unmark(I7oohrs.ppp.km)

plot(layered(I7oohrs.ppp.km, win.km), main = "EMS calls between 1700hrs -1759hrs")

# kernel smoothed intensity
I7oo = density.ppp(I7oohrs.ppp.km)
plot(I7oo)

#convert kernel smoothed intensity to raster
I7oo_r = raster(I7oo)
writeRaster(I7oo_r, "18.tif", overwrite=TRUE)

#convert raster to a dataframe
df_I7oo = as.data.frame(I7oo_r)


#I800hrs
# Load a 1800hrs.shp point feature shapefile
I8oohrs <- st_read("1800hrs.shp")

# convert to planar point pattern
I8oohrs.ppp = as.ppp(I8oohrs)
unitname(I8oohrs.ppp)= c("metre", "metres")
I8oohrs.ppp.km <- rescale(I8oohrs.ppp, 10000, "km")


# observation window from shapefile
I8_mask  <- st_read("Kiambu.shp")
win = as.owin(I8_mask, unitname = "metre")
win.km <- rescale(win, 10000, "km")


# observation window from extent
Window(I8oohrs.ppp.km)= win.km
I8oohrs.ppp.km = unmark(I8oohrs.ppp.km)

plot(layered(I8oohrs.ppp.km, win.km), main = "EMS calls between 1800hrs - 1859hrs")

# kernel smoothed intensity
I8oo = density.ppp(I8oohrs.ppp.km)
plot(I8oo)

#convert kernel smoothed intensity to raster
I800_r = raster(I8oo)
writeRaster(I800_r, "19.tif", overwrite = TRUE)

#convert raster to a dataframe
df_I8hrs = as.data.frame(I800_r)


#1900hrs
# Load a 1900hrs.shp point feature shapefile
I9oohrs <- st_read("1900hrs.shp")

# convert to planar point pattern
I9oohrs.ppp = as.ppp(I9oohrs)
unitname(I9oohrs.ppp)= c("metre", "metres")
I9oohrs.ppp.km <- rescale(I9oohrs.ppp, 10000, "km")

# observation window from shapefile
mask  <- st_read("Kiambu.shp")
win = as.owin(mask, unitname = "metre")
win.km <- rescale(win, 10000, "km")

# observation window from extent
Window(I9oohrs.ppp.km)= win.km
I9oohrs.ppp.km = unmark(I9oohrs.ppp.km)

plot(layered(I9oohrs.ppp.km, win.km), main = "EMS calls between 1900hrs - 1959hrs")

# kernel smoothed intensity
I9oo = density.ppp(I9oohrs.ppp.km)
plot(I9oo)

#convert kernel smoothed intensity to raster
I9oo_r = raster(I9oo)
writeRaster(I9oo_r, "20.tif", overwrite = TRUE)

#convert raster to a dataframe
df_I9oo = as.data.frame(I9oo_r)


#2000hrs
# Load a 2000hrs.shp point feature shapefile
O2ooohrs <- st_read("2000hrs.shp")

# convert to planar point pattern
O2ooohrs.ppp = as.ppp(O2ooohrs)
unitname(O2ooohrs.ppp)= c("metre", "metres")
O20oohrs.ppp.km <- rescale(O2ooohrs.ppp, 10000, "km")

# observation window from shapefile
o2o_mask  <- st_read("Kiambu.shp")
win = as.owin(o2o_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")


# observation window from extent
Window(O20oohrs.ppp.km)= win.km
O20oohrs.ppp.km = unmark(O20oohrs.ppp.km)

plot(layered(O20oohrs.ppp.km, win.km), main = "EMS calls between 2000hrs - 2059hrs")

o2ooo = density.ppp(O20oohrs.ppp.km)
plot(o2ooo)

#convert kernel smoothed intensity to raster
o2ooo_r = raster(o2ooo)
writeRaster(o2ooo_r, "21.tif", overwrite=TRUE)

#convert raster to a dataframe
df_o2ooo = as.data.frame(o2ooo_r)


#2100hrs
# Load a 2100hrs.shp point feature shapefile
o21oohrs <- st_read("2100hrs.shp")

# convert to planar point pattern
o21oohrs.ppp = as.ppp(o21oohrs)
unitname(o21oohrs.ppp)= c("metre", "metres")
o21oohrs.ppp.km <- rescale(o21oohrs.ppp, 10000, "km")

# observation window from shapefile
o21_mask  <- st_read("Kiambu.shp")
win = as.owin(o21_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")

# observation window from extent
Window(o21oohrs.ppp.km)= win.km
o21oohrs.ppp.km = unmark(o21oohrs.ppp.km)

plot(layered(o21oohrs.ppp.km, win.km), main = "EMS calls between 2100hrs - 2159hrs")

# kernel smoothed intensity
o21oo = density.ppp(o21oohrs.ppp.km)
plot(o21oo)

#convert kernel smoothed intensity to raster
o21oo_r = raster(o21oo)
writeRaster(o21oo_r, "22.tif", overwrite=TRUE)

#convert raster to a dataframe
df_o21oo = as.data.frame(o21oo_r)


#2200hrs
# Load a 2200hrs.shp point feature shapefile
o22oohrs <- st_read("2200hrs.shp")

# convert to planar point pattern
o22oohrs.ppp = as.ppp(o22oohrs)
unitname(o22oohrs.ppp)= c("metre", "metres")
o22oohrs.ppp.km <- rescale(o22oohrs.ppp, 10000, "km")

# observation window from shapefile
o22_mask  <- st_read("Kiambu.shp")
win = as.owin(o22_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")


# observation window from extent
Window(o22oohrs.ppp.km)= win.km
o22oohrs.ppp.km = unmark(o22oohrs.ppp.km)


plot(layered(o22oohrs.ppp.km, win.km), main = "EMS calls between 2200hrs - 2259hrs")

# kernel smoothed intensity
o22oo = density.ppp(o22oohrs.ppp.km)
plot(o22oo)

#convert kernel smoothed intensity to raster
o22oo_r = raster(o22oo)
writeRaster(o22oo_r, "23.tif", overwrite=TRUE)

#convert raster to a dataframe
df_o22oo = as.data.frame(o22oo_r)


#2300hrs
# Load a 2300hrs.shp point feature shapefile
o23oohrs <- st_read("2300hrs.shp")

# convert to planar point pattern
o23oohrs.ppp = as.ppp(o23oohrs)
unitname(o23oohrs.ppp)= c("metre", "metres")
o23oohrs.ppp.km <- rescale(o23oohrs.ppp, 10000, "km")

# observation window from shapefile
o23_mask  <- st_read("Kiambu.shp")
win = as.owin(o23_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")

# observation window from extent
Window(o23oohrs.ppp.km)= win.km
o23oohrs.ppp.km = unmark(o23oohrs.ppp.km)

plot(layered(o23oohrs.ppp.km, win.km), main = "EMS calls between 2300hrs -2359hrs")

# kernel smoothed intensity
o23oo = density.ppp(o23oohrs.ppp.km)
plot(o23oo)

#convert kernel smoothed intensity to raster
o23oo_r = raster(o23oo)
writeRaster(o23oo_r, "24.tif", overwrite=TRUE)

#convert raster to a dataframe
df_o23oo = as.data.frame(o23oo_r)
