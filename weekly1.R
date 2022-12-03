setwd("H:\\4.2\\PROJECT\\Week")

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
library(tmaptools)


#Monday
# Load a Monday.shp point feature shapefile
M <- st_read("Monday.shp")

# convert to planar point pattern
M.ppp = as.ppp(M)
unitname(M.ppp)= c("metre", "metres")
M.ppp.km <- rescale(M.ppp, 10000, "km")

# observation window from shapefile
M_mask  <- st_read("Kiambu.shp")
win = as.owin(M_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")

# observation window from extent
Window(M.ppp.km)= win.km
M.ppp.km = unmark(M.ppp.km)

plot(layered(M.ppp.km, win.km), main = "EMS calls on Tuesday")

# kernel smoothed intensity
Mon = density.ppp(M.ppp.km)
plot(Mon)

#convert kernel smoothed intensity to raster
library(raster)
Mon_r = raster(Mon)
writeRaster(Mon_r, "1.tif", overwrite=TRUE)

#convert raster to a dataframe
df_Mon = as.data.frame(Mon_r)


#Tuesday
# Load a tuesday.shp point feature shapefile
pts <- st_read("Tuesday.shp")

# convert to planar point pattern
pts.ppp = as.ppp(pts)
unitname(pts.ppp)= c("metre", "metres")
pts.ppp.km <- rescale(pts.ppp, 10000, "km")

# observation window from shapefile
mask  <- st_read("Kiambu.shp")
win = as.owin(mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")

# observation window from extent
Window(pts.ppp.km)= win.km
pts.ppp.km = unmark(pts.ppp.km)

plot(layered(pts.ppp.km, win.km), main = "EMS calls on Tuesday")

# kernel smoothed intensity
Tue = density.ppp(pts.ppp.km)
plot(Tue)

#convert kernel smoothed intensity to raster
library(raster)
Tue_r = raster(Tue)
writeRaster(Tue_r, "2.tif", overwrite=TRUE)

#convert raster to a dataframe
df_Tue = as.data.frame(Tue_r)


#Wednesday
# Load a Wednesday.shp point feature shapefile
w <- st_read("Wednesday.shp")

# convert to planar point pattern
w.ppp = as.ppp(w)
unitname(w.ppp)= c("metre", "metres")
w.ppp.km <- rescale(w.ppp, 10000, "km")

# observation window from shapefile
w_mask  <- st_read("Kiambu.shp")
win = as.owin(w_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")

# observation window from extent
Window(w.ppp.km)= win.km
w.ppp.km = unmark(w.ppp.km)

plot(layered(w.ppp.km, win.km), main = "EMS calls on Wednesday")

#kernel intensity smoothing
wed = density.ppp(w.ppp.km)
plot(wed)

#convert kernel smoothed intensity to raster
library(raster)
wed_r = raster(wed)
writeRaster(wed_r, "3.tif", overwrite=TRUE)

#convert raster to a dataframe
df_wed = as.data.frame(wed_r)


#Thursday
# Load a Thursday.shp point feature shapefile
T <- st_read("Thursday.shp")

# convert to planar point pattern
T.ppp = as.ppp(T)
unitname(T.ppp)= c("metre", "metres")
T.ppp.km <- rescale(T.ppp, 10000, "km")

# observation window from shapefile
T_mask  <- st_read("Kiambu.shp")
win = as.owin(T_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")

# observation window from extent
Window(T.ppp.km)= win.km
T.ppp.km = unmark(T.ppp.km)

plot(layered(T.ppp.km, win.km), main = "EMS calls on Thursday")

# kernel smoothed intensity
thur = density.ppp(T.ppp.km)
plot(thur)

#convert kernel smoothed intensity to raster
library(raster)
thur_r = raster(thur)
writeRaster(thur_r, "4.tif", overwrite=TRUE)

#convert raster to a dataframe
df_thur = as.data.frame(thur_r)


#Friday
# Load a Friday.shp point feature shapefile
F <- st_read("Friday.shp")

# convert to planar point pattern
F.ppp = as.ppp(F)
unitname(F.ppp)= c("metre", "metres")
F.ppp.km <- rescale(F.ppp, 10000, "km")

# observation window from shapefile
F_mask  <- st_read("Kiambu.shp")
win = as.owin(F_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")

# observation window from extent
Window(F.ppp.km)= win.km
F.ppp.km = unmark(F.ppp.km)


plot(layered(F.ppp.km, win.km), main = "EMS calls on Friday")

# kernel smoothed intensity
Fri = density.ppp(F.ppp.km)
plot(Fri)

#convert kernel smoothed intensity to raster
library(raster)
Fri_r = raster(Fri)
writeRaster(Fri_r, "5.tif", overwrite=TRUE)

#convert raster to a dataframe
df_fri = as.data.frame(Fri_r)


#Saturday
# Load a Saturday.shp point feature shapefile
s <- st_read("Saturday.shp")

# convert to planar point pattern
s.ppp = as.ppp(s)
unitname(s.ppp)= c("metre", "metres")
s.ppp.km <- rescale(s.ppp, 10000, "km")

# observation window from shapefile
s_mask  <- st_read("Kiambu.shp")
win = as.owin(s_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")

# observation window from extent
Window(s.ppp.km)= win.km
s.ppp.km = unmark(s.ppp.km)

plot(layered(s.ppp.km, win.km), main = "EMS calls on Saturday")

# kernel smoothed intensity
Sat = density.ppp(s.ppp.km)
plot(Sat)

#convert kernel smoothed intensity to raster
library(raster)
Sat_r = raster(Sat)
writeRaster(Sat_r, "6.tif", overwrite=TRUE)

#convert raster to a dataframe
df_sat = as.data.frame(Sat_r)


#Sunday
# Load a Sunday.shp point feature shapefile
su <- st_read("Sunday.shp")

# convert to planar point pattern
su.ppp = as.ppp(su)
unitname(su.ppp)= c("metre", "metres")
su.ppp.km <- rescale(su.ppp, 10000, "km")

# observation window from shapefile
su_mask  <- st_read("Kiambu.shp")
win = as.owin(su_mask, unitname = "metres")
win.km <- rescale(win, 10000, "km")

# observation window from extent
Window(su.ppp.km)= win.km
su.ppp.km = unmark(su.ppp.km)

plot(layered(su.ppp.km, win.km), main = "EMS calls on Sunday")

# kernel smoothed intensity
Sun = density.ppp(su.ppp.km)
plot(Sun)


#convert kernel smoothed intensity to raster
library(raster)
Sun_r = raster(Sun)
writeRaster(Sun_r, "7.tif", overwrite=TRUE)

#convert raster to a dataframe
df_sun = as.data.frame(Sun_r)

plot(Sun_r)



