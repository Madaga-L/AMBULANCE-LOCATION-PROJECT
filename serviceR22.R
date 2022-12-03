setwd("H:/4.2/PROJECT/work_R")

library(sf)
library(dplyr)
library(tmap)
#install.packages("data.table")
library(data.table)
library(hereR)

#load ambulance stations shapefile
Amb_stations1 <- read_sf("Ambulance_stations.shp")
Amb_stations1

## Read in the sub county Boundaries 
rc <- st_read("H:/4.2/PROJECT/work_R/Sub_counties.gpkg")
rc

## Setup plotting
tmap_mode("view")

## Map sub county Polygons for LCR
tm_shape(rc) +
  tm_fill(col = "orange") 

## Extract centroids
rc_cent <- st_centroid(rc)

## Map the centroids - note: tm_dots() is used as the object rc_cent contains point data (Retail Centre centroids)
tm_shape(rc_cent) +
  tm_dots(col = "orange")

#Catchments - Drive-Time Catchments
## Set API key
set_key("BY2gpBK3mFu_O0XZDQ7DR50I6OKzRD60LZaY8dMrz1Q")

## Extract Ambsation - the first in our dataset
rc_a <- rc_cent[1, ]

## Extract the 15-minute driving catchment
iso_a <- isoline(rc_a, range = (15 * 60), range_type = "time", transport_mode = "car")

## Map the drive-time catchment for the first Ambulance station
tm_shape(iso_a) +
  tm_fill(col = "orange", alpha = 0.5) +
  tm_shape(rc_a) +
  tm_dots(col = "orange")

## Extract the 15-minute catchment for every Ambulance station in each sub county
iso <- isoline(Amb_stations1, range = (14 * 60), range_type = "time", transport_mode = "car", aggregate = FALSE)

## Map the 15-minute drive-time catchments for the Ambulance stations
tm_shape(iso) +
  tm_fill(col = "red", alpha = 0.3) +
  tm_shape(Amb_stations1) +
  tm_dots(col = "green")

#save shapefile
st_write(iso, "14minserviceregions.shp")



