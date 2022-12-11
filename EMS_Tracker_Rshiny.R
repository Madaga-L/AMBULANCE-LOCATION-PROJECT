setwd("H:\\4.2\\PROJECT\\objective 3\\rshine")
# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(leaflet)
library(sf)
library(dplyr)
library(sp)
library(plotly)
library(htmltools)
library(raster)
library(leaflet.extras)

#Data csvs
Data_EMS = read.csv("H:/4.2/PROJECT/objective 3/rd_eoc_data_Arranged.csv") 
Montly_cases = read.csv("H:/4.2/PROJECT/objective 3/March.csv")
Weekly_cases = read.csv("H:/4.2/PROJECT/objective 3/Monday.csv")
hourly_cases = read.csv("H:/4.2/PROJECT/objective 3/0600hrs.csv")
Existing = read.csv("H:/4.2/PROJECT/objective 3/geocoded_Amb stations.csv")
optimal = read.csv("H:/4.2/PROJECT/objective 3/Optimal_sites.csv")
RTAs = read.csv("Rd_Accidents.csv")
sub_counties = read.csv("H:/4.2/PROJECT/objective 3/Kiambu_sub_counties_UTM.csv")

#Data
#shapefiles
sub_c <- read_sf("H:/4.2/PROJECT/objective 3/rshine/Kiambu_sub_counties_wgs.shp")

#ALL EMS Requests data
Data_all <- read_sf("RD_EoCArr.shp")

#Road accidents in Kimabu
Accidents <- read_sf("H:/4.2/PROJECT/objective 3/rshine/Rd_Accidents.shp")

#load Ambulance stations point shapefile
Amb_stations1 <- read_sf("H:/4.2/PROJECT/objective 3/rshine/Ambulance_stations.shp")
Amb_stations1

#optimal_sites
optimal_sites <- read_sf("optimal_sites.shp")
optimal_sites

# set mapping colour for each plot
Accidents_col = "#cc4c02"
county_col = "#662506"
All_data_col = "#045a8d"
optimal_col = "#4d004b"
existing_col = "#016c59"



bins <- c(312.0, 624.0, 936.0, 1248.0, 1560.0, 1872.0, 2184.0, 2496.0, 3432.0)
pal <- colorBin("Oranges", domain = sub_c$pop_densit, bins = bins)



leaflet(sub_c) %>% 
  addTiles() %>%
  addPolygons(stroke = TRUE,
              color = "white",
              opacity = 1,
              group="Population density",
              weight = 1,
              fillOpacity = 0.4,
              fillColor = pal(sub_c$pop_densit),
              popup = ~glue::glue("<b>{SCouName}</b><br>{total_pop}")) %>%
  addCircleMarkers(data = Data_all,
                   lat = ~lat, lng=~lon,
                   radius = ~3,
                   fillOpacity = 0.4,
                   color = "red",
                   group = "EMS Requests",
                   label = sprintf("<strong>%s</strong><br/>DIAGNOSIS: %s<br/>Day: %s<br/>Request Time: %s", Data_all$addresses, Data_all$DIAGNOSIS_, Data_all$DAY, Data_all$REQUEST_TI) %>% lapply(htmltools::HTML),
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px", "color" = All_data_col),
                     textsize = "15px", direction = "auto")) %>%
  addCircleMarkers(data = Accidents,
                   lat = ~lat, lng=~lon,
                   radius = ~3,
                   fillOpacity = 0.2,
                   color = "blue",
                   group = "Road Traffic Accidents",
                   label = sprintf("<strong>%s</strong><br/>Road: %s<br/>Day: %s<br/>Time: %s", Accidents$addresses, Accidents$ROAD, Accidents$DAY, Accidents$TIME_24_HO) %>% lapply(htmltools::HTML),
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px", "color" = Accidents_col),
                     textsize = "15px", direction = "auto")) %>%
  addCircleMarkers(data = Amb_stations1,
                   lat = ~lat, lng=~lon,
                   radius = ~5,
                   fillOpacity = 0.8,
                   color = "orange",
                   group = "Existing Facilities",
                   label = sprintf("<strong>%s</strong><br/>Address: %s<br/>Sub county: %s<br/>Number of Ambulances: %s", Amb_stations1$addresses, Amb_stations1$geoAddress, Amb_stations1$SCouName, Amb_stations1$NumberofAm) %>% lapply(htmltools::HTML),
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px", "color" = existing_col),
                     textsize = "15px", direction = "auto")) %>%
  addCircleMarkers(data = optimal_sites,
                   lat = ~latitude, lng=~longitude,
                   radius = ~5,
                   fillOpacity = 0.8,
                   color = "brown",
                   group = "Optimal Sites",
                   label = sprintf("<strong>%s</strong><br/>Station ID: %s<br/>Number of Ambulances: %s", optimal_sites$Site_name, optimal_sites$StationID, optimal_sites$NumberOfAm) %>% lapply(htmltools::HTML),
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px", "color" = optimal_col),
                     textsize = "15px", direction = "auto")) %>%
  addLayersControl(
    position = "topright",
    overlayGroups = c("EMS Requests", "Road Traffic Accidents", "Existing Facilities", "Optimal Sites"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("EMS Requests", "Road Traffic Accidents", "Existing Facilities", "Optimal Sites")) %>%
  fitBounds(~-100,-60,~60,70) %>%
  addLegend("topright", pal = pal, values = ~sub_c$pop_densit,
            title = "<small>persons/sq.km>")


Cumulative = read.csv("H:/4.2/PROJECT/objective 3/rshine/cumulative.csv") 

cumulative_plot = function(cv_aggregated, plot_date) {
  plot_df = subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df, aes(x = date, y = calls)) + geom_line(color = All_data_col) + geom_point(size = 1, alpha = 0.8, color = All_data_col) +
    ylab("Cumulative calls") +  xlab("Date") + theme_bw() + 
    scale_colour_manual(values=c(All_data_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1; paste0(trans, "")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# function to plot new COVID cases by date
new_cases_plot = function(cv_aggregated, plot_date) {
  plot_df_new = subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df_new, aes(x = date, y = calls)) + 
    geom_bar(position="stack", stat="identity", fill = Accidents_col) + 
    ylab("Cumulative calls") + xlab("Date") + theme_bw() + 
    scale_colour_manual(values=c(All_data_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1; paste0(trans, "")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

### DATA PROCESSING: EMS Data ###
# extract dates from Cumulative
if (any(grepl("/", Cumulative$date))) { 
  Cumulative$date = format(as.Date(Cumulative$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { Cumulative$date = as.Date(Cumulative$date, format="%Y-%m-%d") }
Cumulative$date = as.Date(Cumulative$date)
cv_min_date = as.Date(min(Cumulative$date),"%Y-%m-%d")
current_date = as.Date(max(Cumulative$date),"%Y-%m-%d")
cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")

# sum EMS call counts by date
cv_aggregated = aggregate(Cumulative$calls, by=list(Category=Cumulative$date), FUN=sum)
names(cv_aggregated) = c("date", "calls") 


##### RASTER SPATIO TEMPORAL PLOTS
####MONTHLY RASTERS
# Read the raster file using the raster() function
mar <- raster("H:/4.2/PROJECT/objective 3/monthly/1mar.tif")
Apr <- raster("H:/4.2/PROJECT/objective 3/monthly/2Apr.tif")
May <- raster("H:/4.2/PROJECT/objective 3/monthly/3may.tif")
June <- raster("H:/4.2/PROJECT/objective 3/monthly/4june.tif")
July <- raster("H:/4.2/PROJECT/objective 3/monthly/5july.tif")
Aug <- raster("H:/4.2/PROJECT/objective 3/monthly/6Aug.tif")

#raster for plotting the legend
March_leg <- raster("H:/4.2/PROJECT/objective 3/monthly/March_leg.tif")
#pal for legend
pal_leg <- colorNumeric(c("RdYlBu"), values(March_leg),
                        na.color = "transparent")

pal0 <- colorNumeric(c("RdYlBu"), values(mar),
                     na.color = "transparent")
pal1 <- colorNumeric(c("RdYlBu"), values(Apr),
                     na.color = "transparent")
pal2 <- colorNumeric(c("RdYlBu"), values(May ),
                     na.color = "transparent")
pal3 <- colorNumeric(c("RdYlBu"), values(June),
                     na.color = "transparent")
pal4 <- colorNumeric(c("RdYlBu"), values(July),
                     na.color = "transparent")
pal5 <- colorNumeric(c("RdYlBu"), values(Aug ),
                     na.color = "transparent")



leaflet() %>%
  addTiles() %>%
  addRasterImage(mar, colors = pal0, opacity = 0.4, group = "March") %>%
  addRasterImage(Apr, colors = pal1, opacity = 0.4, group = "April") %>%
  addRasterImage(May, colors = pal2, opacity = 0.4, group = "May") %>%
  addRasterImage(June, colors = pal3, opacity = 0.4, group = "June") %>%
  addRasterImage(July, colors = pal4, opacity = 0.4, group = "July") %>%
  addRasterImage(Aug, colors = pal5, opacity = 0.4, group = "August") %>%
  addLayersControl(
    position = "topright",
    overlayGroups = c("March", "April", "May", "June", "July", "August"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("March", "April", "May", "June", "July", "August")) %>%
  addLegend(pal = pal_leg, values = values(March_leg),
            opacity = 0.4,
            title = "calls/10sq.km")

#WEEKLY RASTERS
# Read the raster file using the raster() function
mon <- raster("H:/4.2/PROJECT/objective 3/weekly/1mon.tif")
tue <- raster("H:/4.2/PROJECT/objective 3/weekly/2tue.tif")
wed <- raster("H:/4.2/PROJECT/objective 3/weekly/3wed.tif")
thur <- raster("H:/4.2/PROJECT/objective 3/weekly/4thur.tif")
fri <- raster("H:/4.2/PROJECT/objective 3/weekly/5fri.tif")
sat <- raster("H:/4.2/PROJECT/objective 3/weekly/6sat.tif")
sun <- raster("H:/4.2/PROJECT/objective 3/weekly/7sun.tif")

#raster for plotting legend
mon_leg <- raster("H:/4.2/PROJECT/objective 3/weekly/mon_leg.tif")
#pal for legend
pal_mon <- colorNumeric(c("RdYlBu"), values(mon_leg),
                        na.color = "transparent")

library(leaflet)
pal6 <- colorNumeric(c("RdYlBu"), values(mon),
                     na.color = "transparent")
pal7 <- colorNumeric(c("RdYlBu"), values(tue),
                     na.color = "transparent")
pal8 <- colorNumeric(c("RdYlBu"), values(wed),
                     na.color = "transparent")
pal9 <- colorNumeric(c("RdYlBu"), values(thur),
                     na.color = "transparent")
pal10 <- colorNumeric(c("RdYlBu"), values(fri),
                      na.color = "transparent")
pal11 <- colorNumeric(c("RdYlBu"), values(sat),
                      na.color = "transparent")
pal12 <- colorNumeric(c("RdYlBu"), values(sun),
                      na.color = "transparent")

leaflet() %>%
  addTiles() %>%
  addRasterImage(mon, colors = pal6, opacity = 0.4, group = "Monday") %>%
  addRasterImage(tue, colors = pal7, opacity = 0.4, group = "Tuesday") %>%
  addRasterImage(wed, colors = pal8, opacity = 0.4, group = "Wednesday") %>%
  addRasterImage(thur, colors = pal9, opacity = 0.4, group = "Thursday") %>%
  addRasterImage(fri, colors = pal10, opacity = 0.4, group = "Friday") %>%
  addRasterImage(sat, colors = pal11, opacity = 0.4, group = "Saturday") %>%
  addRasterImage(sun, colors = pal12, opacity = 0.4, group = "Sunday") %>%
  addLayersControl(
    position = "topright",
    overlayGroups = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) %>%
  addLegend(pal = pal_mon, values = values(mon_leg), 
            title = "calls/10sq.km")

#####HOURLY RASRERS
# Read the raster file using the raster() function
oohrs <- raster("H:/4.2/PROJECT/objective 3/hourly/1oohrs.tif")
o1hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/2o1hrs.tif")
o2hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/3o2hrs.tif")
o3hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/4o3hrs.tif")
o4hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/5o4hrs.tif")
o5hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/6o5hrs.tif")
o6hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/7o6hrs.tif")
o7hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/8o7hrs.tif")
o8hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/9o8hrs.tif")
o9hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/10o9hrs.tif")
o10hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/11i0hrs.tif")
o11hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/12iihrs.tif")
o12hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/13i2hrs.tif")
o13hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/14i3hrs.tif")
o14hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/15i4hrs.tif")
o15hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/16i5hrs.tif")
o16hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/17i6hrs.tif")
o17hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/18i7hrs.tif")
o18hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/19i8hrs.tif")
o19hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/20i9hrs.tif")
o20hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/21o8pm.tif")
o21hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/22o9pm.tif")
o22hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/23i0pm.tif")
o23hrs <- raster("H:/4.2/PROJECT/objective 3/hourly/24iipm.tif")

##raster for legend
hourly_leg <- raster("H:/4.2/PROJECT/objective 3/hourly/hourly_leg.tif")
#pal for legend
hourly_pal <- colorNumeric(c("RdYlBu"), values(hourly_leg),
                           na.color = "transparent")


library(leaflet)
pal13 <- colorNumeric(c("RdYlBu"), values(oohrs),
                      na.color = "transparent")
pal14 <- colorNumeric(c("RdYlBu"), values(o1hrs),
                      na.color = "transparent")
pal15 <- colorNumeric(c("RdYlBu"), values(o2hrs),
                      na.color = "transparent")
pal16 <- colorNumeric(c("RdYlBu"), values(o3hrs),
                      na.color = "transparent")
pal17 <- colorNumeric(c("RdYlBu"), values(o4hrs),
                      na.color = "transparent")
pal18 <- colorNumeric(c("RdYlBu"), values(o5hrs),
                      na.color = "transparent")
pal19 <- colorNumeric(c("RdYlBu"), values(o6hrs),
                      na.color = "transparent")
pal20 <- colorNumeric(c("RdYlBu"), values(o7hrs),
                      na.color = "transparent")
pal21 <- colorNumeric(c("RdYlBu"), values(o8hrs),
                      na.color = "transparent")
pal22 <- colorNumeric(c("RdYlBu"), values(o9hrs),
                      na.color = "transparent")
pal23 <- colorNumeric(c("RdYlBu"), values(o10hrs),
                      na.color = "transparent")
pal24 <- colorNumeric(c("RdYlBu"), values(o11hrs),
                      na.color = "transparent")
pal25 <- colorNumeric(c("RdYlBu"), values(o12hrs),
                      na.color = "transparent")
pal26 <- colorNumeric(c("RdYlBu"), values(o13hrs),
                      na.color = "transparent")
pal27 <- colorNumeric(c("RdYlBu"), values(o14hrs),
                      na.color = "transparent")
pal28 <- colorNumeric(c("RdYlBu"), values(o15hrs),
                      na.color = "transparent")
pal29 <- colorNumeric(c("RdYlBu"), values(o16hrs),
                      na.color = "transparent")
pal30 <- colorNumeric(c("RdYlBu"), values(o17hrs),
                      na.color = "transparent")
pal31 <- colorNumeric(c("RdYlBu"), values(o18hrs),
                      na.color = "transparent")
pal32 <- colorNumeric(c("RdYlBu"), values(o19hrs),
                      na.color = "transparent")
pal33 <- colorNumeric(c("RdYlBu"), values(o20hrs),
                      na.color = "transparent")
pal34 <- colorNumeric(c("RdYlBu"), values(o21hrs),
                      na.color = "transparent")
pal35 <- colorNumeric(c("RdYlBu"), values(o22hrs),
                      na.color = "transparent")
pal36 <- colorNumeric(c("RdYlBu"), values(o23hrs),
                      na.color = "transparent")
leaflet() %>%
  addTiles() %>%
  addRasterImage(oohrs, colors = pal13, opacity = 0.5, group = "0000hrs") %>%
  addRasterImage(o1hrs, colors = pal14, opacity = 0.5, group = "0100hrs") %>%
  addRasterImage(o2hrs, colors = pal15, opacity = 0.5, group = "0200hrs") %>%
  addRasterImage(o3hrs, colors = pal16, opacity = 0.5, group = "0300hrs") %>%
  addRasterImage(o4hrs, colors = pal17, opacity = 0.5, group = "0400hrs") %>%
  addRasterImage(o5hrs, colors = pal18, opacity = 0.5, group = "0500hrs") %>%
  addRasterImage(o6hrs, colors = pal19, opacity = 0.5, group = "0600hrs") %>%
  addRasterImage(o7hrs, colors = pal20, opacity = 0.5, group = "0700hrs") %>%
  addRasterImage(o8hrs, colors = pal21, opacity = 0.5, group = "0800hrs") %>%
  addRasterImage(o9hrs, colors = pal22, opacity = 0.5, group = "0900hrs") %>%
  addRasterImage(o10hrs, colors = pal23, opacity = 0.5, group = "1000hrs") %>%
  addRasterImage(o11hrs, colors = pal24, opacity = 0.5, group = "1100hrs") %>%
  addRasterImage(o12hrs, colors = pal25, opacity = 0.5, group = "1200hrs") %>%
  addRasterImage(o13hrs, colors = pal26, opacity = 0.5, group = "1300hrs") %>%
  addRasterImage(o14hrs, colors = pal27, opacity = 0.5, group = "1400hrs") %>%
  addRasterImage(o15hrs, colors = pal28, opacity = 0.5, group = "1500hrs") %>%
  addRasterImage(o16hrs, colors = pal29, opacity = 0.5, group = "1600hrs") %>%
  addRasterImage(o17hrs, colors = pal30, opacity = 0.5, group = "1700hrs") %>%
  addRasterImage(o18hrs, colors = pal31, opacity = 0.5, group = "1800hrs") %>%
  addRasterImage(o19hrs, colors = pal32, opacity = 0.5, group = "1900hrs") %>%
  addRasterImage(o20hrs, colors = pal33, opacity = 0.5, group = "2000hrs") %>%
  addRasterImage(o21hrs, colors = pal34, opacity = 0.5, group = "2100hrs") %>%
  addRasterImage(o22hrs, colors = pal35, opacity = 0.5, group = "2200hrs") %>%
  addRasterImage(o23hrs, colors = pal36, opacity = 0.5, group = "2300hrs") %>%
  
  addLayersControl(
    position = "topright",
    overlayGroups = c("0000hrs", "0100hrs", "0200hrs", "0300hrs", "0400hrs", "0500hrs", "0600hrs", "0700hrs", "0800hrs", "0900hrs", "1000hrs", "1100hrs", "1200hrs", "1300hrs", "1400hrs", "1500hrs", "1600hrs", "1700hrs", "1800hrs", "1900hrs", "2000hrs", "2100hrs", "2200hrs", "2300hrs"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("0000hrs", "0100hrs", "0200hrs", "0300hrs", "0400hrs", "0500hrs", "0600hrs", "0700hrs", "0800hrs", "0900hrs", "1000hrs", "1100hrs", "1200hrs", "1300hrs", "1400hrs", "1500hrs", "1600hrs", "1700hrs", "1800hrs", "1900hrs", "2000hrs", "2100hrs", "2200hrs", "2300hrs")) %>%
  addLegend("topleft", pal = hourly_pal, values = values(hourly_leg), 
            title = "calls/10sq.km")

### SHINY UI ###
ui <- bootstrapPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">EMS tracker</a>'), id="nav",
             windowTitle = "EMS tracker",
             
             #EMS 
             tabPanel("EMS mapper",
                      div(class="outer",
                          
                          leafletOutput("mymap", width="100%",height="1000px"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        span(tags$i(h6("Calls for EMS are subject to significant variation in different time periods.")), style="color:#045a8d"),
                                        h3(textOutput("reactive_case_count"), align = "right"),
                                        h6(textOutput("clean_date_reactive"), align = "right"),
                                        plotlyOutput("epi_curve", height="130px", width="100%"),
                                        plotlyOutput("cumulative_plot", height="130px", width="100%"),
                                        span(("Incase of an Emergency, please call 0711 264000."),align = "left", style = "font-size:80%"),#tags$br(),
                                        span(("Kiambu Emergency Operations Center."),align = "left", style = "font-size:80%"),
                                        
                                        
                                        
                                        sliderTextInput("plot_date",
                                                        label = h5("Select mapping date"),
                                                        choices = format(unique(Cumulative$date), "%d %b %y"),
                                                        selected = format(current_date, "%d %b %y"),
                                                        grid = FALSE,
                                                        animate=animationOptions(interval = 3000, loop = FALSE))
                          ),
                          
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://kiambu.go.ke/health-services/', tags$img(src='lshtm_dark.png',height='40',width='80'))),
                          
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                        actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                                     onclick = sprintf("window.open('%s')", 
                                                                       "https://twitter.com/intent/tweet?text=%20@LSHTM_Vaccines%20outbreak%20mapper&url=https://bit.ly/2uBvnds&hashtags=EmergencyMedicalSevices")))
                          
                          
                      )
             ),
             
             tabPanel("Monthly plots",
                      div(class="outer",
                          
                          leafletOutput("monthly_map", width="100%",height="1000px"),
                          
                      )
             ),
             tabPanel("Weekly plots",
                      div(class="outer",
                          
                          leafletOutput("weekly_map", width="100%",height="1000px"),
                          
                      )
             ),
             
             tabPanel("Hourly plots",
                      div(class="outer",
                          
                          leafletOutput("Hourly_map", width="100%",height="1000px"),
                          
                      )
             ),
             
             
             tabPanel("Data",
                      numericInput("maxrows", "Rows to show", 25),
                      verbatimTextOutput("rawtable"),
                      downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
                      "Adapted from data provided by ", tags$a(href="https://kiambu.go.ke/health-services/", 
                                                               "Kiambu County Emergency Operations Center (EOC).")
             ),
             
             tabPanel("About this site",
                      tags$div(
                        tags$h4("Last update"), 
                        
                        "This site is will be updated once daily once fully operational.", 
                        "The aim is to complement available mapping resources with several interactive features, including the timeline function and the ability to overlay past Emergencies.",
                        "It is also meant to share information with the public on emerging emergencies and also serve as a decision support tool for the county government of Kiambu.", 
                        "All information on Emergency Medical Services from Kiambu county will be provided on this site, this will help the county government to better mobilze their resouces when providing emergemcy services to the public.", 
                        
                        
                        tags$br(),tags$br(),tags$h4("Background"), 
                        "Emergency medical services (EMS), also known as ambulance services or paramedic services, are emergency services that provide urgent pre-hospital treatment and stabilisation for serious illness and injuries and transport to definitive care. 
                        They may also be known as a first aid squad.
                        In most places, the EMS can be summoned by members of the public (as well as medical facilities, other emergency services, businesses and authorities) via an emergency telephone number which puts them in contact with a control facility, which will then dispatch a suitable resource for the situation. 
                        Ambulances are the primary vehicles for delivering EMS, though some also use squad cars, motorcycles, aircraft, or boats.",
                        tags$br(),tags$br(),
                        "In isolation, these emergencies can be hard to interpret. 
                        How is demand for emergency medical services changing? Are efforts to reduce loss of life due to delayed emergencies working? Are there particular trends in occurance of these emergencies?
                        This site is updated daily based on data provided by Kiambu Emergency operations center. 
                        By looking beyond the headlines, we hope it is possible to get a deeper understanding of this unfolding Emergencies.",
                        
                        tags$br(),tags$br(),tags$h4("Code"),
                        "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/Madaga-L/AMBULANCE-LOCATION-PROJECT", "Github."),
                        tags$br(),tags$br(),tags$h4("Sources"),
                        tags$b("Kiambu Emergency Operations Center: "), tags$a(href="https://kiambu.go.ke/health-services/", "Kiambu county, department of Health services,"),
                        
                        tags$br(),tags$br(),tags$h4("Authors"),
                        "Lavinia Mmbaga Madaga, Student, GEGIS, JKUAT",tags$br(),
                        
                        tags$br(),tags$br(),tags$h4("Contact"),
                        "lavinia.mmbaga@students.jkuat.ac,ke",tags$br(),tags$br(),
                        
                        tags$br(),tags$br(),tags$h4("Kiambu county emergency contact number"),
                        "+254 709 877 000",tags$br(),tags$br(),
                      )
             )
  )
)



### SHINY SERVER ###
server = function(input, output, session) {
  # EMS tab 
  formatted_date = reactive({
    format(as.Date(input$plot_date, format="%d %b %y"), "%Y-%m-%d")
  })
  
  output$clean_date_reactive <- renderText({
    format(as.POSIXct(formatted_date()),"%d %B %Y")
  })
  
  reactive_db = reactive({
    Cumulative %>% filter(date == formatted_date())
  })
  
  output$reactive_case_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$calls), big.mark=","), " calls")
  })
  
  # EMS tab
  output$mymap <- renderLeaflet({
    
    leaflet(sub_c) %>% 
      addTiles() %>%
      addPolygons(stroke = TRUE,
                  color = "white",
                  opacity = 1,
                  group="Population density",
                  weight = 1,
                  fillOpacity = 0.4,
                  fillColor = pal(sub_c$pop_densit),
                  popup = ~glue::glue("<b>{SCouName}</b><br>{total_pop}")) %>%
      addCircleMarkers(data = Data_all,
                       lat = ~lat, lng=~lon,
                       radius = ~3,
                       fillOpacity = 0.4,
                       color = "red",
                       group = "EMS Requests",
                       label = sprintf("<strong>%s</strong><br/>DIAGNOSIS: %s<br/>Day: %s<br/>Request Time: %s", Data_all$addresses, Data_all$DIAGNOSIS_, Data_all$DAY, Data_all$REQUEST_TI) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = All_data_col),
                         textsize = "15px", direction = "auto")) %>%
      addCircleMarkers(data = Accidents,
                       lat = ~lat, lng=~lon,
                       radius = ~3,
                       fillOpacity = 0.2,
                       color = "blue",
                       group = "Road Traffic Accidents",
                       label = sprintf("<strong>%s</strong><br/>Road: %s<br/>Day: %s<br/>Time: %s", Accidents$addresses, Accidents$ROAD, Accidents$DAY, Accidents$TIME_24_HO) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = Accidents_col),
                         textsize = "15px", direction = "auto")) %>%
      addCircleMarkers(data = Amb_stations1,
                       lat = ~lat, lng=~lon,
                       radius = ~5,
                       fillOpacity = 0.8,
                       color = "orange",
                       group = "Existing Facilities",
                       label = sprintf("<strong>%s</strong><br/>Address: %s<br/>Sub county: %s<br/>Number of Ambulances: %s", Amb_stations1$addresses, Amb_stations1$geoAddress, Amb_stations1$SCouName, Amb_stations1$NumberofAm) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = existing_col),
                         textsize = "15px", direction = "auto")) %>%
      addCircleMarkers(data = optimal_sites,
                       lat = ~latitude, lng=~longitude,
                       radius = ~5,
                       fillOpacity = 0.8,
                       color = "brown",
                       group = "Optimal Sites",
                       label = sprintf("<strong>%s</strong><br/>Station ID: %s<br/>Number of Ambulances: %s", optimal_sites$Site_name, optimal_sites$StationID, optimal_sites$NumberOfAm) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = optimal_col),
                         textsize = "15px", direction = "auto")) %>%
      addLayersControl(
        position = "topright",
        overlayGroups = c("EMS Requests", "Road Traffic Accidents", "Existing Facilities", "Optimal Sites"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup(c("EMS Requests", "Road Traffic Accidents", "Existing Facilities", "Optimal Sites")) %>%
      fitBounds(~-100,-60,~60,70) %>%
      addLegend("topright", pal = pal, values = ~sub_c$pop_densit,
                title = "<small>persons/sq.km>")
    
  })
  
  output$cumulative_plot <- renderPlotly({
    cumulative_plot(cv_aggregated, formatted_date())
  })
  
  output$epi_curve <- renderPlotly({
    new_cases_plot(cv_aggregated, formatted_date())
    
  })
  
  # Monthly tab
  output$monthly_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addRasterImage(mar, colors = pal0, opacity = 0.4, group = "March") %>%
      addRasterImage(Apr, colors = pal1, opacity = 0.4, group = "April") %>%
      addRasterImage(May, colors = pal2, opacity = 0.4, group = "May") %>%
      addRasterImage(June, colors = pal3, opacity = 0.4, group = "June") %>%
      addRasterImage(July, colors = pal4, opacity = 0.4, group = "July") %>%
      addRasterImage(Aug, colors = pal5, opacity = 0.4, group = "August") %>%
      addLayersControl(
        position = "topright",
        overlayGroups = c("March", "April", "May", "June", "July", "August"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup(c("March", "April", "May", "June", "July", "August")) %>%
      addLegend(pal = pal_leg, values = values(March_leg),
                opacity = 0.4,
                title = "calls/10sq.km")
  })
  # Weekly tab
  output$weekly_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addRasterImage(mon, colors = pal6, opacity = 0.4, group = "Monday") %>%
      addRasterImage(tue, colors = pal7, opacity = 0.4, group = "Tuesday") %>%
      addRasterImage(wed, colors = pal8, opacity = 0.4, group = "Wednesday") %>%
      addRasterImage(thur, colors = pal9, opacity = 0.4, group = "Thursday") %>%
      addRasterImage(fri, colors = pal10, opacity = 0.4, group = "Friday") %>%
      addRasterImage(sat, colors = pal11, opacity = 0.4, group = "Saturday") %>%
      addRasterImage(sun, colors = pal12, opacity = 0.4, group = "Sunday") %>%
      addLayersControl(
        position = "topright",
        overlayGroups = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) %>%
      addLegend(pal = pal_mon, values = values(mon_leg), 
                title = "calls/10sq.km")
  })
  
  output$Hourly_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addRasterImage(oohrs, colors = pal13, opacity = 0.5, group = "0000hrs") %>%
      addRasterImage(o1hrs, colors = pal14, opacity = 0.5, group = "0100hrs") %>%
      addRasterImage(o2hrs, colors = pal15, opacity = 0.5, group = "0200hrs") %>%
      addRasterImage(o3hrs, colors = pal16, opacity = 0.5, group = "0300hrs") %>%
      addRasterImage(o4hrs, colors = pal17, opacity = 0.5, group = "0400hrs") %>%
      addRasterImage(o5hrs, colors = pal18, opacity = 0.5, group = "0500hrs") %>%
      addRasterImage(o6hrs, colors = pal19, opacity = 0.5, group = "0600hrs") %>%
      addRasterImage(o7hrs, colors = pal20, opacity = 0.5, group = "0700hrs") %>%
      addRasterImage(o8hrs, colors = pal21, opacity = 0.5, group = "0800hrs") %>%
      addRasterImage(o9hrs, colors = pal22, opacity = 0.5, group = "0900hrs") %>%
      addRasterImage(o10hrs, colors = pal23, opacity = 0.5, group = "1000hrs") %>%
      addRasterImage(o11hrs, colors = pal24, opacity = 0.5, group = "1100hrs") %>%
      addRasterImage(o12hrs, colors = pal25, opacity = 0.5, group = "1200hrs") %>%
      addRasterImage(o13hrs, colors = pal26, opacity = 0.5, group = "1300hrs") %>%
      addRasterImage(o14hrs, colors = pal27, opacity = 0.5, group = "1400hrs") %>%
      addRasterImage(o15hrs, colors = pal28, opacity = 0.5, group = "1500hrs") %>%
      addRasterImage(o16hrs, colors = pal29, opacity = 0.5, group = "1600hrs") %>%
      addRasterImage(o17hrs, colors = pal30, opacity = 0.5, group = "1700hrs") %>%
      addRasterImage(o18hrs, colors = pal31, opacity = 0.5, group = "1800hrs") %>%
      addRasterImage(o19hrs, colors = pal32, opacity = 0.5, group = "1900hrs") %>%
      addRasterImage(o20hrs, colors = pal33, opacity = 0.5, group = "2000hrs") %>%
      addRasterImage(o21hrs, colors = pal34, opacity = 0.5, group = "2100hrs") %>%
      addRasterImage(o22hrs, colors = pal35, opacity = 0.5, group = "2200hrs") %>%
      addRasterImage(o23hrs, colors = pal36, opacity = 0.5, group = "2300hrs") %>%
      
      addLayersControl(
        position = "topright",
        overlayGroups = c("0000hrs", "0100hrs", "0200hrs", "0300hrs", "0400hrs", "0500hrs", "0600hrs", "0700hrs", "0800hrs", "0900hrs", "1000hrs", "1100hrs", "1200hrs", "1300hrs", "1400hrs", "1500hrs", "1600hrs", "1700hrs", "1800hrs", "1900hrs", "2000hrs", "2100hrs", "2200hrs", "2300hrs"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup(c("0000hrs", "0100hrs", "0200hrs", "0300hrs", "0400hrs", "0500hrs", "0600hrs", "0700hrs", "0800hrs", "0900hrs", "1000hrs", "1100hrs", "1200hrs", "1300hrs", "1400hrs", "1500hrs", "1600hrs", "1700hrs", "1800hrs", "1900hrs", "2000hrs", "2100hrs", "2200hrs", "2300hrs")) %>%
      addLegend("topleft", pal = hourly_pal, values = values(hourly_leg), 
                title = "calls/10sq.km")
    
  })
  # output to download data
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("EMS_data_", Data_EMS$date[1], ".csv", sep="")
    },
    content = function(file) {
      cv_cases_sub = Data_EMS %>% dplyr::select(c(date, lon, lat, DAY, DIAGNOSIS_,
                                                  addresses, REQUEST_TI, DISPATCH_T, geoAddress))
      names(cv_cases_sub) = c("date", "longitude", "latitude", "Day", "Emergency Diagnosis",
                              "address", "request time", "dispatch time", "geoaddress")
      write.csv(cv_cases_sub, file)
    }
  )
  
  output$rawtable <- renderPrint({
    cv_cases_sub = Data_EMS %>% dplyr::select(c(date, lon, lat, DAY, DIAGNOSIS_,
                                                addresses, REQUEST_TI, DISPATCH_T, geoAddress))
    names(cv_cases_sub) = c("date", "longitude", "latitude", "Day", "Emergency Diagnosis",
                            "address", "request time", "dispatch time", "geoaddress")
    orig <- options(width = 1000)
    print(tail(cv_cases_sub, input$maxrows), row.names = FALSE)
    options(orig)
  })
  
}



#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)












































































