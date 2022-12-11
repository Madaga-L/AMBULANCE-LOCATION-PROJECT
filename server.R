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