library(shiny)
library(shinythemes)
library(readr)
library(rjson)
library(maps)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(RColorBrewer)
library(leaflet)
library(DT)
library(magrittr)

stopdata <- read.csv("Stopdata.csv")

lat_mean <- mean(rowMeans(stopdata[ , c(12,15)], na.rm=TRUE))
lon_mean <- mean(rowMeans(stopdata[ , c(13,16)], na.rm=TRUE))


basemap <- leaflet() %>% addTiles() %>% setView(lat = lat_mean, lng = lon_mean, 
                                                zoom = 15)
basemap

ui <- navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                 HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">MBTA Boston</a>'), id="nav",
                 windowTitle = "MBTA Boston",
                 tabPanel("Map", 
                          sidebarPanel(
                            titlePanel("Green Line Information"), 
                            fluidRow(selectInput(inputId ="route_id",
                                      label = 'Choose route', 
                                      choices = c("Green-B", "Green-C", 
                                                  "Green-D", "Green-E"), 
                                      selected = "Green-B"), 
                                     selectInput(inputId = 'from_stop', 
                                      label = 'From stop--To stop', 
                                      choices = c("Babcock Street to Boston University Central", 
                                                  "Brandon Hall to Tappan Street", 
                                        "Brookline Village to Longwood", "Boylston to Prudential"), 
                                      selected = "Babcock Street to Boston University Central"), 
                          selectInput(inputId = "service_date", 
                                      label = 'Date', 
                                      choices = unique(stopdata$service_date), 
                                      selected = "2021-11-15"))),
                          mainPanel(
                            titlePanel("Green Line Map"),
                            fluidRow("Map", leafletOutput("mbtamap", 
                                                       width=800,height=500),
                                     "Information Table",
                                     DT::dataTableOutput("Travel_Time")),
                          )
                          )
                 )



server <- function(input, output, session) {
  observe({
    x=input$route_id
    y=input$from_stop
    if (x == "Green-C") 
      y <- c("Brandon Hall to Tappan Street")
    
    else if(x == "Green-D")
          y <- c("Brookline Village to Longwood")
    
    else if (x == "Green-E")
      y <- c("Boylston to Prudential")
    
    else if (x == "Green-B")
      y = c("Babcock Street to Boston University Central")
    
    updateSelectInput(session, 'from_stop', 
                      label = 'From stop--To stop',
                      choices = y)
    })

  
  output$Travel_Time<- DT::renderDataTable({
    data1<-stopdata[, c(1, 4, 8, 11, 14, 10)] %>% 
      filter(route_id==input$route_id) %>%
      filter(service_date == input$service_date) 
      
  })
  
  output$mbtamap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = filter(stopdata, route_id =="Green-B"),
                       lng = ~ from_stop_lon, lat = ~from_stop_lat,
                       popup = ~ from_stop_name,
                       label = ~ from_stop_name,
                       radius = 5,
                       col = "red") %>%
      addCircleMarkers(data = filter(stopdata, route_id =="Green-B"),
                       lng = ~ to_stop_lon, lat = ~to_stop_lat,
                       popup = ~ to_stop_name,
                       label = ~ to_stop_name,
                       radius = 5,
                       col = "red") %>%
      addCircleMarkers(data = filter(stopdata, route_id =="Green-C"),
                       lng = ~ from_stop_lon, lat = ~from_stop_lat,
                       popup = ~ from_stop_name,
                       label = ~ from_stop_name,
                       radius = 5,
                       col = "green") %>%
      addCircleMarkers(data = filter(stopdata, route_id =="Green-C"),
                       lng = ~ to_stop_lon, lat = ~to_stop_lat,
                       popup = ~ to_stop_name,
                       label = ~ to_stop_name,
                       radius = 5,
                       col = "green") %>%
      addCircleMarkers(data = filter(stopdata, route_id =="Green-D"),
                     lng = ~ from_stop_lon, lat = ~from_stop_lat,
                     popup = ~ from_stop_name,
                     label = ~ from_stop_name,
                     radius = 5,
                     col = "blue") %>%
      addCircleMarkers(data = filter(stopdata, route_id =="Green-D"),
                       lng = ~ to_stop_lon, lat = ~to_stop_lat,
                       popup = ~ to_stop_name,
                       label = ~ to_stop_name,
                       radius = 5,
                       col = "blue") %>%
    addCircleMarkers(data = filter(stopdata, route_id =="Green-E"),
                     lng = ~ from_stop_lon, lat = ~from_stop_lat,
                     popup = ~ from_stop_name,
                     label = ~ from_stop_name,
                     radius = 5,
                     col = "purple") %>%
      addCircleMarkers(data = filter(stopdata, route_id =="Green-E"),
                       lng = ~ to_stop_lon, lat = ~to_stop_lat,
                       popup = ~ to_stop_name,
                       label = ~ to_stop_name,
                       radius = 5,
                       col = "purple") %>%
      addPolylines(data = filter(stopdata, route_id =="Green-B"), 
                   lng = ~ c(from_stop_lon, to_stop_lon), 
                   lat = ~ c(from_stop_lat, to_stop_lat),
                   label = "The mean travel time is 240.82 sec.", 
                   color = "red") %>% 
      addPolylines(data = filter(stopdata, route_id =="Green-C"), 
                   lng = ~ c(from_stop_lon, to_stop_lon), 
                   lat = ~ c(from_stop_lat, to_stop_lat), 
                   label = "The mean travel time is 162.47 sec.",
                   color = "green") %>% 
      addPolylines(data = filter(stopdata, route_id =="Green-D"), 
                   lng = ~ c(from_stop_lon, to_stop_lon), 
                   lat = ~ c(from_stop_lat, to_stop_lat), 
                   label = "The mean travel time is 94.19 sec.",
                   color = "blue") %>% 
      addPolylines(data = filter(stopdata, route_id =="Green-E"), 
                   lng = ~ c(from_stop_lon, to_stop_lon), 
                   lat = ~ c(from_stop_lat, to_stop_lat), 
                   label = "The mean travel time is 356.96 sec.",
                   color = "purple")
    })

}
    
    

shinyApp(ui = ui, server = server)
