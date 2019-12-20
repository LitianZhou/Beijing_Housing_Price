library(magrittr)
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)


# define the interface appearance
ui = fluidPage(
  fluidPage(
    titlePanel("Houses on Market"),
    sidebarLayout(
      sidebarPanel(
        sliderInput("price_range", label = h5("price range"), min = 100000, 
                    max = 1000000, value = c(400000, 600000)),
        sliderInput("square_range", label = h5("square"), min = 30, 
                    max = 10000, value = c(100, 6000)),
        selectInput(
          inputId = "district", label = "district",
          c("all", "ChangPing","ChaoYang", "DaXing", "DongCheng", "FangShan",
            "FengTai", "HaiDian", "MenTouGou", "ShiJingShan", "ShunYi", "TongZhou",
            "XiCheng"),
          selected = "all"
        ),
        checkboxGroupInput(inputId = "building_type", label = "building type",
                           choices = c("Tower","Plate", "Plate/Tower"),
                           selected = c("Tower","Plate", "Plate/Tower"),
                           inline = FALSE),
        checkboxInput(inputId = "has_elevator", label="elevator", value = FALSE),
        checkboxInput(inputId = "has_subway", label="subway", value = FALSE),
        actionButton(inputId ="resample", label = "refresh")
      ),
      mainPanel(
        leafletOutput("map"),
        p(),
        textOutput(outputId = "district_filter", inline = TRUE),
        fluidRow(plotOutput(outputId = "histogram", height = 200))
      )
    )
  )
)

library(htmltools)
server = function(input, output, session) {
  # connect to database
  source("data_cleaning_storage/data_query_example.R")
  # querydata = reactiveVal(info$data)
  
  # query raw data only based on district and buildingtype
  points = eventReactive(input$resample, {
    info = gen_data(input$district, input$building_type)
    # querydata(newinfo$data)
    #TODO: fit the model here
    sample_houses = sample(1:nrow(info$data), min(floor(nrow(info$data)/5), 700))
    data_sub <- info$data[sample_houses,]
    return(data_sub)
  })
  
  
  # TODO: generate subset of data for histogram (based on price range and squares elevator subway)
  
  # TODO: generate subset of data for map(random sample)
  
  #show a pop-up when a mark is clicked
  showHouseInfo = function(lng, lat, ind){
    selectedHouse = data_sub[ind,]
    content <- as.character(tagList(
      tags$h4("Price:", as.integer(selectedHouse$totalprice)),
      tags$strong(HTML(sprintf("%.0fm2,   %.0fk/m2",
                                selectedHouse$square, (selectedHouse$price/1000)
      ))), tags$br(),
      sprintf("Building type: %5s", selectedHouse$buildingtype), tags$br(),
      sprintf("Has elevator: %5s", as.character(as.logical(selectedHouse$elevator))), tags$br(),
      sprintf("District: %5s", selectedHouse$district)
    ))
    leafletProxy("map") %>% addPopups(lng, lat+0.001, content)
  }
  
  observe({
    leafletProxy("map") %>% clearPopups()
    house <- input$map_marker_click
    if (is.null(house))
      return()
    
    isolate({
      showHouseInfo(house$lng, house$lat, house$id)
    })
  })
  
  output$map = renderLeaflet({
    data_sub = points()
    data_sub$popup_content = data_sub %>% select(c(square, totalprice))
    data_sub %>% leaflet() %>% addProviderTiles(providers$OpenStreetMap.DE) %>%
      addMarkers(~lng, ~lat, layerId = ~1:nrow(data_sub),
                 clusterOptions = markerClusterOptions())
  })
  
  output$histogram = renderPlot({
    data_sub = points()
    # handle the elevator and subway, considering give the task to Mukai
    ggplot(data_sub  %>% filter(elevator==0 & subway==0), aes(x=totalprice)) +
      geom_histogram(bins = 30)
  })
  
  output$trendline = renderPlot({
    #filter by both district and building_type, then fit Kangping's model
    cat("you select ",input$district)
    cat("you select ", input$building_type)
  })
  
  # close connection to database after session ends
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
}

shinyApp(ui, server)

