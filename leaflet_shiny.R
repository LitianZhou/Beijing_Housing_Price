library(magrittr)
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)

ui = fluidPage(
  fluidPage(
    titlePanel("Beijing Second-hand House Market"),
    sidebarLayout(
      sidebarPanel(
        sliderInput("price_range", label = h5("price range(10k)"), min = 10, 
                    max = 4000, value = c(100, 1000)),
        sliderInput("square_range", label = h5("square(m2)"), min = 7, 
                    max = 500, value = c(70, 400)),
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
        dateRangeInput(inputId="trade_time_range", label="trade time range", 
                       start = "2010-01-01", end = "2017-12-31", min="2010-01-01", max="2018-1-31", 
                       format = "mm/dd/yy", separator="-"),
        actionButton(inputId ="resample", label = "refresh"),
        width = 2
      ),
      mainPanel(
        leafletOutput("map",width = "120%", height = 700),
        p(),
        textOutput(outputId = "district_filter", inline = TRUE),
        fluidRow(plotlyOutput(outputId = "histogram", height = 300, width = 1000)),
        fluidRow(plotOutput(outputId = "trendline", height = 300, width = 1000)),
        splitLayout(tableOutput(outputId = "coefficient"),
                   tableOutput(outputId = "model_para")),
        p("House trade data is from Lianjia.com")
      )
    )
  )
)

library(htmltools)
server = function(input, output, session) {
  # connect to database
  source("data_cleaning_storage/data_query_example.R")
  
  # source the model building code
  source("Prediction_Model_Function.R")
  
  # query raw data only based on district and buildingtype
  points = eventReactive(input$resample, {
    info = gen_data(input$district, input$building_type)
    model = Filter_model(info)
    if(length(model)<3) print(model) # model failing will return a string
    # filters applied to markers only
    data_sub = info$data %>% filter(elevator==as.numeric(input$has_elevator) & subway==as.numeric(input$has_subway)
                                    & totalprice >= input$price_range[1] & totalprice <= input$price_range[2]
                                    & (square+82.69) >= input$square_range[1] & (square+82.69) <= input$square_range[2]
                                    & tradetime >= as.Date(input$trade_time_range[1]) & tradetime <= as.Date(input$trade_time_range[2]))
    # if(nrow(data_sub)==0) output$warning = "No house satisfy the filter"
    sample_houses = sample(1:nrow(data_sub), min(nrow(data_sub), 700))
    data_sub = data_sub[sample_houses,]
    return(list(data_sub, model))
  })
  
  #show a pop-up when a mark is clicked
  showHouseInfo = function(lng, lat, id){
    data_sub = points()[[1]]
    selectedHouse = data_sub[id,]
    content <- as.character(tagList(
      tags$h4("Price:", as.integer(selectedHouse$totalprice*10), " k"),
      tags$strong(HTML(
      sprintf("%.0fm2,   %.0fk/m2", (selectedHouse$square+82.69), (selectedHouse$price/1000)))), tags$br(),
      sprintf("building type: %5s", selectedHouse$buildingtype), tags$br(),
      sprintf("has elevator: %5s", as.character(as.logical(selectedHouse$elevator))), tags$br(),
      sprintf("district: %5s", selectedHouse$district), tags$br(),
      sprintf("trade time: %5s", selectedHouse$tradetime)
    ))
    leafletProxy("map") %>% addPopups(lng, lat+0.0001, content)
  }
  
  observe({
    leafletProxy("map") %>% clearPopups()
    house <- input$map_marker_click
    if (is.null(house))
      return()
    showHouseInfo(house$lng, house$lat, house$id)
  })
  
  output$map = renderLeaflet({
    leaflet() %>% setView(lng = 116.4, lat=39.9, zoom = 11) %>%
      addProviderTiles(providers$OpenStreetMap.DE)
  })
  
  observe({
    data_sub = points()[[1]]
    leafletProxy("map", data = data_sub) %>% 
      clearMarkers() %>%
      addMarkers(~lng, ~lat, layerId = ~1:nrow(data_sub), 
                 clusterOptions = markerClusterOptions())
  })
  
  output$histogram = renderPlotly({
    data_sub = points()[[1]]
    ggplot(data_sub, aes(x=totalprice, fill=data_sub$district)) + 
      geom_histogram(bins = nrow(data_sub)/50,position = "dodge") +
      scale_color_brewer(palette = "Set3") +
      labs( x = "Total Price", y = "Amount of Houses", title = "Histogram of Houses",
            caption = "click the district name to select/deselect them") +
      theme_classic()
  })
  
  output$trendline = renderPlot({
    points()[[2]]$Prediction_Plot
  })
  
  output$coefficient = renderTable({
    betas = points()[[2]]$coefficients
    #betas$variables = rownames(betas)
  }, rownames = TRUE, digits =-2)
  
  output$model_para = renderTable({
    points()[[2]]$R_Squared
  }, rownames = TRUE, digits=3)
  
  # close connection to database after session ends
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
}

shinyApp(ui, server)

