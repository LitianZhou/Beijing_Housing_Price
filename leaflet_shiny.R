library(magrittr)
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)

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
        actionButton(inputId ="resample", label = "refresh"),
        width = 2
      ),
      mainPanel(
        leafletOutput("map",width = "120%", height = 700),
        p(),
        textOutput(outputId = "district_filter", inline = TRUE),
        splitLayout(plotOutput(outputId = "histogram", height = 300, width = 500),
                   plotOutput(outputId = "trendline", height = 300, width = 500)),
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
                                    & (square+82.69) >= input$square_range[1] & (square+82.69) <= input$square_range[2])
    # if(nrow(data_sub)==0) output$warning = "No house satisfy the filter"
    sample_houses = sample(1:nrow(data_sub), min(nrow(data_sub), 700))
    data_sub = data_sub[sample_houses,]
    return(list(data_sub, model))
  })
  
  # TODO: generate subset of data for histogram (based on price range and squares elevator subway)
  
  # TODO: generate subset of data for map(random sample)
  
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
    data_sub = points()[[1]]
    data_sub %>% leaflet() %>% addProviderTiles(providers$OpenStreetMap.DE) %>%
      addMarkers(~lng, ~lat, layerId = ~1:nrow(data_sub),
                 clusterOptions = markerClusterOptions())
  })
  
  output$histogram = renderPlot({
    data_sub = points()[[1]]
    # handle the elevator and subway, considering give the task to Mukai
    ggplot(data_sub  %>% filter(elevator==0 & subway==0), aes(x=totalprice)) +
      geom_histogram(bins = 30)
  })
  
  output$trendline = renderPlot({
    points()[[2]]$Prediction_Plot
  })
  
  output$coefficient = renderTable({
    betas = points()[[2]]$coefficients
    #betas$variables = rownames(betas)
  }, rownames = TRUE, digits =-2)
  
  output$model_para = renderTable({
    print("render model para")
    points()[[2]]$R_Squared
  }, rownames = TRUE, digits=3)
  
  # close connection to database after session ends
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
}

shinyApp(ui, server)

