library(magrittr)
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
# connect to database
source("data_cleaning_storage/data_query_example.R")

# source the model building code
source("Prediction_Model_Function.R")

ui = fluidPage(
  fluidPage(
    titlePanel("Beijing Second-hand House Market"),
    textOutput("Introduction"),
    absolutePanel("Hi"),
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
          selected = "HaiDian"
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
        h3("Plots: "),
        hr(),
        fluidRow(plotlyOutput(outputId = "histogram", height = 300, width = 1200)),
        fluidRow(plotlyOutput(outputId = "trendline", height = 300, width = 1200)),
        p(),
        h3("Statistic: "),
        splitLayout(tableOutput(outputId = "coefficient"),
                   verticalLayout(tableOutput(outputId = "model_para"), 
                                  uiOutput("interpretation"))),
        p("House trade data is from Lianjia.com")
      )
    )
  )
)

library(htmltools)
server = function(input, output, session) {
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
    leafletProxy("map") %>% setView(lng = mean(data_sub$lng), lat=mean(data_sub$lat), zoom = 12)
    return(list(data_sub, model, input$district))
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
    leaflet() %>% setView(lng = 116.4, lat=39.93, zoom = 11) %>%
      addProviderTiles(providers$OpenStreetMap.DE)
  })
  
  observe({
    leafletProxy("map") %>% clearMarkerClusters()
    leafletProxy("map") %>% clearMarkers()
    data_sub = points()[[1]]
    leafletProxy("map", data = data_sub) %>% 
      addMarkers(~lng, ~lat, layerId = ~1:nrow(data_sub), 
                 clusterOptions = markerClusterOptions())
  })
  
  output$Introduction = renderText({
    "To start using this app, specify filters on the left panel and hit refresh. 
    The process may take some time when the district you select has large number of transactions."
  })
  
  output$histogram = renderPlotly({
    data_sub = points()[[1]]
    ggplot(data_sub, aes(x=totalprice, fill=data_sub$district)) + 
      geom_histogram(binwidth=7,position = "dodge") +
      scale_color_brewer(palette = "Set2") +
      labs( x = "Total Price in CNY", y = "Amount of Houses", title = "Histogram of Houses",
            caption = "click the district name to select/deselect them") +
      scale_fill_discrete(name = "District", labels = c("all", "ChangPing","ChaoYang", "DaXing", 
                                                        "DongCheng", "FangShan","FengTai", "HaiDian", 
                                                        "MenTouGou", "ShiJingShan", "ShunYi", "TongZhou","XiCheng")) +
      theme_classic()
  })
  
  output$trendline = renderPlotly({
    result = points()
    data_model = result[[2]]$beta_data
    plots = ggplot(data_model, aes(x = year, y = price , color = class)) + 
      geom_line(size=1) + labs(x = 'Year' ,y = 'Price/m2 in CNY', title = "Price Trend + Prediction") + 
      scale_color_brewer(palette = "Set2") + 
      scale_x_continuous(breaks = c(2012,2014,2016,2018)) +
      scale_fill_discrete(name = "type") +
      theme_classic()
    if(result[[3]] == 'all') {
      # data from https://www.anjuke.com/fangjia/beijing2018/
      housing_price_2018 = cbind(seq(2018,2019, by=0.08333)[-1], 
                                 c(57523,57557,57717,57829,57931,58166,58355,59376,59913,59943,59602,59868))
      # data from https://www.anjuke.com/fangjia/beijing2019/
      housing_price_2019 = cbind(seq(2019,2020, by=0.08333)[-1], 
                                 c(60017,60125,60487,59993,59809,60142,60061,59888,59590,59126,58540,58568))
      housing_price_2018_2019 = rbind(housing_price_2018, housing_price_2019)
      colnames(housing_price_2018_2019) = c("time", "price")
      true_price = rep(2010, length(data_model$price))
      true_price[(length(true_price)-23):length(true_price)] = housing_price_2018_2019[,2]
      true_x = rep(0, length(data_model$year))
      true_x[(length(true_x)-23):length(true_x)] = housing_price_2018_2019[,1]

      plots = ggplot(data_model, aes(x = year, y = price , color = class)) + 
        geom_line(size=1) + labs(x = 'Year' ,y = 'Price/m2 in CNY', title = "Price Trend + Prediction") + 
        geom_line(aes(x=true_x, y=true_price)) +
        scale_color_brewer(palette = "Set2") + 
        scale_x_continuous(breaks = c(2012,2014,2016,2018)) +
        scale_fill_discrete(name = "type") +
        theme_classic()
    }
    ggplotly(plots)
  })
  
  output$coefficient = renderTable({
    betas = points()[[2]]$coefficients
  }, rownames = TRUE)
  
  output$model_para = renderTable({
    points()[[2]]$R_Squared
  }, rownames = TRUE, digits=3)
  
  output$interpretation = renderUI({
    betas = as.data.frame(points()[[2]]$coefficients)
    betas$variables = rownames(betas)
    
    # UI/HTML output
    interpret = tags$div(tags$p(sprintf("In the %s district, when time is fixed, ", input$district)),
                             tags$p(sprintf("One more bathroom will increase the price per m2 by %.0f RMB;",
                                     as.numeric(filter(betas, variables=="bathroom")[1]))),
                             tags$p(sprintf("Price of houses with elevator is %.0f RMB higher than ones without elevator;",
                                     as.numeric(filter(betas, variables=="elevator")[1]))),
                             tags$p(sprintf("Price of houses near subway is %.0f RMB higher than ones away from subway;",
                                     as.numeric(filter(betas, variables=="subway")[1]))),
                             tags$p(sprintf("Price of plate house type is %.0f and %.0f higher than other two respectively.",
                                     abs(filter(betas, variables=="buildingtypePlate/Tower")[1]),
                                     abs(filter(betas, variables=="buildingtypeTower")[1]))))
  })


  # close connection to database after session ends
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
}

shinyApp(ui, server)

