library(magrittr)
library(shiny)
library(leaflet)
library(dplyr)

# generate the connection object
source("data_cleaning_storage/data_query_example.R")

# generate initial data
info = gen_data("all", c())
sample_houses = sample(1:nrow(info$data), 100)
data_sub = info$data[sample_houses,]


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
        leafletOutput("mymap"),
        p(),
        textOutput(outputId = "district_filter", inline = TRUE),
        plotOutput(outputId = "histogram", height = 200)
      )
    )
  )
)


library(htmltools)
server = function(input, output, session) {
  
  # querydata = reactiveVal(info$data)
  
  # query raw data only based on district and buildingtype
  points = eventReactive(input$resample, {
    info = gen_data(input$district, input$building_type)
    print("query data")
    # querydata(newinfo$data)
    # TODO: fit the model here
    sample_houses = sample(1:nrow(info$data), min(floor(nrow(info$data)/5), 100))
    data_sub <- info$data[sample_houses,]
    return(data_sub)
  })
  
  
  # TODO: generate subset of data for histogram (based on price range and squares elevator subway)
  
  # TODO: generate subset of data for map(random sample)
  #input$
  
  output$mymap = renderLeaflet({
    data_sub = points()
    data_sub$popup_content = data_sub[,5]
    data_sub %>% leaflet() %>% addProviderTiles(providers$OpenStreetMap.DE) %>%
      addMarkers(~lng, ~lat, popup = ~htmlEscape(popup_content))
  })
  
  output$district_filter = renderText({
    print(input$district)
    #points() %>% summary()
  })
  
  output$histogram = renderPlot({
    data_sub = points()
    ggplot(data_sub  %>% filter(elevator==0 & subway==0), aes(x=totalprice)) +
      geom_histogram(bins = 70)
  })
}

shinyApp(ui, server)

