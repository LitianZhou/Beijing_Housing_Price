library(magrittr)
library(shiny)
library(leaflet)

#data = read_csv("./new.csv", locale = locale(encoding = "UTF-8")) %>% 
#  mutate(floor = str_trim(str_extract(floor,"( .*)"), side = "both"))

# run data cleaning code here


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
          c("all", "ChaoYang", "HaiDian", "DongCheng"),
          selected = "ChaoYang"
        ),
        checkboxGroupInput(inputId = "building_type", label = "building type",
                           choices = c("tower","plate", "plate/tower"),
                           inline = FALSE),
        checkboxInput(inputId = "has_elevator", label="elevator", value = FALSE),
        checkboxInput(inputId = "has_subway", label="subway", value = FALSE),
        submitButton("apply filter", icon("refresh"))
      ),
      mainPanel(
        leafletOutput("mymap"),
        p(),
        actionButton(inputId ="resample", label = "refresh"),
        textOutput(outputId = "district_filter", inline = TRUE)
      )
    )
  )
)

library(htmltools)
server = function(input, output, session) {
  points = eventReactive(input$resample, {
    sample_houses = sample(1:nrow(data), 300)
    data_sub = data[sample_houses,]
  }, ignoreNULL = FALSE)
  #input$
  
  data_sub$popup_content = data_sub[,5]
  output$mymap = renderLeaflet({
    leaflet(data_sub) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(~lng, ~lat, popup = ~htmlEscape(popup_content))
  })
  
  output$district_filter = renderText({
    print(input$district)
    data_sub %>% filter(district == input$district) %>% summary()
  })
}

shinyApp(ui, server)

