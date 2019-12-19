library(magrittr)
library(shiny)
library(leaflet)

#data = read_csv("./new.csv", locale = locale(encoding = "UTF-8")) %>% 
#  mutate(floor = str_trim(str_extract(floor,"( .*)"), side = "both"))

# run data cleaning code here


ui <- fluidPage(
  fluidPage(
    titlePanel("Houses on Market"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "district_fit", "Fit by distric?",
          c(Yes = "yes", No = "no"),
          selected = "no"
        ),
        conditionalPanel(
          condition = "input.district_fit == 'yes'",
          selectInput(
            "district", "District",
            c("Chao Yang", "Hai Dian", "Dong Cheng")
          )
        )
      ),
      mainPanel()
    )
  ),
  leafletOutput("mymap"),
  p(),
  actionButton("resample", "resample")
)

server <- function(input, output, session) {
  
  points <- eventReactive(input$resample, {
    set.seed(625)
    sample_houses = sample(1:30000, 100)
    cbind(data$Lng[sample_houses], data$Lat[sample_houses])
  }, ignoreNULL = FALSE)
  
  output$points <- renderDataTable(points())
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
  })
}

shinyApp(ui, server)

