
ui <- fluidPage(google_mapOutput("map"))

server <- function(input, output, session){
  
  api_key <- "AIzaSyACIYNuS69B_KiHLEBslcsiL08kcgWez6E"
  
  output$map <- renderGoogle_map({
    google_map(key = api_key)
  })
}

shinyApp(ui, server)

## using split view

library(shinydashboard)
library(googleway)

ui <- dashboardPage(
  
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    box(width = 6,
        google_mapOutput(outputId = "map")
    ),
    box(width = 6,
        google_mapOutput(outputId = "pano")
    )
  )
)

server <- function(input, output) {
  set_key("your_api_key")
  
  output$map <- renderGoogle_map({
    google_map(location = c(-37.817386, 144.967463),
               zoom = 10,
               split_view = "pano")
  })
}

shinyApp(ui, server)