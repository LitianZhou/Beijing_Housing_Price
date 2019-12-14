library(googleway)
library(tidyverse)
library(magrittr)
library(shiny)

ui = fluidPage(
  fluidPage(
    titlePanel("Houses on Market"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "district_fit", "Fit by distric?",
          c(Yes = "yes", No = "no"),
          selected = "yes"
        ),
        conditionalPanel(
          condition = "input.region_fit == 'yes'",
          selectInput(
            "district", "District",
            c("Chao Yang", "Hai Dian", "Dong Cheng")
          )
        )
      ),
      mainPanel()
    )
  ),
  google_mapOutput("map")
)

server = function(input, output, session){
  
  api_key = "AIzaSyACIYNuS69B_KiHLEBslcsiL08kcgWez6E"
  
  # draw markers (houses) on map
  data$info <- paste0("<b>Total price: </b>", data$totalPrice)
  
  output$map = renderGoogle_map({
    house_locations = as.data.frame(cbind(data$Lat, data$Lng))
    google_map(key = map_key, data = head(house_locations), location = c(39.9, 116.4), zoom = 12) %>%
      add_markers(lat = "V1", lon = "V2")
  })
}



shinyApp(ui, server)


