library(magrittr)
library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
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
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "back")
)

server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(data$Lng[1:100], data$Lat[1:100])
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
  })
}

shinyApp(ui, server)



