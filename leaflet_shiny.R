library(magrittr)
library(shiny)
library(leaflet)

#data = read_csv("./new.csv", locale = locale(encoding = "UTF-8")) %>% 
#  mutate(floor = str_trim(str_extract(floor,"( .*)"), side = "both"))

# run data cleaning code here
# load the database handler
library(RPostgreSQL)

# parameters for connecting to database
pg = dbDriver("PostgreSQL")

endpoint <- 'beijing-housing.copmdh9kwiqr.us-east-2.rds.amazonaws.com'
portnum <- 5432
username <- 'biostat625'
pwd <- 'shinygroup2'

# set up connection object
con = dbConnect(pg, user=username, password=pwd,
                host=endpoint, port=portnum, dbname='housing')

# query all the data for now
data = dbGetQuery(con, "SELECT * FROM housing WHERE district = 'ShunYi';")

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
  
  house_subset <- eventReactive(input$resample, {
    sample_houses = sample(1:30000, 100)
    cbind(data$lng[sample_houses], data$lat[sample_houses])
  }, ignoreNULL = FALSE)
  
  output$house_subset <- renderDataTable(house_subset())
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = house_subset())
  })
}

shinyApp(ui, server)

