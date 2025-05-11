#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("preparation.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  fluidRow(div(class = "animated-box", h1("London Bike Rental Points: Stations & Availability"))),
  fluidRow(div(class = "box2", leafletOutput("bikeMap"))), br(),
  fluidRow(
    column(
      3,
      offset = 3,
      div(
        class = "custom-box",
        numericInput("bikes", "Minimum Number of Bikes:", value = 22)
      )
    ),
    column(
      3,
      div(
        class = "custom-box",
        selectInput("metric", "Select a mode:",
          choices = c(
            "Number of Bikes",
            "Empty Places"
          ), selected = "Empty Places"
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$bikeMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -0.15, lat = 51.50009, zoom = 11)
  })


  observe({
    filtered <- data %>%
      filter(nbikes >= input$bikes)

    metric_values <- filtered[[metric_trans(input$metric)]]
    colors <- getColor(metric_values)
    icons <- makeAwesomeIcon(text = fa(icon_trans(input$metric)), markerColor = colors)

    leafletProxy("bikeMap", data = filtered) %>%
      clearMarkers() %>%
      clearControls() %>%
      addAwesomeMarkers(
        lng = ~longitude, lat = ~latitude,
        icon = icons,
        popup = ~ paste0(
          "<b>", name, "</b><br>",
          nbikes, " Bikes<br>",
          nempty, " Empty Places"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("red", "orange", "green"),
        labels = c("0–7", "8–15", "16+"),
        title = if (metric_trans(input$metric) == "nbikes") {
          "Number of bikes"
        } else {
          "Empty Places"
        }
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
