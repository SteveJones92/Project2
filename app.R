library(shiny)
library(shinyalert)
library(tidyverse)

ui <- pageWithSidebar(
  headerPanel(
    "Placeholder title"
  ),
  sidebarPanel(
    "Placeholder sidebar text"
  ),
  mainPanel(
      "Placeholder main panel text"
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
}

# Run the application 
shinyApp(ui = ui, server = server)
