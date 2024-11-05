library(shiny)
library(shinyalert)
library(tidyverse)
library(DT)

source("src/load_data.R")

ui <- pageWithSidebar(
  headerPanel(
    "Placeholder title"
  ),
  sidebarPanel(
    "Placeholder sidebar text"
  ),
  mainPanel(
      "Placeholder main panel text",
      downloadLink('downloadData', 'Download'),
      DT::dataTableOutput("data_table"),
      imageOutput("about_image")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(data, con)
    }
  )
  
  output$data_table <- DT::renderDataTable({
    data
  })

  output$about_image <- renderImage({
    list(src = "images/Mobile-Apps-Usage-Statistics-and-Amazing-Trends-1200x1107.jpg", contentType="image/jpg", alt = "About Section Image", width = 1200, height = 1107)
  }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
