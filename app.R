library(shiny)
library(shinyalert)
library(tidyverse)
library(DT)

source("src/load_data.R")
source("src/data_exploration.R")

cat_list <- categorical_list(data)
num_list <- numerical_list(data)

ui <- pageWithSidebar(
  headerPanel(
    "Mobile Phone Data Usage"
  ),
  sidebarPanel(
    h2("Data Subset"),
    checkboxGroupInput("catGroup", "Categorical (min 2)", cat_list, selected = cat_list),
    uiOutput("cat_levels"),
    br(),
    uiOutput("num_one"),
    uiOutput("num_two"),
    selectInput("num_one", "Select Numerical Variable 1", choices = c("", num_list), selected = ""),
    uiOutput("num_one_range"),
    selectInput("num_two", "Select Numerical Variable 2", choices = c("", num_list), selected = ""),
    uiOutput("num_two_range"),
    actionButton("submit", "Submit"),
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
        "About",
        "Placeholder about text",
        br(),
        imageOutput("about_image"),
      ),
      tabPanel(
        "Data Download",
        DT::dataTableOutput("data_table"),
        br(),
        downloadLink('downloadData', 'Download'),
      ),
      tabPanel(
        "Data Exploration",
      ),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  display_data <- reactiveVal(data)
  selected_cat <- reactiveVal(cat_list)

  # watch for clicking the submit button and subset data by check group
  observeEvent(input$submit, {
    filtered_data <- data
    selected_levels <- lapply(selected_cat(), function(cat) {
      input[[paste0("select_", cat)]]
    })
    names(selected_levels) <- selected_cat()

    for (cat in selected_cat()) {
      filtered_data <- filtered_data |>
        filter(!!sym(cat) %in% selected_levels[[cat]])
    }

    if (input$num_one != "") {
      filtered_data <- filtered_data |>
        filter(!!sym(input$num_one) >= input$num_one_range[1] & !!sym(input$num_one) <= input$num_one_range[2])
    }
    if (input$num_two != "") {
      filtered_data <- filtered_data |>
        filter(!!sym(input$num_two) >= input$num_two_range[1] & !!sym(input$num_two) <= input$num_two_range[2])
    }

    selected_num <- NULL
    if (input$num_one == "" & input$num_two == "") {
      selected_num <- num_list
    } else {
      selected_num <- c(input$num_one, input$num_two)
    }
    selected_num <- selected_num[selected_num != ""]
    display_data(filtered_data |> select(all_of(selected_cat()), all_of(selected_num)))
  })

  # watch for checkbox group becoming less than 2 and undo the deselection
  observeEvent(input$catGroup, {
    if (length(input$catGroup) < 2) {
      updateCheckboxGroupInput(session, "catGroup", choices = cat_list, selected = selected_cat())
    } else {
      selected_cat(input$catGroup)
    }
  })

  output$cat_levels <- renderUI({
    lapply(input$catGroup, function(cat) {
      lvls <- data[[cat]]
      selectInput(paste0("select_", cat), label = paste("Select levels for", cat), choices = lvls, selected = lvls, multiple = TRUE)
    })
  })

  observeEvent(input$num_one, {
    updateSelectInput(session, "num_two", choices = c("", num_list[num_list != input$num_one]), selected = input$num_two)
  })

  output$num_one_range <- renderUI({
    if (input$num_one != "") {
      num_data <- data[[input$num_one]]
        sliderInput("num_one_range", paste("Select range for", input$num_one), min = min(num_data), max = max(num_data), value = c(min(num_data), max(num_data)))
    }
  })

  observeEvent(input$num_two, {
    updateSelectInput(session, "num_one", choices = c("", num_list[num_list != input$num_two]), selected = input$num_one)
  })

  output$num_two_range <- renderUI({
    if (input$num_two != "") {
      num_data <- data[[input$num_two]]
      sliderInput("num_two_range", paste("Select range for", input$num_two), min = min(num_data), max = max(num_data), value = c(min(num_data), max(num_data)))
    }
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0('data-', Sys.Date(), '.csv')
    },
    content = function(con) {
      write.csv(display_data(), con)
    }
  )
  
  output$data_table <- DT::renderDataTable({
    display_data()
  })

  output$about_image <- renderImage({
    list(src = "images/Mobile-Apps-Usage-Statistics-and-Amazing-Trends-1200x1107.jpg", contentType="image/jpg", alt = "About Section Image", width = 400, height = 369)
  }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
