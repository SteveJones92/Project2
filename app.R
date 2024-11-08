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
        imageOutput("about_image"),
        h3("Purpose"),
        hr(),
        "The purpose of this app is to provide a dynamic way to explore the provided dataset (mobile device user data).
        Possible interesting comparisons that can be made is androids vs iphones across categories of battery drain, app and data usage, screen time, age, and gender.",
        br(),
        h3("Data and Source"),
        hr(),
        "The dataset is generated/simulated from general mobile usage trends and is not raw data. As such, certain information should not be compared, such as the popularity of different devices, relational comparisons of categorical variables.
        The data can be checked for general values of numerical variables and general trends of usage data.",
        br(),
        br(),
        a("https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset/data", href = "https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset/data"),
        br(),
        h3("Sidebar and Tab Descriptions"),
        hr(),
        h4("Sidebar"),
        "The sidebar is used to subset the data. Selecting the submit button is required to update the data to the current selection.",
        h5(markdown("___Categorical & Levels___")),
        "The categorical variables are selectable down to a minimum of 2 items. If trying to select less than 2, the selection will be forced to the current selection.
        The levels of each categorical selection are displayed as well (defaulting to all levels). To remove a level, select it and press backspace/delete.",
        h5(markdown("___Numerical___")),
        "There are 2 numeric variable boxes. They default to having no selection. If no selection is made, all numeric variables are used. If a selection is made, the range sliders will appear. It doesn't matter
        which selection is used first, but a selection in one will remove the option from the other.",
        br(),
        br(),
        h4("Tabs"),
        "There are 3 selectable tabs in the main window.",
        h5(markdown("___About___")),
        "A description of the application and it's features.",
        h5(markdown("___Data Download___")),
        "An area to view and download the subsetted data.",
        h5(markdown("___Data Exploration___")),
        "An area to explore the data through plots and summaries of plot data. There are 6 plots available to choose from.",
        # list of items
        markdown(
          "
          1. Boxplot - 1 numeric, 1 categorical for fill, 1 categorical for facet, and an option to flip the display axis
          2. Histogram - 1 numeric, 1 categorical for fill, and 1 categorical for facet
          3. Scatterplot - 2 numeric, 1 categorical for fill, and 1 categorical for facet
          4. Density Plot - 1 numeric, 1 categorical for fill, and 1 categorical for facet
          5. Radar Plot - 1 categorical, the rest of numeric variables will be used in the spider plot
          6. Pairs Plot - Requires 2 numeric variable selections, and draws the pairs() plot for quick viewing of numeric plots against each other.
          
          After most plots are categorical and numerical selection for summaries. The pairs plot is for visual purposes only.
          * Categorical summaries will be table counts based on selected categorical variables. If none are selected, then it will be the total count of all the rows.
          * Numerical summaries will be mean, median, sd, and IQR. There is no numerical summary for the radar plot, but instead it will get text describing the data.
          "
        ),
        br(),
        hr(),
        h3("Data Exploration Summary"),
        markdown("
        Looking at the Radar Plot, a few standout items:
        * Samsung Galaxy S21 has a slightly older population mean.
        * iOS users use their devices slightly more than Android users.
        * Males use ever so slightly more data.
        * Probably the most interesting, age is not a factor in the level of user (light - extreme usage).

        The pairs plot shows the same store as the radar plot for age. The scatterplot and pairs plot shows an artifact
        of data generation as well. The data would be a lot more interesting if it were more naturally collected and had
        more interesting categories to compare. I was hoping to find popularity of devices and battery efficiency type data,
        but that isn't possible with this data.
        "),
        br(),
        br()
      ),
      tabPanel(
        "Data Download",
        h3("Data Table Subset By Sidebar Selections"),
        "Select the download button below the data table to download the data subset as a csv file.",
        hr(),
        DT::dataTableOutput("data_table"),
        hr(),
        br(),
        downloadLink('downloadData', 'Download'),
      ),
      tabPanel(
        "Data Exploration",
        hr(),
        uiOutput("checkbox"),
        uiOutput("plots")
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

  output$checkbox <- renderUI({
    cat_options <- names(display_data())[names(display_data()) %in% cat_list]
    num_options <- names(display_data())[names(display_data()) %in% num_list]

    fluid_rows <- list(
      fluidRow(
        column(3, checkboxInput("boxplot", "Boxplot")),
        conditionalPanel(
          condition = "input.boxplot == true",
          column(2, selectInput("boxplot_x_var", "Select X (Numerical)", choices = num_options, selected = num_options[1])),
          column(2, selectInput("boxplot_y_var", "Select Y (Categorical)", choices = c("", cat_options), selected = "")),
          column(2, selectInput("boxplot_facet_var", "Select Facet (Categorical)", choices = c("", cat_options), selected = "")),
          column(3, checkboxInput("flip", "Flip Axes"))
        )
      ),
      fluidRow(
        column(3, checkboxInput("histogram", "Histogram")),
        conditionalPanel(
          condition = "input.histogram == true",
          column(3, selectInput("hist_x_var", "Select X (Numerical)", choices =  num_options, selected = num_options[1])),
          column(3, selectInput("hist_y_var", "Select Y (Categorical)", choices = c("", cat_options), selected = "")),
          column(3, selectInput("hist_facet_var", "Select Facet (Categorical)", choices = c("", cat_options), selected = ""))
        )
      ),
      fluidRow(
        column(3, checkboxInput("scatterplot", "Scatterplot")),
        conditionalPanel(
          condition = "input.scatterplot == true",
          column(2, selectInput("scatter_x_var", "Select X (Numerical)", choices = num_options, selected = num_options[1])),
          column(2, selectInput("scatter_y_var", "Select Y (Numerical)", choices = num_options, selected = num_options[2])),
          column(2, selectInput("scatter_color_var", "Select Fill (Categorical)", choices = c("", cat_options), selected = "")),
          column(2, selectInput("scatter_facet_var", "Select Facet (Categorical)", choices = c("", cat_options), selected = ""))
        )
      ),
      fluidRow(
        column(3, checkboxInput("densityplot", "Density Plot")),
        conditionalPanel(
          condition = "input.densityplot == true",
          column(3, selectInput("density_x_var", "Select X (Numerical)", choices = num_options, selected = num_options[1])),
          column(3, selectInput("density_fill_var", "Select Fill (Categorical)", choices = c("", cat_options), selected = "")),
          column(3, selectInput("density_facet_var", "Select Facet (Categorical)", choices = c("", cat_options), selected = ""))
        )
      ),
      fluidRow(
        column(3, checkboxInput("radarplot", "Radar Plot")),
        conditionalPanel(
          condition = "input.radarplot == true",
          column(9, selectInput("radar_cat_var", "Select Categorical Variable", choices = cat_options, selected = cat_options[1]))
        )
      ),
      fluidRow(
        column(3, checkboxInput("pairsplot", "Pairs Plot")),
      )
    )
    fluid_rows
  })

  output$plots <- renderUI({
    if (any(sapply(list(input$boxplot, input$histogram, input$scatterplot, input$densityplot, input$radarplot, input$pairsplot), is.null))) {
      return(NULL)
    }
    plot_list <- list()

    if (input$boxplot && input$boxplot_x_var != "") {
      plot_list$boxplot <- renderPlot({
        fill_var <- if(input$boxplot_y_var == "") NULL else input$boxplot_y_var
        facet_var <- if(input$boxplot_facet_var == "") NULL else input$boxplot_facet_var
        boxplot(display_data(), input$boxplot_x_var, fill_var, facet_var, input$flip)
      })
      plot_list$boxplot_summary_cat <- column(4, checkboxInput("boxplot_summary_cat", "Show Categorical Table"))
      plot_list$boxplot_summary_num <- column(8, checkboxInput("boxplot_summary_num", "Show Numerical Summaries"))
      plot_list$boxplot_summary_tables <- fluidRow(
        column(4, conditionalPanel(
          condition = "input.boxplot_summary_cat == true",
          tableOutput("boxplot_summary_cat_table")
        )),
        column(8, conditionalPanel(
          condition = "input.boxplot_summary_num == true",
          dataTableOutput("boxplot_summary_num_table")
        ))
      )
    }
    if (input$histogram && input$hist_x_var != "") {
      plot_list$histogram <- renderPlot({
        fill_var <- if(input$hist_y_var == "") NULL else input$hist_y_var
        facet_var <- if(input$hist_facet_var == "") NULL else input$hist_facet_var
        histogram(display_data(), input$hist_x_var, fill_var, facet_var)
      })
      plot_list$histogram_summary_cat <- column(4, checkboxInput("histogram_summary_cat", "Show Categorical Table"))
      plot_list$histogram_summary_num <- column(8, checkboxInput("histogram_summary_num", "Show Numerical Summaries"))
      plot_list$histogram_summary_tables <- fluidRow(
        column(4, conditionalPanel(
          condition = "input.histogram_summary_cat == true",
          tableOutput("histogram_summary_cat_table")
        )),
        column(8, conditionalPanel(
          condition = "input.histogram_summary_num == true",
          dataTableOutput("histogram_summary_num_table")
        ))
      )
    }
    if (input$scatterplot && input$scatter_x_var != "" && input$scatter_y_var != "") {
      plot_list$scatterplot <- renderPlot({
        color_var <- if(input$scatter_color_var == "") NULL else input$scatter_color_var
        facet_var <- if(input$scatter_facet_var == "") NULL else input$scatter_facet_var
        scatterplot(display_data(), input$scatter_x_var, input$scatter_y_var, color_var, facet_var)
      })
      plot_list$scatterplot_summary_cat <- column(4, checkboxInput("scatterplot_summary_cat", "Show Categorical Table"))
      plot_list$scatterplot_summary_num <- column(8, checkboxInput("scatterplot_summary_num", "Show Numerical Summaries"))
      plot_list$scatterplot_summary_tables <- fluidRow(
        conditionalPanel(
          condition = "input.scatterplot_summary_cat == true",
          tableOutput("scatterplot_summary_cat_table")
        ),
        conditionalPanel(
          condition = "input.scatterplot_summary_num == true",
          dataTableOutput("scatterplot_summary_num_table")
        )
      )
    }
    if (input$densityplot && input$density_x_var != "") {
      plot_list$densityplot <- renderPlot({
        fill_var <- if(input$density_fill_var == "") NULL else input$density_fill_var
        facet_var <- if(input$density_facet_var == "") NULL else input$density_facet_var
        density_plot(display_data(), input$density_x_var, fill_var, facet_var)
      })
      plot_list$densityplot_summary_cat <- column(4, checkboxInput("densityplot_summary_cat", "Show Categorical Table"))
      plot_list$densityplot_summary_num <- column(8, checkboxInput("densityplot_summary_num", "Show Numerical Summaries"))
      plot_list$densityplot_summary_tables <- fluidRow(
        column(4, conditionalPanel(
          condition = "input.densityplot_summary_cat == true",
          tableOutput("densityplot_summary_cat_table")
        )),
        column(8, conditionalPanel(
          condition = "input.densityplot_summary_num == true",
          dataTableOutput("densityplot_summary_num_table")
        ))
      )
    }
    if (input$radarplot && input$radar_cat_var != "") {
      plot_list$radarplot <- renderPlot({
        radar_plot(display_data(), input$radar_cat_var)
      })
      plot_list$radarplot_summary_cat <- column(4, checkboxInput("radarplot_summary_cat", "Show Categorical Table"))
      plot_list$radarplot_summary_num <- column(8, checkboxInput("radarplot_summary_num", "Show Numerical Summary"))
      plot_list$radarplot_summary_tables <- fluidRow(
        column(4, conditionalPanel(
          condition = "input.radarplot_summary_cat == true",
          tableOutput("radarplot_summary_cat_table")
        )),
        column(8, conditionalPanel(
          condition = "input.radarplot_summary_num == true",
          uiOutput("radarplot_summary_text")
        ))
      )
    }
    if (input$pairsplot) {
      plot_list$pairsplot <- renderPlot({
        pairs_data <- display_data() |> select(where(is.numeric))
        if (length(pairs_data) < 2) {
          return(text(0.5, 0.5, "Less than 2 numeric variables present, cannot show pairs plot.", cex = 1.5))
        }
        pairs(pairs_data)
      })
    }

    do.call(tagList, plot_list)
  })

  output$boxplot_summary_cat_table <- renderTable({
    contingency_table(display_data(), input$boxplot_y_var, input$boxplot_facet_var)
  })
  output$boxplot_summary_num_table <- renderDataTable({
    numerical_summaries(display_data(), input$boxplot_x_var, input$boxplot_y_var, input$boxplot_facet_var)
  })
  output$histogram_summary_cat_table <- renderTable({
    contingency_table(display_data(), input$hist_y_var, input$hist_facet_var)
  })
  output$histogram_summary_num_table <- renderDataTable({
    numerical_summaries(display_data(), input$hist_x_var, input$hist_y_var, input$hist_facet_var)
  })
  output$scatterplot_summary_cat_table <- renderTable({
    contingency_table(display_data(), input$scatter_color_var, input$scatter_facet_var)
  })
  output$scatterplot_summary_num_table <- renderDataTable({
    numerical_summaries(display_data(), input$scatter_x_var, input$scatter_y_var, input$scatter_facet_var)
  })
  output$densityplot_summary_cat_table <- renderTable({
    contingency_table(display_data(), input$density_fill_var, input$density_facet_var)
  })
  output$densityplot_summary_num_table <- renderDataTable({
    numerical_summaries(display_data(), input$density_x_var, input$density_fill_var, input$density_facet_var)
  })
  output$radarplot_summary_cat_table <- renderTable({
    contingency_table(display_data(), input$radar_cat_var)
  })
  output$radarplot_summary_text <- renderUI({
    selected_category <- input$radar_cat_var
    category_levels <- levels(display_data()[[selected_category]])
    category_str <- paste(category_levels, collapse=", ")

    min_data <- display_data() |> group_by(!!sym(selected_category)) |> select(where(is.numeric)) |> summarise(across(everything(), min))
    max_data <- display_data() |> group_by(!!sym(selected_category)) |> select(where(is.numeric)) |> summarise(across(everything(), max))
    mean_data <- display_data() |> group_by(!!sym(selected_category)) |> select(where(is.numeric)) |> summarise(across(everything(), ~round(mean(.), 2)))

    min_data <- min_data[-1]
    max_data <- max_data[-1]
    mean_data <- mean_data[-1]
    summary_text <- paste0("The numeric variable ", names(min_data), " falls at the ratio of ", round((mean_data - min_data) / (max_data - min_data), 2), "<br>")
    HTML("This is the order for the following array values:", "<br>", category_str, "<br>", "<br>", paste(
      "Min: ", min_data,
      "Max: ", max_data,
      "Mean: ", mean_data, "<br>",
      summary_text, "<br>", collapse="<br>"
    ))
  })

  output$data_table <- DT::renderDataTable({
    display_data()
  })

  output$about_image <- renderImage({
    list(src = "images/Mobile-Apps-Usage-Statistics-and-Amazing-Trends-1200x1107.jpg", contentType="image/jpg", alt = "About Section Image", width = 400, height = 369)
  }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
