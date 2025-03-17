# Packages
library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)  # for interactive tables

# Function to process PCIbex file
read_pcibex <- function(filepath,
                        auto.colnames = TRUE,
                        fun.col = function(col, cols) {
                          cols[cols == col] <- paste(col, "Ibex", sep = ".")
                          return(cols)
                        }) {
  n.cols <- max(count.fields(filepath, sep = ",", quote = NULL), na.rm = TRUE)
  if (auto.colnames) {
    cols <- c()
    con <- file(filepath, "r")
    while (TRUE) {
      line <- readLines(con, n = 1, warn = FALSE)
      if (length(line) == 0) break
      m <- regmatches(line, regexec("^# (\\d+)\\. (.+)\\.$", line))[[1]]
      if (length(m) == 3) {
        index <- as.numeric(m[2])
        value <- m[3]
        if (index < length(cols)) {
          cols <- c()
        }
        if (is.function(fun.col)) {
          cols <- fun.col(value, cols)
        }
        cols[index] <- value
        if (index == n.cols) break
      }
    }
    close(con)
    return(read.csv(filepath, comment.char = "#", header = FALSE, col.names = cols))
  } else {
    return(read.csv(filepath, comment.char = "#", header = FALSE, col.names = seq(1, n.cols)))
  }
}

# Define UI for the application
ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("Ibex Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select raw output file:"),
      fileInput(inputId = "raw.file",
                accept = c("text", "text/plain", ".txt", ".TXT", ".csv", ".CSV"),
                label = "Upload a CSV file (max. 30 MB)",
                buttonLabel = "Browse",
                placeholder = "No file selected"),
      actionButton("go", "Submit", class = "btn btn-info btn-block", icon = icon("gears")),
      hr(),
      # Dynamic UI for selecting columns to keep
      h4("Select which columns to include:"),
      uiOutput("column_selector"),
      hr(),
      p(strong("Download data as CSV table")),
      downloadButton(outputId = "downloadData", 
                     label = "Download",
                     class = "btn btn-block",
                     icon = icon("download"))
    ),
    mainPanel(
      h2("Interactive Table Preview"),
      DT::dataTableOutput("preview")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Process the file when the "Submit" button is clicked
  mydata <- eventReactive(input$go, {
    inFile <- input$raw.file
    if (is.null(inFile)) return(NULL)
    # Apply the read_pcibex function to the uploaded file
    read_pcibex(inFile$datapath, auto.colnames = TRUE)
  })
  
  # Create dynamic UI for selecting columns based on processed data
  output$column_selector <- renderUI({
    req(mydata())
    checkboxGroupInput("selected_columns", "Select columns to include:", 
                       choices = names(mydata()), 
                       selected = names(mydata()))
  })
  
  # Render an interactive DT table with dynamic filtering and pagination
  output$preview <- DT::renderDataTable({
    req(mydata())
    data_to_show <- mydata()
    if (!is.null(input$selected_columns)) {
      data_to_show <- data_to_show[, input$selected_columns, drop = FALSE]
    }
    DT::datatable(data_to_show, 
                  options = list(pageLength = 20, 
                                 lengthMenu = c(20, 50, 100, 200),
                                 autoWidth = TRUE),
                  filter = "top",
                  rownames = FALSE)
  })
  
  # Download handler to download the processed data as CSV with only selected columns
  output$downloadData <- downloadHandler(
    filename = function() {
      "formatted_data.csv"
    },
    content = function(file) {
      req(mydata())
      data_to_download <- mydata()
      if (!is.null(input$selected_columns)) {
        data_to_download <- data_to_download[, input$selected_columns, drop = FALSE]
      }
      write.csv(data_to_download, file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
# 
# PCIbex Explorer
# PCIbex Analyzer
# PCIbex Insights
# PCIbex Dashboard
# PCIbex Data Studio
# PCIbex QuickView
# PCIbex Companion
# PCIbex Navigator
# PCIbex Wrangler
# PCIbex Interactive
# Ibex Insight Viewer
# Ibex Data Companion
# Ibex Analyzer Pro
# Ibex Data Navigator
# 
# Ibex Summary Station
# Data Ibex Toolbox
# PCIbex Filter & Download
# PCIbex Insight Engine
# PCIbex Smart Viewer