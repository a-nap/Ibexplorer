# Packages
library(shiny)
library(tidyverse)
library(shinythemes)
library(psych)
library(DT)

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

dat <- read_pcibex("results_dev.csv")

char_counts <- function(dat) {
  char_cols <- dat |> dplyr::select(where(is.character)) 
  
  result <- list()
  
  for (col in colnames(char_cols)) {
    summary_df <- char_cols |> 
      group_by(.data[[col]]) |>
      summarize(Count = n(), .groups = "drop")
    
    result[[col]] <- summary_df
  }
  
  return(result) 
}
  
char_summaries <- char_counts(dat)
char_summaries["MD5.hash.of.participant.s.IP.address"]

duration <-
  dat |>
  dplyr::select(IP, DURATION) |>
  group_by(IP) |>
  summarise(DURATION = max(DURATION))

# Define UI for the application
ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("Ibex Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      # File selector
      h3("Upload raw output file:"),
      fileInput(inputId = "raw.file",
                accept = c("text", "text/plain", ".txt", ".TXT", ".csv", ".CSV"),
                label = "Upload a CSV file (max. 30 MB)",
                buttonLabel = "Browse",
                placeholder = "No file selected"),
      actionButton("go", "Submit", class = "btn btn-info btn-block", icon = icon("gears")),
      hr(),
      # Column selector
      h3("Select which columns to include:"),
      uiOutput("column_selector"),
      hr(),
      p(strong("Download data as CSV table")),
      downloadButton(outputId = "downloadData", 
                     label = "Download",
                     class = "btn btn-block",
                     icon = icon("download"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table Preview",
                 DT::dataTableOutput("preview")
        ),
        tabPanel("Data summary",
                 h3("Summary of numerical columns"),
                 DT::dataTableOutput("dataSummary")
        )
      )
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
    checkboxGroupInput("selected_columns", "", 
                       choices = names(mydata()), 
                       selected = names(mydata()))
  })
  
  # Render an interactive DT table with dynamic filtering and pagination
  output$preview <- DT::renderDataTable({
    req(mydata())
    data_to_show <- mydata()
    if (!is.null(input$selected_columns)) {
      data_to_show <- data_to_show |>
        dplyr::select(input$selected_columns)
    }
    DT::datatable(data_to_show, 
                  options = list(pageLength = 20, 
                                 lengthMenu = c(20, 50, 100, 200),
                                 autoWidth = TRUE),
                  filter = "top",
                  rownames = FALSE)
  })
  
  # Render summary information: column types and summary stats for numeric columns
  output$dataSummary <- DT::renderDataTable({
    req(mydata())
    data <- mydata()
    numeric_data <- data |>
      dplyr::select(where(is.numeric)) |>
      describe()


    if (ncol(numeric_data) > 0) {
      DT::datatable(numeric_data, rownames = TRUE)
    } else {
      cat("No numeric columns found.\n")
    }
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
