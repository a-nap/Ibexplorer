# Packages
library(shiny)
library(tidyverse)
library(shinythemes)
library(psych)
library(DT)
library(bslib)

ibextheme <- bs_theme(
  fg = "#201010", 
  bg = "#ebe5e0", 
  primary = "#794729",
  secondary = "#7c6f42",
  info = "#342e1a"
)

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
  theme = ibextheme,
  sidebarLayout(
    sidebarPanel(
      img(src='ibex.svg'),
      h1("Ibex Explorer"),
      p("File converter for PCIbex results files."),
      
      # File selector
      fileInput(inputId = "raw.file",
                accept = c("text", "text/plain", ".txt", ".TXT", ".csv", ".CSV"),
                label = "Upload a CSV file (max. 30 MB)",
                buttonLabel = "Browse",
                placeholder = "No file selected"),
      actionButton("go", "Submit", class = "btn btn-block", icon = icon("gears")),
      downloadButton(outputId = "downloadData", 
                     label = "Download formatted CSV",
                     class = "btn-secondary btn-block",
                     icon = icon("download")),
      hr(),
      # Column selector
      p(strong("Include only these columns:")),
      uiOutput("column_selector"),
      hr(),
      # Search phrase input for filtering rows
      p(strong("Include only rows with this exact phrase:")),
      textInput(inputId = "search_phrase",
                label = "",
                value = ""),
      helpText("For example 'metadata' or 'experiment' or 'SelfPacedReadingParadigm'"),
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table Preview",
                 DT::dataTableOutput("preview")
        ),
        tabPanel("Data summary",
                 DT::dataTableOutput("dataSummary")
        ),
        # tabPanel("Participant overview",
        # DT::dataTableOutput("dataSummary")
        # ),
        tabPanel("Usage Guide",
                 # Convert the markdown text to HTML
                 HTML(markdown::markdownToHTML(text = "
### How to use this app

- Upload a unprocessed PCIbex output CSV file.
- Click the **Submit** button to process the file.
- (Optional) Select the columns to keep.
- (Optional) Enter a search phrase to filter rows.
- View the processed data in the preview tab.
- Download the filtered dataset by clicking **Download formatted CSV**.

### Troubleshooting

Follow these steps if you're having trouble uploading and processing your data.

- Ensure that your file is in CSV format.
- Check that the file size does not exceed 30MB.
- If no data appears, verify that the correct columns are selected.
- If no data appears, verify that the row filter phrase is correct.
- The explorer works only with the unmodified PCIbex results file.
- Contact the developer: Anna Prysłopska `anna . pryslopska [AT] gmail. com`

Sometimes the file encoding might be incorrect, but UTF-8 should usually work.
                 ", fragment.only = TRUE))
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
  
  # Reactive expression for filtered data based on column selection and search phrase
  filtered_data <- reactive({
    req(mydata())
    data <- mydata()
    # Subset columns as selected by the user
    if (!is.null(input$selected_columns)) {
      data <- data[, input$selected_columns, drop = FALSE]
    }
    # If a search phrase is provided, filter rows where any cell contains the phrase
    if (!is.null(input$search_phrase) && input$search_phrase != "") {
      data <- data[apply(data, 1, function(row) {
        any(grepl(input$search_phrase, as.character(row), ignore.case = TRUE))
      }), ]
    }
    return(data)
  })
  
  # Render an interactive DT table with dynamic filtering and pagination (using filtered data)
  output$preview <- DT::renderDataTable({
    req(filtered_data())
    DT::datatable(filtered_data(),
                  options = list(pageLength = 20,
                                 lengthMenu = c(20, 50, 100, 200),
                                 autoWidth = TRUE),
                  filter = "top",
                  rownames = FALSE)
  })
  
  # Render summary information: numeric summary for numeric columns of filtered data
  output$dataSummary <- DT::renderDataTable({
    req(filtered_data())
    data <- filtered_data()
    numeric_data <- data |> dplyr::select(where(is.numeric))
    if (ncol(numeric_data) > 0) {
      # Using the psych package's describe function for summary statistics
      summary_stats <- psych::describe(numeric_data)
      DT::datatable(summary_stats, rownames = TRUE)
    } else {
      # If no numeric columns, display a message
      DT::datatable(data.frame(Message = "No numeric columns found."), rownames = FALSE)
    }
  })
  
  # Download handler: writes the filtered data as a CSV file
  output$downloadData <- downloadHandler(
    filename = function() {
      "formatted_data.csv"
    },
    content = function(file) {
      req(filtered_data())
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
