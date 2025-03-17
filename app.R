# Packages
library(shiny)
library(tidyverse)
library(shinythemes)

# Function

read_pcibex <-
  function(filepath,
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
        if (length(line) == 0) {
          break
        }
        m <-
          regmatches(line, regexec("^# (\\d+)\\. (.+)\\.$", line))[[1]]
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
          if (index == n.cols) {
            break
          }
        }
      }
      close(con)
      return(read.csv(
        filepath,
        comment.char = "#",
        header = FALSE,
        col.names = cols
      ))
    }
    else{
      return(read.csv(
        filepath,
        comment.char = "#",
        header = FALSE,
        col.names = seq(1:n.cols)
      ))
    }
  }






# Define UI for application that draws a histogram
ui <- fluidPage(
  # Theme
  theme = shinytheme("yeti"),
  
  # Application title
  titlePanel("Format a PCIbex results file"),

  sidebarLayout(
  #   # Sidebar with a slider input for number of bins 
    sidebarPanel(
      h4("Select raw output file"),
    # Input raw PCIbex text file
      fileInput(inputId = "raw.file",
              accept = c(
                "text",
                "text/plain",
                ".txt",
                ".TXT",
                ".csv",
                ".CSV"),
              label = "Upload a csv file (max. 30 MB)",
              buttonLabel = "Browse",
              placeholder = "No file selected"),
    
    actionButton("go", "Submit", class = "btn btn-info btn-block", icon = shiny::icon("gears")),
    hr(),
    p(strong("Download data as CSV table")),
    
    # Download data
    downloadButton(outputId = "downloadData", 
                   label = "Download",
                   class = "btn btn-block",
                   icon = shiny::icon("download"))
    
    ),
        # Show a table preview of the raw.file
        mainPanel(
          h2("Table preview"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Process data file and create a table
  mydata <- eventReactive(input$go, {
    inFile <- input$raw.file
    if (is.null(inFile))
      return(NULL)
    

    
    raw.file <- read_pcibex(inFile$datapath, locale(encoding="utf8"))
      })
}

# Run the application 
shinyApp(ui = ui, server = server)