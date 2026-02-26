# Packages
library(shiny)
library(tidyverse)
library(shinythemes)
library(timevis)
library(psych)
library(DT)
library(bslib)


options(shiny.maxRequestSize = 30*1024^2)

ibextheme <- bs_theme(
  fg = "#201010", 
  bg = "#ebe5e0", 
  primary = "#794729",
  secondary = "#201010", 
  info = "#7c6f42"
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

# Standard error of the mean
sem <- function(x) { sd(x,na.rm=T) / sqrt(length(x)); }


# UI ----------------------------------------------------------------------

# Define UI for the application
ui <- fluidPage(
  theme = ibextheme,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "ibex.css")
  ),
  sidebarLayout(
    
    ## Sidebar -----------------------------------------------------------------
    
    sidebarPanel(
      width = 3,
      img(src='ibex.svg'),
      h1("Ibex Explorer"),
      p("File converter for PCIbex results files."),
      
      ### File selector -----------------------------------------------------------
      
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
      
      ### Column selector ---------------------------------------------------------
      
      p(strong("Include only these columns:")),
      uiOutput("column_selector"),
      hr(),
      
      ### Search phrase input for filtering rows ----------------------------------
      
      textInput(inputId = "search_phrase",
                label = tagList(p(strong("Include only rows with this exact phrase:"))),
                value = ""),
      helpText("For example 'metadata' or 'experiment' or 'SelfPacedReadingParadigm'."),
      
    ),
    
    ## Main panel --------------------------------------------------------------
    
    mainPanel(
      
      ### Table preview -----------------------------------------------------------
      
      tabsetPanel(
        tabPanel(title=tagList(icon("table"),"Table preview"),
                 fluidRow(
                   card(
                     height="1000px",
                   h3("Preprocessed data preview"),
                   DT::dataTableOutput("preview")
                   )
                 )),
        
        ### Data summary ------------------------------------------------------------
        
        tabPanel(title=tagList(icon("chart-simple"),"Data summary"),
                 card(
                   h3("Numerical data overview"),
                   DT::dataTableOutput("dataSummary")),
                 
                 card(
                   h3("Custom variable overview"),
                   fluidRow(
                     column(width = 3,
                            helpText("For example 'List' or 'Condition'."),
                            textInput(
                              inputId = "custom_var_name",
                              label   = "Type your variable name here:",
                              value   = "",
                              width   = "100%"),
                            helpText("For example 'Start' or 'undefined' or 'NULL'."),
                            textInput(
                              inputId = "exclude_var_list",
                              label   = "Exclude values (comma-separated):",
                              value   = "",
                              width   = "100%"),
                     
                     checkboxInput(
                       inputId = "remove_na",
                       label   = "Remove missing values",
                       value   = FALSE
                     ),
                     
                     uiOutput("duration_zoom_ui")
                   ),
                     column(width = 8,
                            plotOutput("varPlot"), 
                            plotOutput("varDurationPlot"))
                   ))
        ),
        
        ### Participant overview ----------------------------------------------------
        
        tabPanel(title=tagList(icon("users"),"Participant overview"),
                 fluidRow(
                   column(
                     width=4,
                     value_box(
                       title = "Participant count", 
                       value = textOutput("participant_count"), 
                       theme = value_box_theme(
                         bg = "#201010",
                         fg = "#EBE5E0"
                       ),
                       showcase = fontawesome::fa_i("users"), 
                       showcase_layout = "left center",
                       full_screen = FALSE, fill = TRUE, height = NULL
                     )),
                   
                   column(
                     width=4,
                     value_box(
                       title = "Avg Trials / Participant", 
                       value = textOutput("average_trial"), 
                       theme = value_box_theme(
                         bg = "#7c6f42",
                         fg = "#EBE5E0"
                       ),
                       showcase = fontawesome::fa_i("list-check"), 
                       showcase_layout = "left center",
                       full_screen = FALSE, fill = TRUE, height = NULL
                     )),
                   
                   column(
                     width=4,
                     value_box(
                       title = "Median duration in minutes (SD)", 
                       value = textOutput("median_duration"), 
                       theme = value_box_theme(
                         bg = "#794729",
                         fg = "#EBE5E0"
                       ),
                       showcase = fontawesome::fa_i("hourglass-end"), 
                       showcase_layout = "left center",
                       full_screen = FALSE, fill = TRUE, height = NULL
                     )),
                 ),
                 fluidRow(
                   card(
                   h3("Data preview"),
                   fluidRow(
                   column(width = 6,
                          plotOutput("participantPlot", 
                                     height = "500px")
                   ),
                   column(width = 6,
                          plotOutput("participantDurationHistogramPlot",
                                     height = "500px")
                   )))
                 ),
                 fluidRow(
                   card(
                   column(h3("Data collection timeline"),
                          width = 12, timevisOutput("participantTimeline"))
                 )),
                 fluidRow(
                   card(
                   column(h3("Participant summary"),
                          width = 12, DT::dataTableOutput("participantSummary"))
                   
                 ))
        ),
        
        ### Usage guide -------------------------------------------------------------
        
        tabPanel(title=tagList(icon("circle-info"),"Usage Guide"),
                 fluidRow(
                     column(width=6,
                     card(
                       fluidRow(h3("How to use this app"),
                     column(width=10,
                            
                 HTML(markdown::markdownToHTML(text = "
The recommended workflow is as follows:
                 
###### 1. **Upload:** Select a raw PCIbex CSV results file.
###### 2. **Process:** Click the 'Submit' button to process the file.
###### 3. **Preview:** View the processed data in the 'Table preview', 'Data summary', and 'Participant overview' tabs. 
###### 4. **Filter:**

- (Optional) In the sidebar, select the columns you want to include.
- (Optional) In the sidebar, enter a search phrase to filter rows.

###### 5. **Download:** Download the filtered dataset by clicking 'Download formatted CSV'.
                                               
Any filter that was applied to the data will be applied to the downloaded file.", 
                                               fragment.only = TRUE)
                 )), # end column
                 column(width=2,
                        img(src='flowchart.svg', style="min-width: 30%; width: 90%")
                 ))), # end card
                 ),
column(width=6,
       card(h3("Troubleshooting"),
            HTML(markdown::markdownToHTML(text = "
Follow these steps if you're having trouble uploading and processing your data.

- Ensure that your file is the unmodified PCIbex results file in CSV format.
- Check that the file size does not exceed 30MB.
- Check the file encoding. UTF-8 should usually work.
- If no data appears, verify that the correct columns are selected (e.g. 'Results.reception.time', 'EventTime', 'MD5.hash.of.participant.s.IP.addres').
- If no data appears, verify that the row filter phrase is correct.
- It's always a good idea to double-check the spelling.
- The plots might take a few seconds to load. If no plots appear or there are errors, ensure that you have not unchecked required columns or filtered out required values in the sidebar.
- Contact the developer: Anna Prysłopska `anna . pryslopska [AT] gmail. com`
                             ", fragment.only = TRUE)
                 ) # end HTML
            ))     # end column
),
fluidRow(
  column(width=12,
         card(h3("Data preview"),
              accordion(
                accordion_panel(
                  value="table",
                  title = tagList(
                  icon("table"),
                  " Table preview"
                ),
                                HTML(markdown::markdownToHTML(text = "
This tab shows the preview of the preprocessed file. 
This is the data that will be saved after clicking on 'Download formatted CSV'.
All exlusion and inclusion criteria are applied. 
The data summaries in the other tabs will not be saved.", fragment.only = TRUE
                                ))
                                ),
                accordion_panel(
                  value="data summary",
                  title=tagList(icon("chart-simple"),"Data summary"),
                                HTML(markdown::markdownToHTML(text = "
This tab shows a summary of all numerical data and plots for counts and durations of a custom variable.
This information can be useful for checking whether there is an equal amount of lists, conditions, and items in the recorded data.

- **Occurrences** bar plot shows how many times each group appears in the data. Helps identify unbalanced data.
- **Average duration** boxplot shows the duration in minutes of each trial for each group. Helps identify cases which take longer or shorter on average.
- Enter the variable name to plot.
- (Optional) Input comma-separated values in the second text field to exclude values from the plots. This can also be a space, NULL, Start, End, or any other value.
- (Optional) Use the slider to zoom in and out on duration ranges in the boxplot. The slider appears only after data upload.
- Keep or remove missing values (NA) in the checkbox.
- If the app does not detect your list variable, you can specify it in the text field.

You can download the plots by right-clicking on them and selecting 'Save image as...'.
", fragment.only = TRUE
                                ))
                                ),
                accordion_panel(
                  value="participants",
                  title=tagList(icon("users"),"Participant overview"),
                                HTML(markdown::markdownToHTML(text = "
This tab shows two plots and a summary table of counts and durations.
This information is useful for determining participants that are outside of the normal range (took the experiment several times, and took too long or too short to complete the experiment).

- **Participant counts** bar plot shows the number of trials per participant. Should probably be the same number for each participant. Plot height adjusts automatically if there are many participants.
- **Participant durations** histogram with a density line shows the distribution of total participant durations in minutes. Helps identify participants who took much longer or shorter than average. The dashed vertical line shows the mean duration. Dotted lines show ±2 standard deviations from the mean; the lower bound is capped at 0.

You can download the plots by right-clicking on them and selecting 'Save image as...'.", fragment.only = TRUE
                                ))
                                )
                ), # end accordion

              
              ))
  ), # end row

                 card(
                 HTML(markdown::markdownToHTML(text = "
### Version


- **1.02**: Formatted plots; changed condition and list plots to use custom input with data selection.  
- **1.01**: Added plots for conditions, lists, and participants.  
- **1.00**: First version.

### Links

- [GitHub page](https://github.com/a-nap/Ibexplorer)
- [Project page](https://pryslopska.com/projects/ibexplorer/)

                 ", fragment.only = TRUE))
                 
        )
      )
    ))
  ),
  div(id = "bottom-svg", tags$img(src = "mountains.svg"))
)


# SERVER ------------------------------------------------------------------

# Define server logic
server <- function(input, output, session) {
  
  ## Data input processing ---------------------------------------------------
  
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
  
  
  ## Table preview -----------------------------------------------------------
  
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
      summary_stats <- psych::describe(numeric_data) |> 
        mutate(across(everything())) |>
        select(-vars, -trimmed, -mad, -skew, -kurtosis) |>
        mutate(mean = round(mean, 2),
               sd = round(sd, 2),
               se = round(se, 2))
      DT::datatable(summary_stats, rownames = TRUE)
    } else {
      # If no numeric columns, display a message
      DT::datatable(data.frame(Message = "No numeric columns found."), rownames = FALSE)
    }
  })
  
  ## Custom variable info ----------------------------------------------------
  
  # Variable count plot
  output$varPlot <- renderPlot({
    req(filtered_data())
    
    data <- filtered_data() |>
      rename_with(~ make.unique(tolower(.)))
    
    # Find list column
    col_names <- tolower(names(data))
    
    
    if (!is.null(custom_var()) && trimws(custom_var()) != "") {
      user_col <- tolower(custom_var())
      if (user_col %in% col_names) {
        var_col <- names(data)[which(col_names == user_col)[1]]
      } else {
        var_col <- NULL
      }
    } else {
      # fallback logic: if no variable is chosen then look for lists or conditions
      var_col <- if ("list" %in% col_names) {
        names(data)[which(col_names == "list")[1]]
      } else if ("group" %in% col_names) {
        names(data)[which(col_names == "group")[1]]
      } else if ("condition" %in% col_names) {
        names(data)[which(col_names == "condition")[1]]
      } else if ("treatment" %in% col_names) {
        names(data)[which(col_names == "treatment")[1]]
      } else if ("item" %in% col_names) {
        names(data)[which(col_names == "item")[1]]
      } else {
        NULL
      }
    }
    
    # Stop if no valid variable found
    if (is.null(var_col)) {
      stop("No valid variable found.")
    }
    
    
    # Group by the found column and summarize
    data <- data |>
      group_by(.data[[var_col]]) |>
      summarize(count = n(), .groups = 'drop')
    
    # Optionally remove missing values
    if (input$remove_na) {
      data <- na.omit(data)
    }
    
    # Optionally remove values
    excl <- exclude_var_list() 
    if (!is.null(excl)) {
      data <- data %>%
        filter(!(.data[[var_col]] %in% excl))
    }
    
    # Calculate the mean and standard deviation
    mean_value <- mean(data$count, na.rm = TRUE)
    sem_value   <- sem(data$count)
    ymin = mean_value - sem_value
    ymax = mean_value + sem_value
    
    # Set up for plotting
    grouping_column <- var_col
    data[[grouping_column]] <- as.factor(data[[grouping_column]])
    
    
    # Generate the bar plot
    ggplot(data, 
           aes(x = .data[[grouping_column]], y = count)) +
      annotate(
        "rect",
        xmin = -Inf, xmax = Inf,
        ymin = ymin,
        ymax = ymax,
        fill = "#342e1a",
        alpha = 0.15
      ) +
      geom_bar(stat = "identity", fill = "#794729") +
      geom_text(aes(label = count), 
                vjust = -0.3, 
                size = 4, 
                color = "#201010") +
      
      geom_hline(
        yintercept = mean_value,
        color = "#201010",
        linewidth = 1,
        linetype = "dashed"
      ) +
      labs(
        x = tools::toTitleCase(grouping_column),
        y = "Row count",
        title = paste0("Occurrences of each ",grouping_column," in the data")
      ) +
      theme_bw() +
      theme(
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#ebe5e0", color = NA),
        panel.grid.major = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank()
      )
  })
  
  
  # Duration plot
  output$varDurationPlot <- renderPlot({
    req(filtered_data())
    
    data <- filtered_data() |>
      rename_with(~ make.unique(tolower(.)))
    
    # Find list column
    col_names <- names(data)
    
    if (!is.null(custom_var()) && trimws(custom_var()) != "") {
      user_col <- tolower(custom_var())
      if (user_col %in% col_names) {
        var_col <- names(data)[which(col_names == user_col)[1]]
      } else {
        var_col <- NULL
      }
    } else {
      # fallback logic
      var_col <- if ("list" %in% col_names) {
        names(data)[which(col_names == "list")[1]]
      } else if ("group" %in% col_names) {
        names(data)[which(col_names == "group")[1]]
      } else if ("condition" %in% col_names) {
        names(data)[which(col_names == "condition")[1]]
      } else if ("treatment" %in% col_names) {
        names(data)[which(col_names == "treatment")[1]]
      } else if ("item" %in% col_names) {
        names(data)[which(col_names == "item")[1]]
      } else {
        NULL
      }
    }
    
    # Stop if no valid list column found
    if (is.null(var_col)) {
      stop("No valid variable column found.")
    }
    
    # Calculate average duration per list
    duration_data <- data |>
      mutate(
        EventTime = eventtime / 1000,
        duration = results.reception.time - EventTime,
        duration = round(duration / 60, 1)  # minutes
      ) |>
      mutate(
        !!var_col := as.factor(.data[[var_col]])   
      )  
    
    # Optionally remove missing values
    if (input$remove_na) {
      duration_data <- na.omit(duration_data)
    }
    
    # Optionally remove values
    excl <- exclude_var_list() 
    if (!is.null(excl)) {
      duration_data <- duration_data |>
        filter(!(duration_data[[var_col]] %in% excl))
    }
    
    # Duration range
    zoom_range <- input$duration_zoom
    
    # Plot
    ggplot(duration_data, aes(y = duration, x = .data[[var_col]])) +
      geom_boxplot(fill = "#ebe5e0", 
                   outlier.color = "#201010", 
                   outlier.size = 2) +
      coord_cartesian(ylim = zoom_range) +
      labs(
        title = paste0("Average duration per ", var_col),
        y = "Time in minutes",
        x = tools::toTitleCase(var_col)
      ) +
      theme_bw() +
      theme(
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#ebe5e0", color = NA),
        panel.grid.major = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank()
      )
  })
  
  
  # Input the custom variable 
  custom_var <- reactive({
    val <- input$custom_var_name
    if (is.null(val) || trimws(val) == "") {
      return(NULL)
    } else {
      return(val)
    }
  })  
  
  # Exclude these values
  exclude_var_list <- reactive({
    val <- input$exclude_var_list
    if (is.null(val) || trimws(val) == "") {
      return(NULL)
    } else {
      strsplit(val, ",")[[1]] |>
        trimws() 
    }
  })
  
  # Calculate duration limits
  
  output$duration_zoom_ui <- renderUI({
    req(filtered_data()) 
    
    data <- filtered_data() |>
      mutate(duration = round((Results.reception.time - EventTime / 1000)/60, 1)) %>%
      filter(!is.na(duration)) 
    
    min_dur <- floor(min(data$duration, na.rm = TRUE))
    max_dur <- ceiling(max(data$duration, na.rm = TRUE))
    
    sliderInput(
      inputId = "duration_zoom",
      label = "Duration range (minutes):",
      min = min_dur,
      max = max_dur,
      value = c(min_dur, max_dur),
      step = 1,
      ticks = FALSE  
    )
  })
  
  
  
  # Participant info --------------------------------------------------------
  
  
  
  # Render participant information: count of participants in data
  output$participantSummary <- DT::renderDataTable({
    req(filtered_data())
    data <- filtered_data()
    
    participant_data <- data |>
      mutate(
        EventTime = EventTime/1000,
        duration = Results.reception.time - EventTime,
        duration = round(duration/60, 1),
        EventTimePOSIX = as.POSIXct(EventTime, origin = "1970-01-01", tz = "UTC")
      ) |>
      group_by(MD5.hash.of.participant.s.IP.address) |>
      summarise(count = n(),
                duration = max(duration),
                `data collection start` = min(EventTimePOSIX)
      )
    
    
    if (ncol(participant_data) > 0) {
      # Using the psych package's describe function for summary statistics
      summary_stats <- participant_data
      DT::datatable(summary_stats, rownames = TRUE)
    } else {
      # If no numeric columns, display a message
      DT::datatable(data.frame(Message = "No participant data found."), rownames = FALSE)
    }
  })
  
  
  # Participant count
  
  output$participantPlot <- renderPlot({
    req(filtered_data())
    data <- filtered_data()
    
    # Ensure the participant ID column exists
    if ("MD5.hash.of.participant.s.IP.address" %in% colnames(data)) {
      participant_data <- data |>
        group_by(MD5.hash.of.participant.s.IP.address) |>
        summarize(count = n()) |>
        ungroup()
      
      ggplot(participant_data, aes(y = MD5.hash.of.participant.s.IP.address, x = count)) +
        geom_bar(stat = "identity", fill="#201010") + 
        coord_flip() +
        labs(
          title = "Occurrences of each participant in the data",
          y = "Participant IP",
          x = "Row count"
        ) +
        theme_bw() +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1),
          panel.background = element_blank(),
          plot.background = element_rect(fill="#ebe5e0", color=NA),
          panel.grid.major = element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_blank()
        )
    } else {
      # Display a message if the participant ID column is missing
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Participant ID column not found in the data.", size = 5, hjust = 0.5) +
        theme_void()
    }
  })
  
  # Duration histogram
  
  output$participantDurationHistogramPlot <- renderPlot({
    req(filtered_data())
    data <- filtered_data()
    
    if ("MD5.hash.of.participant.s.IP.address" %in% colnames(data)) {
      
      # Calculate max duration per participant
      duration_data <- data |>
        mutate(
          EventTime = EventTime / 1000,
          duration = Results.reception.time - EventTime,
          duration = round(duration / 60, 1)
        ) |>
        group_by(MD5.hash.of.participant.s.IP.address) |>
        summarise(duration = max(duration, na.rm = TRUE)) |>
        ungroup()
      
      # Compute mean duration
      mean_duration <- mean(duration_data$duration, na.rm = TRUE)
      outlier_max <- mean_duration + 2*sd(duration_data$duration, na.rm = TRUE)
      outlier_min <- mean_duration - 2*sd(duration_data$duration, na.rm = TRUE)
      if (outlier_min < 0) {
        outlier_min<-0
      } 
      
      # Histogram
      ggplot(duration_data, aes(x = duration)) +
        geom_histogram(
          aes(y = after_stat(density)),
          binwidth = 4,  # 1 minute per bin, adjust as needed
          fill = "#794729",
          color = "#ebe5e0"
        ) +
        geom_density(color = scales::alpha("#201010", 0.5),
                     linewidth=1) +
        geom_vline(
          xintercept = mean_duration,
          color = "#201010",
          linewidth = 1,
          linetype = "solid"
        ) +
        geom_vline(
          xintercept = outlier_max,
          color = "#201010",
          linewidth = 1,
          linetype = "dashed",
          alpha=0.3
        ) +
        geom_vline(
          xintercept = outlier_min,
          color = "#201010",
          linewidth = 1,
          linetype = "dashed",
          alpha=0.3
        ) +
        labs(
          title = "Distribution of participant durations",
          x = "Time in minutes",
          y = "Density"
        ) +
        theme_bw() +
        theme(
          panel.background = element_blank(),
          plot.background = element_rect(fill = "#ebe5e0", color = NA),
          panel.grid.major = element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_blank()
        )
      
    } else {
      # Message if column missing
      ggplot() +
        annotate(
          "text", x = 0.5, y = 0.5, 
          label = "Participant ID column not found in the data.", 
          size = 5, hjust = 0.5
        ) +
        theme_void()
    }
  })
  
  
  ### Timeline ----------------------------------------------------------------
  
  
  # Render timeline
  output$participantTimeline <- renderTimevis({
    
    req(filtered_data())
    data <- filtered_data()
    
    participant_data <- data |>
      mutate(
        EventTime = EventTime/1000,  # Convert to seconds
        date_time = as.POSIXct(EventTime, origin = "1970-01-01")
      ) |>
      group_by(`MD5.hash.of.participant.s.IP.address`) |>
      summarise(
        first_event = min(date_time, na.rm = TRUE),
        trials = n(),
        .groups = "drop"
      ) |>
      mutate(
        id = row_number(),
        content = MD5.hash.of.participant.s.IP.address
      ) |>
      select(id, content, start = first_event)
    
    timevis(
      data = participant_data,
      options = list(
        height = "300px",
        editable = FALSE,
        showZoom = TRUE
      )
    )
  })
  
  
  ### Cards ---------------------------------------------------------------
  
  ## Participant count 
  output$participant_count <- renderText({
    req(filtered_data())
    data <- filtered_data()
    
    part_nr <- if ("MD5.hash.of.participant.s.IP.address" %in% colnames(data)) {
      data |>
        dplyr::distinct(MD5.hash.of.participant.s.IP.address) |>
        nrow()
    } else {
      NA
    }
    
    if (is.na(part_nr)) {
      "Participant ID column missing."
    } else {
      as.character(part_nr)
    }
  })
  output$median_duration <- renderText({
    req(filtered_data())
    data <- filtered_data()
    
    if ("MD5.hash.of.participant.s.IP.address" %in% colnames(data)) {
      
      duration_data <- data |>
        dplyr::mutate(
          EventTime = EventTime / 1000,
          duration = Results.reception.time - EventTime,
          duration = round(duration / 60, 1)
        ) |>
        dplyr::group_by(MD5.hash.of.participant.s.IP.address) |>
        dplyr::summarise(
          duration = max(duration, na.rm = TRUE),
          .groups = "drop"
        )
      
      median_duration <- round(median(duration_data$duration, na.rm = TRUE), 1)
      sd_duration <- round(stats::sd(duration_data$duration, na.rm = TRUE), 1)
      
    } else {
      median_duration <- NA
      sd_duration <- NA
    }
    
    if (is.na(median_duration)) {
      "Participant ID or duration column missing."
    } else {
      paste0(median_duration, " (", sd_duration, ")")
    }
  })
  
  output$average_trial <- renderText({
    req(filtered_data())
    data <- filtered_data()
    
    avg_trials <- if ("MD5.hash.of.participant.s.IP.address" %in% colnames(data)) {
      data |>
        dplyr::group_by(MD5.hash.of.participant.s.IP.address) |>
        dplyr::summarise(trials = dplyr::n(), .groups = "drop") |>
        dplyr::summarise(
          mean_trials = round(median(trials, na.rm = TRUE), 1)
        ) |>
        dplyr::pull(mean_trials)
    } else {
      NA
    }
    
    if (is.na(avg_trials)) {
      "Participant ID column missing."
    } else {
      as.character(avg_trials)
    }
  })
  
  
  
  ## Data download -----------------------------------------------------------
  
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


# RUN ---------------------------------------------------------------------
# Run the application 
shinyApp(ui = ui, server = server)
