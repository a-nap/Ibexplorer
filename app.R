# Packages
library(shiny)
library(tidyverse)
library(shinythemes)
library(psych)
library(DT)
library(bslib)

# options(shiny.maxRequestSize = 30*1024^2) 

ibextheme <- bs_theme(
  fg = "#201010", 
  bg = "#ebe5e0", 
  primary = "#794729",
  secondary = "#342e1a",
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

# tabPanel(title=tagList(icon("code"),"Script"),


# UI ----------------------------------------------------------------------

# Define UI for the application
ui <- fluidPage(
  theme = ibextheme,
  sidebarLayout(

## Sidebar -----------------------------------------------------------------
    
    sidebarPanel(
      width = 2,
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

      p(strong("Include only rows with this exact phrase:")),
      textInput(inputId = "search_phrase",
                label = "",
                value = ""),
      helpText("For example 'metadata' or 'experiment' or 'SelfPacedReadingParadigm'"),
    ),

## Main panel --------------------------------------------------------------

    mainPanel(

### Table preview -----------------------------------------------------------
# tabPanel(title=tagList(,"Stimuli file"),
         
      tabsetPanel(
        tabPanel(title=tagList(icon("table"),"Table Preview"),
                 DT::dataTableOutput("preview")
        ),

### Data summary ------------------------------------------------------------

        tabPanel(title=tagList(icon("chart-simple"),"Data summary"),
                 h3("Numerical data overview"),
                 DT::dataTableOutput("dataSummary"),
                 hr(),
                 # h3("List overview"),
                 # fluidRow(
                 #   column(width=12,
                 #          textInput(
                 #   inputId = "list_var_name",
                 #   label   = "Add your list/group variable name here:",
                 #   value   = ""
                 # ))),
                 # fluidRow(
                 #   column(width = 6,
                 #          plotOutput("listPlot")
                 #   ),
                 #   column(width = 6,
                 #          plotOutput("listDurationPlot")
                 #   )
                 # ),
                 # hr(),
                 # h3("Condition overview"),
                 # fluidRow(
                 #   column(width=12,
                 #          textInput(
                 #            inputId = "cond_var_name",
                 #            label   = "Add your condition variable name here:",
                 #            value   = ""
                 #          ))),
                 # fluidRow(
                 #   column(width = 6,
                 #          plotOutput("conditionPlot")
                 #   ),
                 #   column(width = 6,
                 #          plotOutput("conditionDurationPlot")
                 #   )
                 # ),
                 # hr(),
                 h3("Custom variable overview"),
                 fluidRow(
                   column(width = 6,
                   textInput(
                   inputId = "custom_var_name",
                   label   = "Type your variable name here:",
                   value   = ""),
                   helpText("For example 'List' or 'Condition'."))
                   ,
                   column(width = 6,
                   checkboxInput(
                     inputId = "remove_na",
                     label   = "Remove missing values (NA)?",
                     value   = FALSE  # default is not checked
                   )
                   )),
                 fluidRow(
                   column(width = 6,
                   plotOutput("varPlot")), 
                   column(width = 6,
                   plotOutput("varDurationPlot"))
                   )
        ),

### Participant overview ----------------------------------------------------

        tabPanel(title=tagList(icon("users"),"Participant overview"),
                 fluidRow(
                   column(width = 7,
                          # plotOutput("participantPlot")
                          uiOutput("participantPlotUI")
                   ),
                   column(width = 5,
                          DT::dataTableOutput("participantSummary")
                   )
                 ),
                 fluidRow(
                   column(width = 7,
                          # uiOutput("participantDurationPlotUI")
                          # plotOutput("participantDurationPlot")
                          plotOutput("participantDurationHistogramPlot")
                   ),
                   column(width = 5,
                          DT::dataTableOutput("participantDuration")
                   )
                 )
        ),

### Usage guide -------------------------------------------------------------

        tabPanel(title=tagList(icon("circle-info"),"Usage Guide"),
                 HTML(markdown::markdownToHTML(text = "
### How to use this app

- Upload a raw PCIbex CSV results file.
- Click the **Submit** button to process the file.
- (Optional) Select the columns you want to include.
- (Optional) Enter a search phrase to filter rows.
- View the processed data in the **Table Preview** tab. 
- Download the filtered dataset by clicking **Download formatted CSV**.

### Plots

**List / Group plots**

- *List occurrences* bar plot shows how many times each list or group appears in the data. Should probably be the same number for each list.
- *Average duration* boxplot shows the duration in minutes of each trial for each list/group. Helps identify lists which take longer or shorter on average.
- If the app does not detect your list variable, you can specify it in the text field.

**Condition plots**

- *Condition occurrences* bar plot showing how many trials are in each condition. Should probably be the same number for each condition.
- *Average duration* boxplot shows the trial durations for each condition. Helps identify experimental conditions which tend to be longer or shorter.
- If the app does not detect your condition variable, you can specify it in the text field.

**Participant plots**

- *Participant counts* bar plot shows the number of trials per participant. Should probably be the same number for each participant. Plot height adjusts automatically if there are many participants.
- *Participant durations* histogram shows the distribution of total participant durations in minutes. Helps identify participants who took much longer or shorter than average. Dashed vertical line: mean duration across all participants.

### Troubleshooting

Follow these steps if you're having trouble uploading and processing your data.

- Ensure that your file is in CSV format.
- Check that the file size does not exceed 30MB.
- If no data appears, verify that the correct columns are selected (e.g. 'Results.reception.time.', 'EventTime', 'MD5.hash.of.participant.s.IP.addres').
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


# SERVER ------------------------------------------------------------------

# Define server logic
server <- function(input, output, session) {
  

## Feature development -----------------------------------------------------

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
        fill = "#201010",
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
    
    ggplot(duration_data, aes(y = duration, x = .data[[var_col]])) +
      geom_boxplot(fill = "#ebe5e0", outlier.color = "#342e1a", outlier.size = 2) +
      labs(
        title = paste0("Average duration per ", var_col),
        y = "Time in minutes",
        x = tools::toTitleCase(var_col)
      ) +
      theme_bw() +
      theme(
        # axis.text.x = element_text(angle = 90, hjust = 1),
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
        mutate(across(everything()))
      DT::datatable(summary_stats, rownames = TRUE)
    } else {
      # If no numeric columns, display a message
      DT::datatable(data.frame(Message = "No numeric columns found."), rownames = FALSE)
    }
  })

## List info ---------------------------------------------------------------

  # output$listSummary <- DT::renderDataTable({
  #   req(filtered_data(), list_var())  # also require list_var reactive
  #   
  #   data <- filtered_data() |>
  #     rename_with(~ make.unique(tolower(.)))
  #   
  #   # Find list column using new logic
  #   col_names <- tolower(names(data))
  #   
  #   if (!is.null(list_var()) && trimws(list_var()) != "") {
  #     user_col <- tolower(list_var())
  #     if (user_col %in% col_names) {
  #       list_col <- names(data)[which(col_names == user_col)[1]]
  #     } else {
  #       list_col <- NULL
  #     }
  #   } else {
  #     # original fallback logic
  #     list_col <- if ("list" %in% col_names) {
  #       names(data)[which(col_names == "list")[1]]
  #     } else if ("group" %in% col_names) {
  #       names(data)[which(col_names == "group")[1]]
  #     } else {
  #       NULL
  #     }
  #   }
  #   
  #   # Stop if no valid list column found
  #   if (is.null(list_col)) {
  #     return(DT::datatable(data.frame(Message = "No list column found."), rownames = FALSE))
  #   }
  #   
  #   # Group by the found column and summarize
  #   summary_data <- data |>
  #     group_by(.data[[list_col]]) |>
  #     summarize(count = n(), .groups = 'drop')
  #   
  #   DT::datatable(summary_data, rownames = TRUE)
  # })
  # 
  
  # List plot
  output$listPlot <- renderPlot({
    req(filtered_data())
    
    data <- filtered_data() |>
      rename_with(~ make.unique(tolower(.)))
    
    # Find list column
    col_names <- tolower(names(data))
    
    if (!is.null(list_var()) && trimws(list_var()) != "") {
      user_col <- tolower(list_var())
      if (user_col %in% col_names) {
        list_col <- names(data)[which(col_names == user_col)[1]]
      } else {
        list_col <- NULL
      }
    } else {
      # fallback logic
      list_col <- if ("list" %in% col_names) {
        names(data)[which(col_names == "list")[1]]
      } else if ("group" %in% col_names) {
        names(data)[which(col_names == "group")[1]]
      } else {
        NULL
      }
    }
    
    # Stop if no valid list column found
    if (is.null(list_col)) {
      stop("No valid list column found.")
    }
    
    # Group by the found column and summarize
    data <- data |>
      group_by(.data[[list_col]]) |>
      summarize(count = n(), .groups = 'drop')
    
    # Set up for plotting
    grouping_column <- list_col
    data[[grouping_column]] <- as.factor(data[[grouping_column]])
    
    # Generate the bar plot
    ggplot(data, 
           aes(x = .data[[grouping_column]], y = count)) +
      geom_bar(stat = "identity", fill = "#342e1a") +
      geom_text(aes(label = count), vjust = -0.3, size = 4, color = "#342e1a") +
      labs(
        x = tools::toTitleCase(grouping_column),
        y = "Row count",
        title = "Occurrences of each list in the data"
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
  
  
  # List duration plot
  output$listDurationPlot <- renderPlot({
    req(filtered_data())
    
    data <- filtered_data() |>
      rename_with(~ make.unique(tolower(.)))
    
    # Find list column
    col_names <- names(data)
    
    if (!is.null(list_var()) && trimws(list_var()) != "") {
      user_col <- tolower(list_var())
      if (user_col %in% col_names) {
        list_col <- names(data)[which(col_names == user_col)[1]]
      } else {
        list_col <- NULL
      }
    } else {
      # fallback logic
      list_col <- if ("list" %in% col_names) {
        names(data)[which(col_names == "list")[1]]
      } else if ("group" %in% col_names) {
        names(data)[which(col_names == "group")[1]]
      } else {
        NULL
      }
    }
    
    # Stop if no valid list column found
    if (is.null(list_col)) {
      stop("No valid list column found.")
    }
    
    # Calculate average duration per list
    duration_data <- data |>
      mutate(
        EventTime = eventtime / 1000,
        duration = results.reception.time - EventTime,
        duration = round(duration / 60, 1)  # minutes
      ) |>
      mutate(
        !!list_col := as.factor(.data[[list_col]])   
      )  

    ggplot(duration_data, aes(y = duration, x = .data[[list_col]])) +
      geom_boxplot(fill = "#ebe5e0", outlier.color = "#342e1a", outlier.size = 2) +
      labs(
        title = "Average duration per list",
        y = "Time in minutes",
        x = tools::toTitleCase(list_col)
      ) +
      theme_bw() +
      theme(
        # axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#ebe5e0", color = NA),
        panel.grid.major = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank()
      )
  })

  
# Optionally, input the list variable 

list_var <- reactive({
  val <- input$list_var_name
  if (is.null(val) || trimws(val) == "") {
    return(NULL)
  } else {
    return(val)
  }
})


## Condition info ---------------------------------------------------------------


output$conditionSummary <- DT::renderDataTable({
  req(filtered_data())
  
  data <- filtered_data() |>
    rename_with(~ make.unique(tolower(.)))
  
  # Find condition column 
  col_names <- tolower(names(data))
  
  if (!is.null(cond_var()) && trimws(cond_var()) != "") {
    user_col <- tolower(cond_var())
    if (user_col %in% col_names) {
      cond_col <- names(data)[which(col_names == user_col)[1]]
    } else {
      cond_col <- NULL
    }
  } else {
    # fallback logic
    cond_col <- if ("condition" %in% col_names) {
      names(data)[which(col_names == "condition")[1]]
    } else if ("treatment" %in% col_names) {
      names(data)[which(col_names == "treatment")[1]]
    } else {
      NULL
    }
  }
  
  # Stop if no valid condition column found
  if (is.null(cond_col)) {
    return(DT::datatable(data.frame(Message = "No condition column found."), rownames = FALSE))
  }
  
  # Group by the found column and summarize
  summary_data <- data |>
    group_by(.data[[cond_col]]) |>
    summarize(count = n(), .groups = 'drop')
  
  DT::datatable(summary_data, rownames = TRUE)
})


# Conditions plot
output$conditionPlot <- renderPlot({
  req(filtered_data()) 
  
  data <- filtered_data() %>%
    rename_with(~ make.unique(tolower(.)))
  
  # Find condition column using new logic
  col_names <- tolower(names(data))
  
  if (!is.null(cond_var()) && trimws(cond_var()) != "") {
    user_col <- tolower(cond_var())
    if (user_col %in% col_names) {
      cond_col <- names(data)[which(col_names == user_col)[1]]
    } else {
      cond_col <- NULL
    }
  } else {
    # original fallback logic
    cond_col <- if ("condition" %in% col_names) {
      names(data)[which(col_names == "condition")[1]]
    } else if ("treatment" %in% col_names) {
      names(data)[which(col_names == "treatment")[1]]
    } else {
      NULL
    }
  }
  
  # Stop if no valid list column found
  if (is.null(cond_col)) {
    stop("No valid condition column found.")
  }
  
  # Group by the found column and summarize
  data <- data %>%
    group_by(.data[[cond_col]]) |>
    summarize(count = n(), .groups = 'drop')
  
  # Set up for plotting
  grouping_column <- cond_col
  data[[grouping_column]] <- as.factor(data[[grouping_column]])
  
  # Generate the bar plot
  ggplot(data, 
         aes(x = .data[[grouping_column]], y = count)) +
    geom_bar(stat = "identity", fill = "#342e1a") +
    geom_text(aes(label = count), vjust = -0.3, size = 4, color = "#342e1a") +
    labs(
      x = tools::toTitleCase(grouping_column),
      y = "Row count",
      title = "Occurrences of each list in the data"
    ) +
    theme_bw() +
    theme(
      panel.background = element_blank(),
      plot.background = element_rect(fill = "#ebe5e0", color = NA),
      panel.grid.major = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
    )
})


# Condition duration plot
output$conditionDurationPlot <- renderPlot({
  req(filtered_data())
  
  data <- filtered_data() |>
    rename_with(~ make.unique(tolower(.)))
  
  # Find list column
  col_names <- names(data)
  
  
  if (!is.null(cond_var()) && trimws(cond_var()) != "") {
    # try to match user-provided name case-insensitively
    user_col <- tolower(cond_var())
    if (user_col %in% col_names) {
      cond_col <- names(data)[which(col_names == user_col)[1]]
    } else {
      cond_col <- NULL  
    }
  } else {
    # fallback to logic
    cond_col <- if ("condition" %in% col_names) {
      names(data)[which(col_names == "condition")[1]]
    } else if ("treatment" %in% col_names) {
      names(data)[which(col_names == "treatment")[1]]
    } else {
      NULL
    }
  }
  
  # Stop if no valid condition column found
  if (is.null(cond_var)) {
    stop("No valid condition column found.")
  }
  
  # Calculate average duration per condition
  duration_data <- data |>
    mutate(
      EventTime = eventtime / 1000,
      duration = results.reception.time - EventTime,
      duration = round(duration / 60, 1)  # minutes
    ) |>
    mutate(
      !!cond_col := as.factor(.data[[cond_col]])   
    )  

  ggplot(duration_data, aes(y = duration, x = .data[[cond_col]])) +
    geom_boxplot(fill = "#ebe5e0", outlier.color = "#342e1a", outlier.size = 2) +
    # geom_col(fill = "#7c6f42") + 794729
    labs(
      title = "Average duration per condition",
      y = "Time in minutes",
      x = tools::toTitleCase(cond_col)
    ) +
    theme_bw() +
    theme(
      # axis.text.x = element_text(angle = 90, hjust = 1),
      panel.background = element_blank(),
      plot.background = element_rect(fill = "#ebe5e0", color = NA),
      panel.grid.major = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank()
    )
})


# Optionally, input the list variable nae 

cond_var <- reactive({
  val <- input$cond_var_name
  if (is.null(val) || trimws(val) == "") {
    return(NULL)
  } else {
    return(val)
  }
})

  
# Participant info --------------------------------------------------------



  # Render participant information: count of participants in data
  output$participantSummary <- DT::renderDataTable({
    req(filtered_data())
    data <- filtered_data()
    participant_data <- data |> group_by(MD5.hash.of.participant.s.IP.address) |> summarize(count = n())
    if (ncol(participant_data) > 0) {
      # Using the psych package's describe function for summary statistics
      summary_stats <- participant_data
      DT::datatable(summary_stats, rownames = TRUE)
    } else {
      # If no numeric columns, display a message
      DT::datatable(data.frame(Message = "No numeric columns found."), rownames = FALSE)
    }
  })
  

# Participant count
output$participantPlotUI <- renderUI({
  n <- nrow(filtered_data())  # number of participants
  max_height <- 700          # cap the height at 1000px
  height <- min(50 + n * 5, max_height)  # 5px per participant + base
  
  plotOutput("participantPlot", height = paste0(height, "px"))
})

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
        geom_bar(stat = "identity", fill="#342e1a") +
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
  
  # Duration table
  output$participantDuration <- DT::renderDataTable({
    req(filtered_data())
    data <- filtered_data()
    
    duration_data <- data |>
      mutate(
        EventTime = EventTime/1000,
        duration = Results.reception.time - EventTime,
        duration = round(duration/60, 1)
      ) |>
      group_by(MD5.hash.of.participant.s.IP.address) |>
      summarise(duration = max(duration)) |>
      ungroup()
    
    # participant_data <- data |> group_by(MD5.hash.of.participant.s.IP.address) |> summarize(count = n())
    if (ncol(duration_data) > 0) {
      # Using the psych package's describe function for summary statistics
      summary_stats <- duration_data
      DT::datatable(summary_stats, rownames = TRUE)
    } else {
      # If no numeric columns, display a message
      DT::datatable(data.frame(Message = "No duration data found."), rownames = FALSE)
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
          duration = round(duration / 60, 1)  # minutes
        ) |>
        group_by(MD5.hash.of.participant.s.IP.address) |>
        summarise(duration = max(duration, na.rm = TRUE)) |>
        ungroup()
      
      # Compute mean duration
      mean_duration <- mean(duration_data$duration, na.rm = TRUE)
      
      # Histogram
      ggplot(duration_data, aes(x = duration)) +
        geom_histogram(
          binwidth = 5,  # 1 minute per bin, adjust as needed
          fill = "#342e1a",
          color = "#ebe5e0"
        ) +
        geom_vline(
          xintercept = mean_duration,
          color = "#7c6f42",
          linewidth = 1,
          linetype = "dashed"
        ) +
        labs(
          title = "Distribution of participant durations",
          x = "Time in minutes",
          y = "Number of participants"
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
  
  
  # Experiment duration per participant

  output$participantDurationPlotUI <- renderUI({
    n <- nrow(filtered_data())  # number of participants
    max_height <- 1000          # cap the height at 1000px
    height <- min(50 + n * 5, max_height)  # 5px per participant + base
    
    plotOutput("participantDurationPlot", height = paste0(height, "px"))
  })
  
  output$participantDurationPlot <- renderPlot({
    req(filtered_data())
    data <- filtered_data()
    
    
    if ("MD5.hash.of.participant.s.IP.address" %in% colnames(data)) {
      duration_data <- data |>
        mutate(
        EventTime = EventTime/1000,
               duration = Results.reception.time - EventTime,
               duration = round(duration/60, 1)
          ) |>
        group_by(MD5.hash.of.participant.s.IP.address) |>
        summarise(duration = max(duration)) |>
        ungroup()
      
      mean_data <- duration_data |>
        summarise(mean_duration = mean(duration, na.rm = TRUE))
      
      ggplot(duration_data) +
        geom_vline(
          data = mean_data,
          aes(xintercept = mean_duration),
          color = "#342e1a",
          linewidth = 1,
          inherit.aes = FALSE
        ) +
        geom_bar(aes(y = MD5.hash.of.participant.s.IP.address, x = duration),
                 stat = "identity", fill="#7c6f42") +
        labs(
          title = "Duration of the experiment",
          y = "Participant IP",
          x = "Time in minutes"
        ) +
        theme_bw() +
        theme(
          # axis.text.x = element_text(angle = 90, hjust = 1),
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


# TODO --------------------------------------------------------------------

