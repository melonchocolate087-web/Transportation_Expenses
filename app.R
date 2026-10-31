
# ============================================
# TRANSPORTATION EXPENSES DASHBOARD
# ============================================

library(shiny)
library(ggplot2)

# 1. Load the dataset
transport <- read.csv(
  "transportation_expenses.csv",
  stringsAsFactors = FALSE
)

# 2. Clean column names
names(transport) <- tolower(names(transport))

names(transport) <- gsub(
  "[^a-z0-9]+",
  "_",
  names(transport)
)

names(transport) <- gsub(
  "^_|_$",
  "",
  names(transport)
)

# 3. Identify the correct columns
# Based on your actual dataset

method_col <- "main_transport"

purpose_col <- "travel_purpose"

fare_col <- "daily_fare_php"

# Check if the required columns exist
required_columns <- c(
  method_col,
  purpose_col,
  fare_col
)

missing_columns <- setdiff(
  required_columns,
  names(transport)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing columns:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

# 4. Prepare the data

transport[[method_col]] <- as.character(
  transport[[method_col]]
)

transport[[purpose_col]] <- as.character(
  transport[[purpose_col]]
)

transport[[fare_col]] <- as.numeric(
  gsub(
    "[^0-9.-]",
    "",
    as.character(transport[[fare_col]])
  )
)

# Remove rows with missing transportation or purpose
transport <- transport[
  !is.na(transport[[method_col]]) &
    !is.na(transport[[purpose_col]]),
]

# Remove blank categories
transport <- transport[
  trimws(transport[[method_col]]) != "" &
    trimws(transport[[purpose_col]]) != "",
]

# 5. Create the dashboard interface

ui <- fluidPage(
  
  titlePanel("Transportation Expenses Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "method",
        "Transportation Method",
        choices = c(
          "All",
          sort(unique(transport[[method_col]]))
        ),
        selected = "All"
      ),
      
      selectInput(
        "purpose",
        "Travel Purpose",
        choices = c(
          "All",
          sort(unique(transport[[purpose_col]]))
        ),
        selected = "All"
      )
    ),
    
    mainPanel(
      
      h3("Transportation Summary"),
      
      textOutput("total_respondents"),
      
      textOutput("average_fare"),
      
      plotOutput("fare_plot"),
      
      h3("Respondent Records"),
      
      tableOutput("data_table")
    )
  )
)

# 6. Create the dashboard server

server <- function(input, output, session) {
  
  # Apply selected filters
  filtered_data <- reactive({
    
    data <- transport
    
    if (input$method != "All") {
      data <- data[
        data[[method_col]] == input$method,
        ,
        drop = FALSE
      ]
    }
    
    if (input$purpose != "All") {
      data <- data[
        data[[purpose_col]] == input$purpose,
        ,
        drop = FALSE
      ]
    }
    
    data
  })
  
  # Total respondents
  output$total_respondents <- renderText({
    
    paste(
      "Number of Respondents:",
      nrow(filtered_data())
    )
    
  })
  
  # Average daily fare
  output$average_fare <- renderText({
    
    data <- filtered_data()
    
    valid_fares <- data[[fare_col]]
    
    valid_fares <- valid_fares[
      is.finite(valid_fares)
    ]
    
    if (length(valid_fares) == 0) {
      return("Average Daily Fare: No available data")
    }
    
    paste(
      "Average Daily Fare: PHP",
      format(
        round(mean(valid_fares), 2),
        nsmall = 2
      )
    )
    
  })
  
  # Interactive fare graph
  output$fare_plot <- renderPlot({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No records match the selected filters."
      )
    )
    
    validate(
      need(
        any(is.finite(data[[fare_col]])),
        "No valid fare values are available."
      )
    )
    
    ggplot(
      data,
      aes(
        x = .data[[method_col]],
        y = .data[[fare_col]]
      )
    ) +
      
      geom_boxplot(
        fill = "#2563eb",
        na.rm = TRUE
      ) +
      
      labs(
        title = "Daily Fare by Transportation Method",
        x = "Transportation Method",
        y = "Daily Fare (PHP)"
      ) +
      
      theme_minimal()
    
  })
  
  # Interactive respondent table
  output$data_table <- renderTable({
    
    head(filtered_data(), 10)
    
  })
  
}

# 7. Launch the Shiny application

shinyApp(ui = ui, server = server)
