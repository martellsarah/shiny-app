library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# Load real estate dataset
project1_data <- read.csv("/srv/shiny-server/midtermdata.csv")

# Define the UI
ui <- fluidPage(  
  theme = bs_theme(
    version = 4,
    bootswatch = "flatly",
    primary = "#ff66b2",
    secondary = "#d9a1d1",
    success = "#f4c2c2",
    danger = "#ff4d94"
  ),
  
  # Custom CSS for consistent styling with the pink theme
  tags$head(
    tags$style(HTML("
    body { font-family: 'Roboto', sans-serif; background-color: #f9f9f9; }
    .btn, .form-control {
      border-radius: 4px;
      background-color: #ff66b2;
      color: white;
      border: 1px solid #ff66b2;
    }
    .btn:hover, .form-control:focus {
      background-color: #ff4d94;
      border-color: #ff4d94;
    }
    .metric-box {
      padding: 20px;
      border-radius: 12px;
      box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      margin: 10px;
      background: #ffffff;
      border: 1px solid #f4c2c2;
      position: relative;
    }
    .metric-title {
      color: black;
      font-size: 14px;
      margin-bottom: 8px;
    }
    .value {
      font-size: 28px;
      font-weight: bold;
      color: black;
    }
  "))
  ),
  
  # Title of the app
  titlePanel("Project 1 Deployment - LotwiZe Case Study"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      selectInput("bedrooms_select", "Select Number of Bedrooms:",
                  choices = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "14", "32"),
                  selected = "1"),
      helpText("Choose a bedroom amount to analyze."),
      sliderInput("lotSize_range", "Select Lot Size (in square feet):",
                  min = 87, max = 363421080, value = c(87, 363421080), step = 500),
      helpText("Adjust the lot size range for property analysis."),
      downloadButton("download_data", "Download Home Data")
    ),
    mainPanel(
      h4("Home Summary"),
      fluidRow(
        column(4, div(class = "metric-box", h4(class = "metric-title", "Price"), uiOutput("price"))),
        column(4, div(class = "metric-box", h4(class = "metric-title", "Matching Homes"), uiOutput("matchingHomes"))),
        column(4, div(class = "metric-box", h4(class = "metric-title", "State"), uiOutput("state")))
      ),
      h4("Distribution of Homes Based on Selected Criteria"),
      plotOutput("histogram_plot")
    )
  )
)

# Define the server
server <- function(input, output, session) {
  
  # Reactive expression to filter data based on selected bedrooms and lot size
  filtered_home_data <- reactive({
    project1_data %>%
      filter(bedrooms %in% as.numeric(input$bedrooms_select) &
               lotSize >= input$lotSize_range[1] & lotSize <= input$lotSize_range[2])
  })
  
  # Render price output
  output$price <- renderUI({
    data <- filtered_home_data()
    avg_price <- mean(data$price, na.rm = TRUE)
    HTML(paste("<span class='value'>$", formatC(avg_price, format = "f", big.mark = ",", digits = 0), "</span>"))
  })
  
  # Render matching homes count
  output$matchingHomes <- renderUI({
    num_homes <- nrow(filtered_home_data())
    HTML(paste("<span class='value'>", num_homes, "</span>"))
  })
  
  # Render state output
  output$state <- renderUI({
    states <- paste(unique(filtered_home_data()$state), collapse = ", ")
    HTML(paste("<span class='value'>", states, "</span>"))
  })
  
  # Render histogram
  output$histogram_plot <- renderPlot({
    data <- filtered_home_data()
    ggplot(data, aes(x = price)) +
      geom_histogram(binwidth = 50000, fill = "#ff66b2", color = "#e75480", alpha = 0.7) +
      labs(title = "Distribution of Home Prices", x = "Price ($)", y = "Number of Homes") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
