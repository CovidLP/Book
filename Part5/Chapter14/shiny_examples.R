###---------------------------------------------------------------###
###   Building a Platform for Data-Driven Pandemic Prediction:    ###
###   From Data Modelling to Visualization - the CovidLP Project  ###
###                                                               ###
###   Chapter 14: Building an interactive app withShiny           ###
###   Sections 14.3 and 14.5                                      ###
###                                                               ###
###   Author: the CovidLP Team                                    ###
###---------------------------------------------------------------###

# Packages
library(PandemicLP)
library(shiny)

# Data
data <- PandemicLP::load_covid(country_name = 'argentina', last_date = '2020-11-15')$data

# data <- read.table(file = "data/argentine.csv", sep = ";", header = TRUE) # Run this to load the data from a file

# Simple app 1
data$date <- as.Date(data$date)

server <- function(input, output) {
  output$time_series <- renderPlot({
    series <- input$series
    plot(
      x = data[["date"]], y = data[[series]], 
      xlab = "Date", ylab = series
    )
  })
}

ui <- fluidPage(
  selectInput(
    inputId = "series", 
    label = "Select a variable", 
    choices = c(
      "cases", "deaths", 
      "new_cases", "new_deaths"
    ),
    selected = "cases"
  ),
  plotOutput(outputId = "time_series")
)

shinyApp(ui = ui, server = server)

# Simple app 1 - reactive
server <- function(input, output) {
  reactive_data <- reactive({
    series <- input$series
    ts_plot <- data[, c("date", input$series)]
    
    return(ts_plot)
  })
  
  output$time_series <- renderPlot({
    series <- input$series
    start_date <- input$start_date
    
    data <- reactive_data()
    data_sub <- subset(data, date >= start_date)
    
    plot(
      x = data_sub[["date"]], y = data_sub[[series]], 
      xlab = "Date", ylab = series
    )
  })
}

ui <- fluidPage(
  selectInput(
    inputId = "series", 
    label = "Select a variable", 
    choices = c("cases", "deaths", "new_cases",
                "new_deaths"), 
    selected = "cases"
  ),
  dateInput(
    inputId = "start_date", 
    label = "Start date", 
    value = "2020-01-23",
    min = "2020-01-23",
    max = "2020-11-15"
  ),
  plotOutput(outputId = "time_series")
)

shinyApp(ui = ui, server = server)

# Simple app 2
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      width = 2,
      selectInput(
        inputId = "series", 
        label = "Select a variable", 
        choices = c("cases", "deaths", "new_cases", "new_deaths"), 
        selected = "cases"
      ),
      dateInput(
        inputId = "start_date", 
        label = "Start date", 
        value = "2020-01-23",
        min = "2020-01-23",
        max = "2020-11-15"
      )
    ), 
    mainPanel = mainPanel(
      plotOutput(outputId = "time_series")
    )
  )
)

shinyApp(ui = ui, server = server)

# Simple app 3
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      width = 2,
      fluidRow(
        column(
          width = 6,
          selectInput(
            inputId = "series", 
            label = "Select a variable", 
            choices = c("cases", "deaths", "new_cases", "new_deaths"), 
            selected = "cases"
          )
        ),
        column(
          width = 6,
          dateInput(
            inputId = "start_date", 
            label = "Start date", 
            value = "2020-01-23",
            min = "2020-01-23",
            max = "2020-11-15"
          )
        )
      )
    ), 
    mainPanel = mainPanel(
      column(width = 6,
        plotOutput(outputId = "time_series") 
      )
    )
  )
)

shinyApp(ui = ui, server = server)

# Simple app 4
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      width = 2,
      style = 'background-color: grey',
      fluidRow(
        column(
          width = 6,
          selectInput(
            inputId = "series", 
            label = "Select a variable", 
            choices = c("cases", "deaths", "new_cases", "new_deaths"), 
            selected = "cases"
          )
        ),
        column(
          width = 6,
          dateInput(
            inputId = "start_date", 
            label = "Start date", 
            value = "2020-01-23",
            min = "2020-01-23",
            max = "2020-11-15"
          )
        )
      )
    ), 
    mainPanel = mainPanel(
      column(
        width = 6,
        plotOutput(outputId = "time_series")
      )
    )
  )
)

shinyApp(ui = ui, server = server)

# Simple app 4
ui <- fluidPage(
  includeCSS("www/styles.css"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      width = 2,
      fluidRow(
        column(
          width = 6,
          selectInput(
            inputId = "series", 
            label = "Select a variable", 
            choices = c(
              "cases", "deaths", 
              "new_cases", "new_deaths"
            ), 
            selected = "cases"
          )
        ),
        column(
          width = 6,
          dateInput(
            inputId = "start_date", 
            label = "Start date", 
            value = "2020-01-23",
            min = "2020-01-23",
            max = "2020-11-15"
          )
        )
      )
    ), 
    mainPanel = mainPanel(
      column(width = 6,
      plotOutput(outputId = "time_series")
      )
    )
  )
)

shinyApp(ui = ui, server = server)
