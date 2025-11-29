# Load required packages ---------------------------------------------------
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)

# Read and prepare data ----------------------------------------------------
cities   <- read_csv("cities.csv") %>% na.omit()
outlook  <- read_csv("outlook_meanings.csv") %>% na.omit()
weather  <- read_csv("weather_forecasts.csv") %>% na.omit()

weather <- weather %>%
  mutate(date = as.Date(date)) %>%
  left_join(outlook, by = "forecast_outlook")   # adds meaning column

outlook_types <- sort(unique(weather$forecast_outlook))

# UI: sidebar on left, graphs on right ------------------------------------
ui <- fluidPage(
  titlePanel("Interactive Weather Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filters"),
      selectInput(
        "selected_outlook", "Forecast Outlook:",
        choices = c("All", outlook_types), selected = "All"
      ),
      dateRangeInput(
        "date_range", "Date Range:",
        start = min(weather$date, na.rm = TRUE),
        end   = max(weather$date,  na.rm = TRUE)
      ),
      hr(),
      textOutput("selected_filters")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Daily Temp Trend",
          br(),
          plotlyOutput("temp_trend", height = "400px")
        ),
        tabPanel(
          "Top Cities (Temp)",
          br(),
          plotlyOutput("city_plot", height = "400px")
        )
      )
    )
  )
)

# SERVER -------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Text summary of filters
  output$selected_filters <- renderText({
    req(input$date_range)
    paste(
      "Forecast Outlook:", input$selected_outlook,
      "\nDate Range:",
      format(input$date_range[1], "%B %d, %Y"), "to",
      format(input$date_range[2], "%B %d, %Y")
    )
  })
  
  # Common filtered data
  filtered_weather <- reactive({
    req(input$date_range)
    weather %>%
      filter(
        date >= input$date_range[1],
        date <= input$date_range[2],
        if (input$selected_outlook != "All")
          forecast_outlook == input$selected_outlook
        else TRUE
      )
  })
  
  # Plot 1: Daily temperature trend ---------------------------------------
  output$temp_trend <- renderPlotly({
    df <- filtered_weather() %>%
      group_by(date) %>%
      summarise(
        avg_observed_temp = mean(observed_temp, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(df, aes(x = date, y = avg_observed_temp)) +
      geom_line(color = "black") +
      geom_point(color = "blue", size = 2) +
      theme_minimal(base_size = 14) +
      labs(
        title = "Daily Average Observed Temperature",
        x = "Date",
        y = "Avg Observed Temp"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Plot 2: Top cities by average temp ------------------------------------
  output$city_plot <- renderPlotly({
    df <- filtered_weather() %>%
      group_by(city, state) %>%
      summarise(
        avg_observed_temp = mean(observed_temp, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(avg_observed_temp)) %>%
      slice_head(n = 10) %>%
      mutate(city_label = factor(paste(city, state, sep = ", "),
                                 levels = paste(city, state, sep = ", ")))
    
    plot_ly(
      data = df,
      x    = ~city_label,
      y    = ~avg_observed_temp,
      type = "bar",
      marker = list(color = "blue")
    ) %>%
      layout(
        title = "Top 10 Cities by Avg Observed Temperature",
        xaxis = list(title = "City", tickangle = -45),
        yaxis = list(title = "Avg Observed Temp"),
        margin = list(b = 150)
      )
  })
}

# Run app ------------------------------------------------------------------
shinyApp(ui, server)
