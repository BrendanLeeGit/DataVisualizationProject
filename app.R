#Group 4
#Brendan, Ian, Steven
#2025-11-17

# Libraries ---------------------------------------------------------------
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(tidytuesdayR)
library(shinyWidgets)

# Get the data ------------------------------------------------------------

# Option A: via tidytuesdayR
tuesdata <- tidytuesdayR::tt_load(2022, week = 51)

weather_forecasts <- tuesdata$weather_forecasts
cities            <- tuesdata$cities
outlook_meanings  <- tuesdata$outlook_meanings

# Basic preparation -------------------------------------------------------

weather <- weather_forecasts %>%
  mutate(date = as.Date(date)) %>%
  left_join(outlook_meanings, by = "forecast_outlook")

outlook_types <- sort(unique(weather$forecast_outlook))

#Creating forecast error column by subtracting observed-forecast temperature.
weather_forecasts$forecast_error <- weather_forecasts$observed_temp - weather_forecasts$forecast_temp

#Cleaning up weather forecast dataset for heatmap
weather_forecast_clean <- weather_forecasts %>%
  filter(!is.na(forecast_error),
         !is.na(forecast_hours_before),
         !is.na(state))

weather_forecast_clean <- weather_forecast_clean %>%
  mutate(state = factor(state,
                        levels = unique(state)))  # preserves dataset order

weather_forecast_clean <- weather_forecast_clean %>%
  mutate(forecast_hours_before = factor(forecast_hours_before,
                                        levels = sort(unique(forecast_hours_before))))

weather_forecast_clean <- weather_forecast_clean %>%
  mutate(state = factor(state, levels = unique(state)))

weather_forecast_clean <- weather_forecast_clean %>%
  mutate(
    state = tolower(state.name[match(state, state.abb)])  # convert abbreviations to full lowercase names
  )

#Creating dataset to show the average forecast error by time observed by state
weather_forecast_avg <- weather_forecast_clean %>%
  group_by(state, forecast_hours_before) %>%
  summarise(
    avg_forecast_error = mean(forecast_error, na.rm = TRUE)
  ) %>%
  ungroup()

#Creating data set showing the minimum and maximum forecast error by state
weather_forecast_minmax <- weather_forecast_clean %>%
  group_by(state) %>%
  summarise(
    min_error = min(forecast_error, na.rm = TRUE),
    max_error = max(forecast_error, na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(cols = c(min_error, max_error),
                      names_to = "error_type",
                      values_to = "forecast_error")


weather_forecast_minmax <- weather_forecast_minmax %>%
  mutate(state = fct_reorder(state, forecast_error, .fun = max))

# UI: sidebar on left, plots on right -------------------------------------

ui <- fluidPage(
  titlePanel("Interactive Weather Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton(inputId = "reset", label = "Reset", icon = icon("sync")),
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
      
      pickerInput(
        inputId = "State",
        label = h4("State"),
        choices = sort(unique(c(weather_forecast_clean$state, weather_forecast_avg$state))),
        selected = unique(weather_forecast_clean$state),
        multiple = TRUE,
        options  = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 10"
        )),
      
      
      sliderInput("Forecast_Error","Forecast Error",
                  min = min(weather_forecast_clean$forecast_error, na.rm = TRUE),
                  max = max(weather_forecast_clean$forecast_error, na.rm = TRUE),
                  value = c(-30, 107)),
      hr(),
      textOutput("selected_filters")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Daily Temp Trend",  br(), plotlyOutput("temp_trend", height = "400px")),
        tabPanel("Top Cities (Temp)", br(), plotlyOutput("city_plot", height = "400px")),
        tabPanel("Average Forecast Error", br(), plotOutput("Forecast_avg", height = "700px")),
        tabPanel("Min/Max Error by State", br(), plotOutput("min_max", height = "400px")),
        tabPanel("Correlation Test Results", br(), plotOutput("ct_results", height = "400px"))
      )
    )
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  output$selected_filters <- renderText({
    req(input$date_range)
    paste(
      "Forecast Outlook:", input$selected_outlook,
      "\nDate Range:",
      format(input$date_range[1], "%B %d, %Y"), "to",
      format(input$date_range[2], "%B %d, %Y")
    )
  })
  
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
  
  # Plot 1: Daily average temperature
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
  
  # Plot 2: Top 10 cities by avg temp
  output$city_plot <- renderPlotly({
    df <- filtered_weather() %>%
      group_by(city, state) %>%
      summarise(
        avg_observed_temp = mean(observed_temp, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(avg_observed_temp)) %>%
      slice_head(n = 10) %>%
      mutate(
        city_label = factor(
          paste(city, state, sep = ", "),
          levels = paste(city, state, sep = ", ")
        )
      )
    
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

  #Heatmap
  filtered_forecast_avg <- reactive({
    req(input$State)
    
    weather_forecast_avg %>%
      filter(
        state %in% input$State,
      )
  })
  
  output$Forecast_avg <- renderPlot({
    ggplot(filtered_forecast_avg(),
           aes(x = forecast_hours_before, y = state, fill = avg_forecast_error)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(
        low = "blue", mid = "gray90", high = "red",
        midpoint = 0, name = "Avg Forecast Error"
      ) +
      labs(
        title = "Average Forecast Error by Lead Time and State",
        x = "Forecast Hours Before Observation",
        y = "State"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 45, hjust = 1)
      )
  })

#Scatterplot
  filtered_min_max <- reactive({
    req(input$State, input$Forecast_Error)
    
    weather_forecast_minmax %>%
      filter(
        state %in% input$State,
        forecast_error >= input$Forecast_Error[1],
        forecast_error <= input$Forecast_Error[2]
      )
  })
  
  
  output$min_max <- renderPlot(
    
    ggplot(filtered_min_max(),
           aes(x = state, y = forecast_error, color = error_type, shape = error_type)) +
      geom_point(size = 3) +
      geom_line(aes(group = state), color = "gray50", linetype = "dashed") +
      labs(
        title = "Minimum and Maximum Forecast Error by State",
        x = "State",
        y = "Forecast Error",
        color = "Error Type",
        shape = "Error Type"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  )

  observeEvent(eventExpr = input$reset, handlerExpr = {
    
    updatePickerInput(session = session, 
                      inputId = "State", 
                      selected = unique(unique(weather_forecast_clean$state)))
    
    updateSliderInput(session,
                      inputId = "Forecast_Error",
                      value = c(-30, 107)
    )
    
    updateSelectInput(session,
                      "selected_outlook",
                      selected = "All"   
    )
    updateDateRangeInput(
      session,
      "date_range",
      start = min(weather$date, na.rm = TRUE),
      end   = max(weather$date, na.rm = TRUE)
    )
  
  
  })
  
  # --------------Correlation graph -----------------
  # Set up data
  # Create new column showing discrepancy between forecast and observed temps
  weather_accuracy <- weather_forecasts %>% mutate(temp_dif = abs(observed_temp - forecast_temp))
  
  # Summarize the average per city
  discrepancy_per_city <- weather_accuracy %>% group_by(city) %>%
    summarize(avg_discrepancy = mean(temp_dif, na.rm = TRUE)) %>% arrange(desc(avg_discrepancy))
  
  # Join tables to get citys' distance from coast
  cities_forecasts_join <- discrepancy_per_city %>% left_join(cities, by = "city")
  
  # Remove NA values
  cities_forecasts_join <- na.omit(cities_forecasts_join)
  
  # The outlier here is Juneau in Alaska
  # It has a much higher annual precipitation, so we remove it
  cities_forecasts_join <- cities_forecasts_join %>% filter(city != "JUNEAU")
  
  # Perform correlation tests
  correlation_test_results <- tibble(
    test = c("avg_precip", "elevation", "wind", "distance_to_coast"),
    correlation = c(
      cor.test(cities_forecasts_join$avg_annual_precip, cities_forecasts_join$avg_discrepancy)$estimate,
      cor.test(cities_forecasts_join$elevation, cities_forecasts_join$avg_discrepancy)$estimate,
      cor.test(cities_forecasts_join$wind, cities_forecasts_join$avg_discrepancy)$estimate,
      cor.test(cities_forecasts_join$distance_to_coast, cities_forecasts_join$avg_discrepancy)$estimate
    ),
    p_value = c(
      cor.test(cities_forecasts_join$avg_annual_precip, cities_forecasts_join$avg_discrepancy)$p.value,
      cor.test(cities_forecasts_join$elevation, cities_forecasts_join$avg_discrepancy)$p.value,
      cor.test(cities_forecasts_join$wind, cities_forecasts_join$avg_discrepancy)$p.value,
      cor.test(cities_forecasts_join$distance_to_coast, cities_forecasts_join$avg_discrepancy)$p.value
    )
  )
  
  # Rename columns for the graph
  correlation_test_results <- correlation_test_results %>%
    mutate(test = recode(test,
                         "avg_precip" = "Avg Annual Precip",
                         "wind"   = "Wind Speed",
                         "elevation" = "Elevation",
                         "distance_to_coast" = "Distance to Coast"))
  
  # Create reactive
  c_t_results <- reactive({
    correlation_test_results
  })
  
  # Render plot
  output$ct_results <- renderPlot(
    ggplot(c_t_results(), 
           aes(x = reorder(test, correlation),
               y = correlation, fill = correlation > 0)) +
      geom_col() +
      labs(
        title = "Correlation Test Results",
        x = "Factor",
        y = "Correlation with Temperature Discrepancy"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none" # Key is unnecessary
      )
    )
}

shinyApp(ui, server)
