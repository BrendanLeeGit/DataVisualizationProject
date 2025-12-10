library(flexdashboard)
library(tidyverse)
library(lubridate)
library(plotly)
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)

# Brendan
# This is conceptual for exploring what factors cause forecast inaccuracies

# Get data (TidyTuesday)
cities <- read_csv("cities.csv")
weather_forecasts <- read_csv("weather_forecasts.csv")

head(cities)
weather_forecasts
cities

# Find what cities have the least accurate forecasts

# Create new column showing discrepancy between forecast and observed temps
weather_accuracy <- weather_forecasts %>% mutate(temp_dif = abs(observed_temp - forecast_temp))

# Summarize the average per city
discrepancy_per_city <- weather_accuracy %>% group_by(city) %>%
  summarize(avg_discrepancy = mean(temp_dif, na.rm = TRUE)) %>% arrange(desc(avg_discrepancy))

# Join tables to get citys' distance from coast
cities_forecasts_join <- discrepancy_per_city %>% left_join(cities, by = "city")

# Remove NA values
cities_forecasts_join <- na.omit(cities_forecasts_join)

# Explore relationship between discrepancy and factors like elevation, distance_to_coast, avg annual precipitation, and wind
ggplot(cities_forecasts_join, aes(x = distance_to_coast, y = avg_discrepancy)) +
  geom_point() +   
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Temp. Discrepancy vs Distance to Coast",
    x = "Distance to Coast (miles)",
    y = "Temp Discrepancy"
  ) +
  theme_minimal()

ggplot(cities_forecasts_join, aes(x = wind, y = avg_discrepancy)) +
  geom_point() +   
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Temp. Discrepancy vs Mean Wind Speed",
    x = "Mean Wind Speed",
    y = "Temp Discrepancy"
  ) +
  theme_minimal()

ggplot(cities_forecasts_join, aes(x = elevation, y = avg_discrepancy)) +
  geom_point() +   
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Temp. Discrepancy vs Elevation",
    x = "Elevation(meters)",
    y = "Temp Discrepancy"
  ) +
  theme_minimal()

# The outlier here in Juneau in Alaska
# It has a much higher annual precipitation, so we remove it
cities_forecasts_join <- cities_forecasts_join %>% filter(city != "JUNEAU")
ggplot(cities_forecasts_join, aes(x = avg_annual_precip, y = avg_discrepancy)) +
  geom_point() +   
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Temp. Discrepancy vs Avg Annual Precip.",
    x = "Average Annual Precipitation(inches)",
    y = "Temp Discrepancy"
  ) +
  theme_minimal()


# Correlation test for each factor


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
) %>% arrange(correlation)

# Rename columns for the graph
correlation_test_results <- correlation_test_results %>%
  mutate(test = recode(test,
                       "avg_precip" = "Avg Annual Precip",
                       "wind"   = "Wind Speed",
                       "elevation" = "Elevation",
                       "distance_to_coast" = "Distance to Coast"))

# Graph correlation test results in bar graph
ggplot(correlation_test_results, aes(x = reorder(test, correlation),
                                     y = correlation, fill = correlation > 0)) +
  geom_col() +
  labs(
    title = "Correlation Test Results",
    x = "Factor",
    y = "Correlation with Temperature Discrepancy"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    legend.position = "none" # Key is unnecessary
  )
  
