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

# Explore relationship between discrepancy and factors like elevation, distance_to_coast, avg annual precipitation, and wind
ggplot(cities_forecasts_join, aes(x = distance_to_coast, y = avg_discrepancy)) +
  geom_point() +   
  geom_smooth(method = "loess",span = .5, se = FALSE, color = "blue") +
  labs(
    title = "Temperature Discrepancy vs Distance to Coast",
    x = "Distance to Coast (miles)",
    y = "Temp Discrepancy"
  ) +
  theme_minimal()

ggplot(cities_forecasts_join, aes(x = wind, y = avg_discrepancy)) +
  geom_point() +   
  geom_smooth(method = "loess",span = .5, se = FALSE, color = "blue") +
  labs(
    title = "Temperature Discrepancy vs Wind",
    x = "Wind)",
    y = "Temp Discrepancy"
  ) +
  theme_minimal()

ggplot(cities_forecasts_join, aes(x = elevation, y = avg_discrepancy)) +
  geom_point() +   
  geom_smooth(method = "loess",span = .5, se = FALSE, color = "blue") +
  labs(
    title = "Temperature Discrepancy vs Elevation",
    x = "Elevation",
    y = "Temp Discrepancy"
  ) +
  theme_minimal()

ggplot(cities_forecasts_join, aes(x = avg_annual_precip, y = avg_discrepancy)) +
  geom_point() +   
  geom_smooth(method = "loess",span = .5, se = FALSE, color = "blue") +
  labs(
    title = "Temperature Discrepancy vs Average Annual Precipitation",
    x = "Distance to Coast (miles)",
    y = "Average Annual Precipitation"
  ) +
  theme_minimal()

