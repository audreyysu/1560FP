#' WEATHER.R - ADDING WEATHER DATA TO df
#' 4 FUNCTIONS: snap_to_grid(), get_weathercode_day(),
#' fetch_weather_for_df(), decode_weather()

library(openmeteo)
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)

#' Snap Latitude and Longitude to a Grid
#'
#' Rounds StopLat, StopLng columns to customizable resolution for ERA5 grid 
#' matching and makes subsequent steps much faster
#'
#' @param df data.frame with `StopLat` and `StopLng` numeric columns.
#' @param resolution numeric, grid resolution in degrees (default 0.25) 
#'
#' @return df with four additional columns:
#' 1. `Lat_round`: StopLat rounded to 3 decimals.
#' 2. `Lng_round`: StopLng rounded to 3 decimals.
#' 3. `Lat_grid`: StopLat snapped to grid resolution.
#' 4. `Lng_grid`: StopLng snapped to grid resolution.
#' 
#' @examples
#' df <- data.frame(StopLat = c(41.823, 41.829), StopLng = c(-71.5, -71.48))
#' df <- snap_to_grid(df)
snap_to_grid <- function(df, resolution = 0.25) {
  df %>%
    mutate(
      Lat_round = round(StopLat, 3),
      Lng_round = round(StopLng, 3),
      Lat_grid = round(StopLat / resolution) * resolution,
      Lng_grid = round(StopLng / resolution) * resolution
    )
}

#' Fetch Hourly Weather Data for Specific Day
#'
#' Retrieves hourly historical weather data for given latitude, longitude, and 
#' date from Open-Meteo API
#'
#' @param lat Numeric. Latitude of the location.
#' @param lon Numeric. Longitude of the location.
#' @param date Date. For which to retrieve hourly weather.
#'
#' @return tibble of hourly weather data for the specified day
#' 
#' @examples
#' df_hourly <- get_weathercode_day(41.75, -71.50, as.Date("2024-05-01"))
get_weathercode_day <- function(lat, lon, date) {
  date_str <- format(date, "%Y-%m-%d")
  
  res <- tryCatch({
    weather_history(
      location = c(lat, lon),
      start = date_str,
      end   = date_str,
      hourly = "weathercode",
      model  = "era5",
      timezone = "UTC"
    )
  }, error = function(e) NULL)
  
  if (is.null(res)) return(NULL)
  res
}

#' Fetch Weather for All Unique Stop/Day Combinations
#'
#' Retrieves hourly weather data for each unique `Lat_grid`/`Lng_grid`/`Date` 
#' combination 
#'
#' @param df containing at least `Lat_grid`, `Lng_grid`, and `Rounded.Time`
#'
#' @return df with two additional columns:
#' 1. `weathercode`: numeric weather code for the given hour.
#' 2. `weather_desc`: human-readable description of the weather code.
#'
#' @examples
#' df <- fetch_weather(df)
fetch_weather <- function(df) {
  # unique stop/day combos to reduce runtime
  unique_queries <- df %>%
    distinct(Lat_grid, Lng_grid, Date)
  
  # full-day weather for each unique stop/day
  unique_queries <- unique_queries %>%
    rowwise() %>%
    mutate(day_weather = list(get_weathercode_day(Lat_grid, Lng_grid, Date))) %>%
    ungroup()
  
  unique_queries_expanded <- unique_queries %>%
    filter(!map_lgl(day_weather, is.null)) %>%
    unnest(cols = c(day_weather)) %>%
    rename(Rounded.Time = datetime,
           weathercode = hourly_weathercode) %>%
    select(Lat_grid, Lng_grid, Rounded.Time, weathercode)
  
  # join back to df
  df <- df %>%
    left_join(unique_queries_expanded,
              by = c("Lat_grid", "Lng_grid", "Rounded.Time"))
  
  # decode weathercodes thru labels
  weather_labels <- c(
    "0"  = "Clear sky",
    "1"  = "Mainly clear",
    "2"  = "Partly cloudy",
    "3"  = "Overcast",
    "45" = "Fog",
    "48" = "Depositing rime fog",
    "51" = "Drizzle: Light",
    "53" = "Drizzle: Moderate",
    "55" = "Drizzle: Dense",
    "56" = "Freezing drizzle: Light",
    "57" = "Freezing drizzle: Dense",
    "61" = "Rain: Slight",
    "63" = "Rain: Moderate",
    "65" = "Rain: Heavy",
    "66" = "Freezing rain: Light",
    "67" = "Freezing rain: Heavy",
    "71" = "Snow fall: Slight",
    "73" = "Snow fall: Moderate",
    "75" = "Snow fall: Heavy",
    "77" = "Snow grains",
    "80" = "Rain showers: Slight",
    "81" = "Rain showers: Moderate",
    "82" = "Rain showers: Violent",
    "85" = "Snow showers: Slight",
    "86" = "Snow showers: Heavy",
    "95" = "Thunderstorm: Slight or moderate",
    "96" = "Thunderstorm with slight hail",
    "99" = "Thunderstorm with heavy hail"
  )
  
  df$weather_desc <- weather_labels[as.character(df$weathercode)]
  
  # add weather_cat (broader categories) for modeling and visualization
  df <- df %>%
    mutate(
      weather_cat = case_when(
        grepl("Dense|Heavy|Moderate|Light|Slight|Drizzle|Snow", weather_desc) ~ "precip",
        grepl("Clear|Mainly clear|Partly cloudy|Overcast", weather_desc) ~ "clear",
        TRUE ~ "other"
      )
    )
  
  df
}