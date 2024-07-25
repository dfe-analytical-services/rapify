library(readr)
library(curl)
library(dplyr)
library(tidyr)
library(stringr)
library(httr)
library(xml2)
library(jsonlite)

source("R/ees-functions.R")

check_api_reasons <- function() {
  url <- "https://dev.statistics.api.education.gov.uk/api/v1.0/data-sets/53e59001-f5b5-9370-8527-8b7ff006b114"
  response <- httr::GET(url)
  content(response)
}


read_api_reasons <- function(
    parse = TRUE,
    time_frame = "Latest week",
    geographic_level = "National",
    establishment_phase = "Primary",
    area_name = "England",
    dataset_id = "53e59001-f5b5-9370-8527-8b7ff006b114") {
  # Get the data-set meta data
  meta_response <- httr::GET(paste0(
    "https://dev.statistics.api.education.gov.uk/api/v1.0/data-sets/",
    dataset_id,
    "/meta"
  ))
  filters <- parse_ees_api_meta_filters(meta_response)
  meta <- meta_response %>%
    content("text") %>%
    fromJSON()
  latest_week <- meta$timePeriods %>%
    mutate(year_week = as.numeric(paste0(period, ".", str_pad(gsub("W", "", code), 2, pad = 0)))) %>%
    pull(year_week) %>%
    max() %>%
    format() %>%
    gsub(".*\\.", "", .)
  
  # Define the query url
  url <- paste0(
    "https://dev.statistics.api.education.gov.uk/api/v1.0/data-sets/",
    dataset_id,
    "/query"
  )
  
  if (time_frame == "Latest week") {
    time_period_query <- paste0(
      '{
      "timePeriods": {
        "in": [
          {
            "period": "2024",
            "code": "W', latest_week, '"
          }
        ]
      }
    }'
    )
    time_frame_query <- filter_query("time_frame", c("Daily", "Weekly"), filters)
  }
  
  # Create the query
  body <- paste0(
    '{
        "criteria": {
           "and": [
             ', geography_query(geographic_level),
    ",", time_period_query,
    ",",
    filter_query("establishment_phase", establishment_phase, filters),
    ",", time_frame_query, '
    ]
  },
  "indicators": [
    "pupil_count"
  ],
  "debug": true,
  "page": 1,
  "pageSize": 1000
}'
  )
  cat(body, file = "temp.txt")
  
  response <- httr::POST(
    url,
    body = body,
    encode = "json",
    content_type("application/json")
  )
  output <- content(response)
  if (parse) {
    output <- parse_ees_api_output(output)
  }
  output
}
