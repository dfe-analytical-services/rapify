library(readr)
library(curl)
library(dplyr)
library(tidyr)
library(stringr)

home_dir <- Sys.getenv("HOME") |> strsplit("\\\\")

data_folder <- paste0(paste0(home_dir[[1]][1:3], collapse = "/"), "/test-data/api-attendance/")

primary_filters <- c(
  "time_period", "time_identifier", "time_frame", "geographic_level",
  "country_code", "country_name",
  "region_code", "region_name",
  "new_la_code", "la_name", "old_la_code",
  "week_commencing", "day_number", "attendance_date",
  "establishment_phase"
)

read_attendance <- function() {
  url <- "https://raw.githubusercontent.com/dfe-analytical-services/attendance-data-dashboard/main/data/attendance_data_dashboard.csv"
  data_file <- "attendance_data_dashboard.csv"
  if (!file.exists(paste0(data_folder, data_file))) {
    message(paste0(data_folder, data_file, "\n not found. Downloading from repository."))
    att_wide <- read_csv(url) %>%
      rename(
        time_frame = breakdown,
        establishment_phase = school_type
        ) %>%
      mutate(time_identifier = paste("Week", time_identifier))
    att_wide |> write_csv(paste0(data_folder, data_file))
  } else {
    message(paste0(data_folder, data_file, " found. Readng in from file
                   ."))
    att_wide <- read_csv(paste0(data_folder, data_file)) %>%
      rename(
        time_frame = breakdown,
        establishment_phase = school_type
        ) %>%
      mutate(time_identifier = paste("Week", time_identifier))
  }
  att_wide
}

create_reasons_tidy <- function() {
  att_underlying <- read_attendance()
  reason_filters <- data.frame(colname = names(att_underlying)) %>%
    filter(grepl("reason", colname)) %>%
    pull(colname)
  reason_tidy <- att_underlying %>%
    select(all_of(c(primary_filters, reason_filters))) %>%
    pivot_longer(
      cols = starts_with("reason_"),
      names_to = "reason",
      names_prefix = "reason_",
      values_to = "pupil_count"
    ) %>%
    mutate(
      reason = str_to_sentence(reason %>% gsub("_", " ", .)),
      reason = gsub("aea", "AEA", reason),
      across(region_code:old_la_code, ~ if_else(is.na(.), "", as.character(.)))
    ) %>%
    select(all_of(c(primary_filters, "reason", "pupil_count")))
  write_csv(reason_tidy, paste0(data_folder, "attendance_reasons.csv"))
  reason_meta <- meta_template(reason_tidy) %>% 
    filter(!(col_name %in% c("attendance_date", "week_commencing")))
  write_csv(reason_meta, paste0(data_folder, "attendance_reasons.meta.csv"))
  reason_tidy
}
