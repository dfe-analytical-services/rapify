library(readr)
library(curl)
library(dplyr)
library(tidyr)
library(stringr)
library(httr)
library(xml2)
library(jsonlite)
library(tibble)
library(dfeR)

source("R/ees-functions.R")

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

discarded_filters <- c("pa_flag", "academic_year")

pupil_school_file_indicators <- c("enrolments", "num_schools", "total_enrolments", "ytd_enrolments", "total_num_schools")

description_mapping <- data.frame(
  original = c("unauth_hol_perc",
               "unauth_late_registers_closed_perc",
               "unauth_not_yet_perc",
               "unauth_oth_perc",
               "auth_excluded_perc",
               "auth_grt_perc",
               "auth_holiday_perc",
               "auth_other_perc",
               "auth_religious_perc",
               "auth_study_perc",
               "illness_perc",
               "appointments_perc",
               "attendance_perc"),
  cleaned = c("reason_g_unauthorised_holiday_perc",
              "reason_u_unauthorised_late_after_registers_closed_perc",
              "reason_n_no_reason_yet_perc",
              "reason_o_other_unauthorised_perc",
              "reason_e_authorised_excluded_perc",
              "reason_t_authorised_grt_absence_perc",
              "reason_h_authorised_holiday_perc",
              "reason_c_authorised_other_perc",
              "reason_r_authorised_religious_observance_perc",
              "reason_s_authorised_study_leave_perc",
              "reason_i_authorised_illness_perc",
              "reason_m_authorised_medical_dental_perc",
              "overall_attendance_perc")
)

dm <- description_mapping %>% 
  ungroup %>%
  select(original, cleaned) %>%
  deframe


initial_clean <- function(attendance_data) {
  attendance_cleaned <- attendance_data %>%
    rename_with(~ paste0(., "_count"), starts_with("reason_")) %>%
    rename(any_of(setNames(description_mapping$original, description_mapping$cleaned))) %>%
    rename(any_of(setNames(paste0(description_mapping$original, "_scaled"), paste0(description_mapping$cleaned, "scaled")))) %>%
    rename(
      time_frame = breakdown,
      establishment_phase = school_type
    ) %>%
    mutate(
      time_identifier = paste("Week", time_identifier),
      time_frame = gsub("YTD", "Year to date", time_frame),
      day_number = if_else(time_frame == "Weekly", "Total", as.character(day_number))
    ) %>%
    rename_with(
      ~ paste0(., "_count"), 
      any_of(
        c("approved_educational_activity", "authorised_absence", "unauthorised_absence", 
             "late_sessions", "overall_absence", "overall_attendance", 
             "possible_sessions", "present_sessions")
      )
      ) %>%
    rename_with(~ stringr::str_replace_all(., "auth_", "authorised_")) %>%
    rename_with(~ stringr::str_replace_all(., "_perc_scaled", "_percscaled")) 
  time_lookup <- attendance_cleaned %>%
    select(attendance_date, time_period, time_identifier, week_commencing) %>%
    distinct() %>%
    arrange(week_commencing, time_period, time_identifier) %>%
    filter(
      !is.na(week_commencing),
      time_period == week_commencing %>% lubridate::year()
    )
  attendance_cleaned <- attendance_cleaned %>%
    select(-week_commencing, -time_period, -time_identifier) %>%
    left_join(time_lookup)
  attendance_cleaned
}

read_attendance <- function() {
  url <- "https://raw.githubusercontent.com/dfe-analytical-services/attendance-data-dashboard/main/data/attendance_data_dashboard.csv"
  data_file <- "attendance_data_dashboard.csv"
  if (!file.exists(paste0(data_folder, data_file))) {
    message(paste0(data_folder, data_file, "\n not found. Downloading from repository."))
    att_wide <- read_csv(url) %>%
      initial_clean()
    att_wide |> write_csv(paste0(data_folder, data_file))
  } else {
    message(paste0(data_folder, data_file, " found. Readng in from file
                   ."))
    att_wide <- read_csv(paste0(data_folder, data_file)) %>%
      initial_clean()
  }
  att_wide
}

create_reasons_tidy <- function() {
  att_underlying <- read_attendance()
  reason_filters <- data.frame(colname = names(att_underlying)) %>%
    filter(grepl("reason", colname)) %>%
    pull(colname)
  reason_tidy <- att_underlying %>%
    select(-all_of(c(pupil_school_file_indicators, discarded_filters))) %>%
    pivot_longer(
      !any_of(c(primary_filters)),
                 names_to = c("attendance_description", ".value"),
                 names_pattern = "^(.*)_(.*)"
    ) %>%
    mutate(
      reason = case_when(
        attendance_description == "reason_present" ~ "Total",
        grepl("reason_", attendance_description) ~ str_to_sentence(
          attendance_description %>% 
            sub("reason_", "", .) %>%
            gsub("_", " ", .)
          ),
        .default= 'Total'
      ),
      reason = gsub("aea ", "", reason),
      attendance_type = case_when(
        grepl("unauthorised", attendance_description) ~ "Unauthorised",
        grepl("authorised", attendance_description) ~ "Authorised",
        grepl("aea", attendance_description) ~ "Approved educational activity",
        grepl("approved_educational", attendance_description) ~ "Approved educational activity",
        grepl("present", attendance_description) ~ "Present",
        .default = 'Not determined'
      ),
      attendance_status = case_when(
        grepl("authorised", attendance_description) ~ "Absence",
        grepl("absence", attendance_description) ~ "Absence",
        grepl("aea", attendance_description) ~ "Attendance",
        grepl("attendance", attendance_description) ~ "Attendance",
        grepl("approved_educational", attendance_description) ~ "Attendance",
        grepl("present", attendance_description) ~ "Attendance",
        grepl("possible_sessions", attendance_description) ~ "Possible sessions",
        grepl("late_sessions", attendance_description) ~ "Late sessions",
        .default = 'Not determined'
      ),
      across(region_code:old_la_code, ~ if_else(is.na(.), "", as.character(.))),
      session_count = if_else(is.na(count), "x", format(count)),
      session_percent = if_else(is.na(perc), "x", format(dfeR::round_five_up(perc, dp=2))),
      session_scaled = if_else(is.na(percscaled), "x", format(dfeR::round_five_up(percscaled, dp=1)))
    ) %>%
    select(all_of(c(primary_filters, "attendance_description", "attendance_status", "attendance_type", "reason", "session_count", "session_percent", "session_scaled")))
  write_csv(reason_tidy, paste0(data_folder, "attendance_reasons.csv"))
  reason_meta <- meta_template(reason_tidy) %>%
    filter(!(col_name %in% c("attendance_date", "week_commencing", "time_frame"))) %>%
    mutate(
      filter_grouping_column = if_else(col_name == "day_number", "time_frame", "")
    )
  write_csv(reason_meta, paste0(data_folder, "attendance_reasons.meta.csv"))
  reason_tidy
}

