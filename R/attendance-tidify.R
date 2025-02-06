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

data_folder <- paste0(paste0(home_dir[[1]][1:3], collapse = "/"), "/offline-data/api-attendance/")

primary_filters <- c(
  "time_period", "time_identifier", "time_frame", "weekday", "geographic_level",
  "country_code", "country_name",
  "region_code", "region_name",
  "new_la_code", "la_name", "old_la_code",
  "week_commencing", "reference_date",
  "education_phase"
)


discarded_filters <- c("academic_year")

school_indicators <- c("num_schools", "total_num_schools")

enrolment_indicators <- c("enrolments", "total_enrolments", "ytd_enrolments")

persistent_absence_indicators <- c("pa_flag", "pa_perc", "pa_percscaled")

description_mapping <- data.frame(
  original = c(
    "day_number",
    "unauth_hol_perc",
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
    "attendance_perc"
  ),
  cleaned = c(
    "weekday",
    "reason_g_unauthorised_holiday_perc",
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
    "overall_attendance_perc"
  )
)

dm <- description_mapping %>%
  ungroup() %>%
  select(original, cleaned) %>%
  deframe()


initial_clean <- function(attendance_data) {
  attendance_cleaned <- attendance_data %>%
    rename_with(~ paste0(., "_count"), starts_with("reason_")) %>%
    rename(any_of(setNames(description_mapping$original, description_mapping$cleaned))) %>%
    rename(any_of(setNames(paste0(description_mapping$original, "_scaled"), paste0(description_mapping$cleaned, "scaled")))) %>%
    rename(
      time_frame = breakdown,
      education_phase = school_type,
      reference_date = attendance_date
    ) %>%
    mutate(
      time_identifier = case_when(
        time_frame == "YTD" ~ paste("Week", max(time_identifier)),
        .default = paste("Week", time_identifier)
      ),
      time_frame = case_when(
        time_frame == "Weekly" ~ "Week",
        time_frame == "YTD" ~ "Year to date",
        weekday == 1 ~ "Monday",
        weekday == 2 ~ "Tuesday",
        weekday == 3 ~ "Wednesday",
        weekday == 4 ~ "Thursday",
        weekday == 5 ~ "Friday",
        .default = "Week"
      )
    ) %>%
    rename_with(
      ~ paste0(., "_count"),
      any_of(
        c(
          "approved_educational_activity", "authorised_absence", "unauthorised_absence",
          "late_sessions", "overall_absence", "overall_attendance",
          "possible_sessions", "present_sessions"
        )
      )
    ) %>%
    rename_with(~ stringr::str_replace_all(., "auth_", "authorised_")) %>%
    rename_with(~ stringr::str_replace_all(., "_perc_scaled", "_percscaled"))
  message("Number of rows in input data :", nrow(attendance_cleaned))
  time_lookup <- attendance_cleaned %>%
    select(reference_date, time_period, time_identifier, week_commencing) %>%
    distinct() %>%
    arrange(week_commencing, time_period, time_identifier) %>%
    filter(
      !is.na(week_commencing),
      time_period == week_commencing %>% lubridate::year()
    )
  attendance_cleaned <- attendance_cleaned %>%
    select(-week_commencing, -time_period, -time_identifier) %>%
    left_join(
      time_lookup,
      by = c("reference_date")
    )
  message("Number of rows in cleaned data :", nrow(attendance_cleaned))
  attendance_cleaned
}

read_attendance <- function(refresh = FALSE) {
  url <- "https://raw.githubusercontent.com/dfe-analytical-services/attendance-data-dashboard/main/data/attendance_data_dashboard.csv"
  data_file <- "attendance_data_dashboard.csv"
  if (refresh || !file.exists(paste0(data_folder, data_file))) {
    message(paste0(data_folder, data_file, "\n not found. Downloading from repository."))
    att_wide <- read_csv(url)
    att_wide |> write_csv(paste0(data_folder, data_file))
    att_wide <- att_wide %>%
      initial_clean()
  } else {
    message(paste0(data_folder, data_file, " found. Readng in from file
                   ."))
    att_wide <- read_csv(paste0(data_folder, data_file)) %>%
      initial_clean()
  }
  att_wide
}

create_reasons_tidy <- function(refresh = FALSE) {
  att_underlying <- read_attendance(refresh = refresh)
  reason_filters <- data.frame(colname = names(att_underlying)) %>%
    filter(grepl("reason", colname)) %>%
    pull(colname)
  reason_tidy <- att_underlying %>%
    select(-all_of(
      c(
        school_indicators,
        enrolment_indicators,
        persistent_absence_indicators,
        discarded_filters
      )
    )) %>%
    pivot_longer(
      !any_of(c(primary_filters)),
      names_to = c("attendance_description", ".value"),
      names_pattern = "^(.*)_(.*)"
    ) %>%
    mutate(
      attendance_description = gsub("_mob", "_mobile_child", attendance_description),
      attendance_reason = case_when(
        grepl("reason_", attendance_description) | attendance_description %in% c("pa", "excluded") ~ str_to_sentence(
          attendance_description %>%
            sub("reason_", "", .) %>%
            gsub("_", " ", .)
        ),
        attendance_description %in% c("authorised_mobile_child", "authorised_performance", "authorised_interview", "authorised_part_time") ~ attendance_description |>
          str_replace("authorised", "") |>
          str_replace("_", " ") |>
          str_trim() |> str_to_sentence(),
        .default = "Total"
      ),
      attendance_reason = gsub("aea ", "", attendance_reason),
      attendance_type = case_when(
        grepl("unauthorised", attendance_description) ~ "Unauthorised",
        grepl("authorised", attendance_description) ~ "Authorised",
        grepl("aea", attendance_description) ~ "Approved educational activity",
        grepl("approved_educational", attendance_description) ~ "Approved educational activity",
        grepl("present", attendance_description) ~ "Present",
        .default = "Not determined"
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
        .default = "Not determined"
      ),
      across(region_code:old_la_code, ~ if_else(is.na(.), "", as.character(.))),
      session_count = if_else(is.na(count), "x", format(count)),
      session_percent = if_else(is.na(perc), "x", format(dfeR::round_five_up(perc, dp = 2))),
      session_scaled = if_else(is.na(percscaled), "x", format(dfeR::round_five_up(percscaled, dp = 1)))
    ) %>%
    mutate(
      attendance_reason = case_when(
        attendance_status != "Not determined" & attendance_type != "Not determined" & attendance_reason == "Total" ~ paste("All", tolower(attendance_type)),
        attendance_status != "Not determined" & attendance_type == "Not determined" & attendance_reason == "Total" ~ paste("All", tolower(attendance_status)),
        .default = attendance_reason
      ),
      attendance_type = case_when(
        attendance_status != "Not determined" & attendance_type == "Not determined" ~ paste("All", tolower(attendance_status)),
        .default = attendance_type
      ),
      attendance_reason = paste(stringr::str_replace(attendance_reason, "^[A-Z] ", ""), stringr::str_extract(attendance_reason, "^[A-Z] ")) |>
        str_replace(" NA", "") |>
        str_replace(" ([A-Z]) $", " \\(\\1\\)") |>
        str_to_sentence()
    ) %>%
    select(all_of(c(primary_filters, "attendance_description", "attendance_status", "attendance_type", "attendance_reason", "session_count", "session_percent", "session_scaled")))
  write_csv(reason_tidy, paste0(data_folder, "attendance_data_api.csv"))
  reason_meta <- meta_template(reason_tidy) %>%
    filter(!(col_name %in% c("weekday", "attendance_description", "week_commencing"))) %>%
    mutate(
      filter_grouping_column = "",
      col_type = case_when(
        col_name %in% c("session_count", "session_percent", "session_scaled", "reference_date") ~ "Indicator",
        .default = "Filter"
      )
    )
  duplicated_rows_desc <- reason_tidy |>
    select(-session_count, -session_percent, -session_scaled, -weekday, -week_commencing, -attendance_description) |>
    filter(geographic_level == "National") |>
    summarise(count = n(), .by = everything()) |>
    filter(count > 1) |>
    left_join(
      x |> select(-session_count, -session_percent, -session_scaled, -weekday, -week_commencing) |>
        filter(geographic_level == "National")
    ) |>
    select(attendance_description) |>
    distinct()
  if (nrow(duplicated_rows_desc) > 0){print(duplicated_rows_desc)}
  write_csv(reason_meta, paste0(data_folder, "attendance_data_api.meta.csv"))
  reason_tidy
}

create_enrol_tidy <- function() {
  att_underlying <- read_attendance()
  tidy_enrol_pa <- att_underlying |>
    select(all_of(c(primary_filters, school_indicators, enrolment_indicators))) |>
    rename(
      school_count_submitted = num_schools,
      school_count_all = total_num_schools,
      enrolment_count_submitted = enrolments,
      enrolment_count_all = total_enrolments,
      enrolments_year_to_date = ytd_enrolments
    ) |>
    mutate(
      across(region_code:old_la_code, ~ if_else(is.na(.), "", as.character(.))),
      across(school_count_submitted:enrolments_year_to_date, ~ if_else(is.na(.), "x", as.character(.))),
    )
  write_csv(tidy_enrol_pa, paste0(data_folder, "attendance_enrol_api.csv"))
  enrol_pa_meta <- meta_template(tidy_enrol_pa) %>%
    filter(!(col_name %in% c("attendance_description", "reference_date", "week_commencing", "time_frame"))) %>%
    mutate(
      filter_grouping_column = if_else(col_name == "weekday", "time_frame", ""),
      col_type = case_when(
        grepl("school_count|enrolment", col_name) ~ "Indicator",
        .default = "Filter"
      )
    )
  write_csv(enrol_pa_meta, paste0(data_folder, "attendance_enrol_api.meta.csv"))
}

create_persistent_absence_tidy <- function() {
  att_underlying <- read_attendance()
  tidy_enrol_pa <- att_underlying |>
    select(all_of(c(primary_filters, persistent_absence_indicators))) |>
    rename(
      persistent_absence_flag = pa_flag,
      persistent_absence_percent = pa_perc,
      persistent_absence_percent_scaled = pa_percscaled
    ) |>
    mutate(
      across(region_code:old_la_code, ~ if_else(is.na(.), "", as.character(.))),
      across(persistent_absence_flag:persistent_absence_percent_scaled, ~ if_else(is.na(.), "x", as.character(.))),
    ) |>
    filter(time_frame == "Year to date")
  write_csv(tidy_enrol_pa, paste0(data_folder, "attendance_persistent_absence_api.csv"))
  enrol_pa_meta <- meta_template(tidy_enrol_pa) %>%
    filter(!(col_name %in% c("weekday", "attendance_description", "reference_date", "week_commencing", "time_frame"))) %>%
    mutate(
      filter_grouping_column = if_else(col_name == "weekday", "time_frame", ""),
      col_type = case_when(
        grepl("persistent_absence", col_name) ~ "Indicator",
        .default = "Filter"
      )
    )
  write_csv(enrol_pa_meta, paste0(data_folder, "attendance_persistent_absence_api.meta.csv"))
}
