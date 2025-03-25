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

# The scripts expect the old-format dashboard data file to be in:
# c:/Users/username/offline-data/api-attendance/
# That's my go to for large (or unpublished) data files so that OneDrive doesn't start trying to 
# sync them. You can use a different directory, but you'll need to set it here:
data_folder <- paste0(paste0(home_dir[[1]][1:3], collapse = "/"), "/offline-data/api-attendance/")
# It also assumes the file itself is saved as something like:
# attendance_data_dashboard_2025_week10.csv
# That can be changed further down if you don't like that convention.

# Assuming you go with the above conventions for how to save the file, then you should just need
# the following commands (updating the year/week reference as needed):
# source("C:/Users/rbielby/repos/rapify/R/attendance-tidify.R")
#   reasons_data <- create_reasons_tidy(source = "2025_week10")
#   pa_data <- create_persistent_absence_tidy(source = "2025_week10")
#   school_returns_data <- create_school_returns_tidy(source = "2025_week10")
# Those will write out the following files to the directory given by data_folder:
#   - pupil-attendance-reasons_2025_week10.csv / pupil-attendance-reasons_2025_week10.meta.csv
#   - attendance_persistent_absence_2025_week10.csv / attendance_persistent_absence_2025_week10.meta.csv
#   - attendance_submitting_school_counts_2025_week10.csv / attendance_submitting_school_counts_2025_week10.meta.csv

# Note: Excel can't handle the resulting reasons_data file, because Excel is just a bit rubbish at handling large data sets. 
# Best way to inspect it is to stay in R and use variants on this code to check what weeks have been included:
#   tidy_data |> 
#     select(time_period, time_identifier) |> 
#     distinct() |> 
#     View()


primary_filters <- c(
  "time_period", "time_identifier", "time_frame", "geographic_level",
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
    "auth_performance_perc",
    "auth_part_time_perc",
    "auth_interview_perc",
    "auth_mob_perc",
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
    "reason_c1_authorised_regulated_performance_perc",
    "reason_c2_authorised_temp_reduced_timetable_perc",
    "reason_j1_authorised_interview_perc",
    "reason_t_authorised_mobile_child_perc",
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
  date_format <- lubridate::guess_formats(
    attendance_data$attendance_date |> unique(), 
    c("ymd", "dmy")
  )
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
      reference_date = lubridate::as_date(reference_date, format = date_format),
      week_commencing = lubridate::as_date(week_commencing, format = date_format),
      time_identifier = case_when(
        time_frame == "YTD" ~ paste("Week", max(time_identifier, na.rm = TRUE)),
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
      ),
      education_phase = case_when(
        education_phase == "Total" ~ "All schools",
        .default = education_phase
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
    rename_with(~ stringr::str_replace_all(., "_perc_scaled", "_percscaled")) |>
    select(-weekday)
  message("Number of rows in input data: ", nrow(attendance_cleaned))
  time_lookup <- attendance_cleaned %>%
    select(reference_date, time_period, time_identifier, week_commencing) %>%
    distinct() %>%
    arrange(week_commencing, time_period, time_identifier) %>%
    filter(
      !is.na(week_commencing),
      time_period == week_commencing %>%
        lubridate::year()
    )
  latest_week_beginning <- attendance_cleaned %>%
    filter(time_frame == "Week") |>
    pull(reference_date) |>
    max(na.rm = TRUE)
  message("Most recent week commencing date:", latest_week_beginning)
  attendance_cleaned <- attendance_cleaned %>%
    select(-week_commencing, -time_period, -time_identifier) %>%
    mutate(
      reference_date = case_when(
        time_frame == "Year to date" ~ latest_week_beginning,
        .default = reference_date
      )
    ) |>
    left_join(
      time_lookup,
      by = c("reference_date")
    )
  message("Number of rows in cleaned data: ", nrow(attendance_cleaned))
  attendance_cleaned
}

read_attendance <- function(source, refresh = NULL) {
  if(is.null(refresh)){refresh = FALSE}
  if (source == "github") {
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
  } else {
    data_file <- paste0("attendance_data_dashboard_", source, ".csv")
    att_wide <- read_csv(paste0(data_folder, data_file)) %>%
      initial_clean()
  }
  att_wide
}

create_reasons_tidy <- function(source = "2025_week7", refresh = FALSE) {
  att_underlying <- read_attendance(source = source, refresh = refresh)
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
      attendance_reason = case_when(
        attendance_description == "reason_h_authorised_holiday" ~ "Authorised holiday (h)",
        attendance_description == "reason_g_unauthorised_holiday" ~ "Unauthorised holiday (g)",
        attendance_description == "reason_c_authorised_other" ~ "Other authorised (c)",
        attendance_description == "reason_o_other_unauthorised" ~ "Other unauthorised (o)",
        grepl("reason_", attendance_description) | attendance_description %in% c("pa", "excluded") ~ attendance_description %>%
          sub("reason_", "", .) %>%
          gsub("_", " ", .) |>
          str_replace("temp ", "temporary ") |>
          str_replace("unauthorised", "") |>
          str_replace("authorised", "") |>
          str_replace("aea", "") |>
          str_trim(side = "both") |>
          str_to_sentence(),
        grepl("overall", attendance_description) ~ attendance_description |>
          str_replace("_", " ") |>
          str_trim(side = "both") |>
          str_to_sentence(),
        .default = "Total"
      ),
      attendance_type = case_when(
        grepl("unauthorised", attendance_description) ~ "Unauthorised",
        grepl("authorised", attendance_description) ~ "Authorised",
        grepl("aea", attendance_description) ~ "Approved educational activity",
        grepl("approved_educational", attendance_description) ~ "Approved educational activity",
        grepl("present", attendance_description) ~ "Present",
        grepl("absence", attendance_description) ~ "Overall absence",
        grepl("attendance", attendance_description) ~ "Overall attendance",
        grepl("possible_sessions", attendance_description) ~ "All possible sessions",
        grepl("late_sessions", attendance_description) ~ "All late sessions",
        grepl("no_reason_yet", attendance_description) ~ "Unauthorised",
        .default = "Management and legacy codes"
      ),
      attendance_status = case_when(
        grepl("authorised", attendance_description) ~ "Absence",
        grepl("absence", attendance_description) ~ "Absence",
        grepl("no_reason_yet", attendance_description) ~ "Absence",
        grepl("aea", attendance_description) ~ "Attendance",
        grepl("attendance", attendance_description) ~ "Attendance",
        grepl("approved_educational", attendance_description) ~ "Attendance",
        grepl("present", attendance_description) ~ "Attendance",
        grepl("possible_sessions", attendance_description) ~ "Possible sessions",
        grepl("late_sessions", attendance_description) ~ "Late sessions",
        .default = "Management and legacy codes"
      ),
      across(region_code:old_la_code, ~ if_else(is.na(.), "", as.character(.))),
      session_count = if_else(is.na(count), "x", as.character(count)),
      session_percent = if_else(is.na(perc), "x", as.character(dfeR::round_five_up(perc, dp = 2))),
      session_scaled = if_else(is.na(percscaled), "x", as.character(dfeR::round_five_up(percscaled, dp = 1)))
    ) %>%
    mutate(
      attendance_reason = case_when(
        attendance_reason == "Total" ~ paste("All", tolower(attendance_type) |> str_replace("^all ","")),
        .default = attendance_reason
      ),
      attendance_type = case_when(
        attendance_status != "Not determined" & attendance_type == "Not determined" ~ paste("All", tolower(attendance_status)),
        .default = attendance_type
      ),
      attendance_reason = paste(
        stringr::str_replace(attendance_reason, "^[A-Z][1-9] |^[A-Z] ", ""),
        stringr::str_extract(attendance_reason, "^[A-Z][1-9] |^[A-Z] ")
      ) |>
        str_replace(" NA", "") |>
        str_replace(" ([A-Z]) $", " \\(\\1\\)") |>
        str_replace(" ([A-Z][1-9]) $", " \\(\\1\\)") |>
        str_trim(side = "both") |>
        str_to_sentence() |>
        str_replace("La ", "LA ") |>
        str_replace(" la ", " LA ")
    ) %>%
    select(all_of(c(primary_filters, "attendance_status", "attendance_type", "attendance_reason", "attendance_description", "session_count", "session_percent", "session_scaled"))) |>
    arrange(time_period, time_identifier, country_code, region_code, new_la_code, education_phase)
  write_csv(
    reason_tidy |> select(-any_of(c("attendance_description", "week_commencing"))),
    paste0(data_folder, "pupil-attendance-reasons_", source, ".csv")
  )
  reason_meta <- meta_template(reason_tidy) %>%
    filter(!(col_name %in% c("weekday", "attendance_description", "week_commencing"))) %>%
    mutate(
      filter_grouping_column = "",
      col_type = case_when(
        col_name %in% c("session_count", "session_percent", "session_scaled", "reference_date") ~ "Indicator",
        .default = "Filter"
      ),
      label = case_when(
        col_name == "session_count" ~ "Number of sessions",
        col_name == "session_percent" ~ "Percent of sessions",
        col_name == "session_scaled" ~ "Number of sessions (scaled)",
        .default = label
      ),
      indicator_dp = case_when(
        col_name == "session_count" ~ "0",
        col_name == "session_percent" ~ "1",
        col_name == "session_scaled" ~ "0",
        .default = indicator_dp
      ),
      filter_grouping_column = case_when(
        col_name == "attendance_type" ~ "attendance_status",
        col_name == "attendance_reason" ~ "attendance_type",
        .default = filter_grouping_column
      ) 
    ) |>
    select(-filter_default)
  write_csv(reason_meta, paste0(data_folder, "pupil-attendance-reasons_", source, ".meta.csv"))
  # duplicated_rows_desc <- reason_tidy |>
  #   select(-session_count, -session_percent, -session_scaled, -weekday, -week_commencing, -attendance_description) |>
  #   filter(geographic_level == "National") |>
  #   summarise(count = n(), .by = everything()) |>
  #   filter(count > 1) |>
  #   left_join(
  #     reason_tidy |> select(-session_count, -session_percent, -session_scaled, -weekday, -week_commencing) |>
  #       filter(geographic_level == "National")
  #   ) |>
  #   select(attendance_description) |>
  #   distinct()
  # if (nrow(duplicated_rows_desc) > 0){print(duplicated_rows_desc)}
  reason_tidy
}

# Note that to avoid creating repeated major version changes, I'm appending previous data to the 
# latest data. That means the resulting file has the year to date as measured for each 2 week 
# release. Will look into a cleaner way to do this in future, but may need updates to EES itself.
create_persistent_absence_tidy <- function(
    source = "2025_week10", 
    append_to = NULL, 
    refresh = NULL) {
  if (is.null(append_to)){
    append_to <- source |> stringr::str_split( "week", simplify = TRUE)
    append_to <- paste0(append_to[1], "week", as.numeric(append_to[2])-2)
  }
  att_underlying <- read_attendance(source = source, refresh = refresh)
  tidy_enrol_pa <- att_underlying |>
    select(all_of(c(primary_filters, persistent_absence_indicators))) |>
    rename(
      persistent_absence_flag = pa_flag,
      persistent_absence_percent = pa_perc,
      persistent_absence_percent_scaled = pa_percscaled
    ) |>
    mutate(
      across(region_code:old_la_code, ~ if_else(is.na(.), "", as.character(.))),
      across(
        persistent_absence_flag:persistent_absence_percent_scaled,
        ~ if_else(
          is.na(.), 
          "x", 
                  . |> dfeR::round_five_up(dp = 2) |> as.character()
          )
        )
    ) |>
    filter(time_frame == "Year to date") |>
    arrange(time_period, time_identifier, country_code, region_code, new_la_code, education_phase)
  if(append_to != "no-append"){
    existing_data <- read_csv(paste0(data_folder, "attendance_persistent_absence_", append_to, ".csv")) |>
      mutate(
        across(region_code:old_la_code, ~ if_else(is.na(.), "", as.character(.)))
      )
    tidy_enrol_pa <- tidy_enrol_pa |> 
      bind_rows(existing_data)
  }
  write_csv(
    tidy_enrol_pa |>
      select(-any_of(c("weekday", "attendance_description", "reference_date", "week_commencing", "persistent_absence_flag", "persistent_absence_percent_scaled"))),
    paste0(data_folder, "attendance_persistent_absence_", source, ".csv")
  )
  enrol_pa_meta <- meta_template(tidy_enrol_pa) %>%
    # Note time frame needs filtering out from the meta until we implement something that allows it as a time_identifier or time_label
    # It's fine in the other file, but here it's only got one value (Year to date), so is rejected by the screener
    filter(!(col_name %in% c("time_frame", "weekday", "attendance_description", "reference_date", "week_commencing", "persistent_absence_flag", "persistent_absence_percent_scaled"))) %>%
    mutate(
      col_type = case_when(
        grepl("persistent_absence", col_name) ~ "Indicator",
        col_name == "reference_date" ~ "Indicator",
        .default = "Filter"
      ),
      label = case_when(
        col_name == "persistent_absence_percet" ~ "Persistent absence rate",
        .default = label
      )
    ) |>
    select(-filter_default)
  write_csv(enrol_pa_meta, paste0(data_folder, "attendance_persistent_absence_", source, ".meta.csv"))
  return(tidy_enrol_pa)
}

create_school_returns_tidy <- function(source = "2025_week7", refresh = NULL) {
  att_underlying <- read_attendance(source = source, refresh = refresh)
  tidy_enrol_schools <- att_underlying |>
    select(all_of(c(primary_filters, school_indicators))) |>
    rename(school_submitted_count = num_schools, school_all_count = total_num_schools) |>
    mutate(
      across(starts_with("school_"), ~ if_else(is.na(.), "x", as.character(.))),
      across(region_code:old_la_code, ~ if_else(is.na(.), "", as.character(.)))
      ) |>
    arrange(time_period, time_identifier, country_code, region_code, new_la_code, education_phase)
  write_csv(
    tidy_enrol_schools |>
      select(-any_of(c("weekday", "week_commencing"))),
    paste0(data_folder, "attendance_submitting_school_counts_", source, ".csv")
  )
  enrol_school_meta <- meta_template(tidy_enrol_schools) %>%
    filter(!(col_name %in% c("weekday", "week_commencing"))) %>%
    mutate(
      col_type = case_when(
        grepl("count", col_name) ~ "Indicator",
        col_name == "reference_date" ~ "Indicator",
        .default = "Filter"
      ),
      label = case_when(
        col_name == "school_submitted_count" ~ "Number of schools submitting",
        col_name == "school_all_count" ~ "Total number of schools",
        .default = label
      ),
      indicator_dp = case_when(
        grepl("count", col_name) ~ "0",
        .default = ""
      )
    ) |>
    select(-filter_default)
  write_csv(enrol_school_meta, paste0(data_folder, "attendance_submitting_school_counts_", source, ".meta.csv"))
  return(tidy_enrol_schools)
}


# Test run lines for reasons data
# tidy_data <- create_reasons_tidy(source = "2025_week10")
# tidy_data |> select(time_period, time_identifier) |> distinct() |> print(n = 25)
# 
# tidy_file_data <- read_csv("../../offline-data/api-attendance/pupil-attendance-reasons_2025_week10.csv")
# tidy_file_data |> select(time_period, time_identifier) |> distinct() |> print(n = 25)


# Started this next function for enrolments, but didn't seem needed in the end. Have left it in, in 
# case it becomes useful as a starting point for enrolments later down the line
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
  write_csv(tidy_enrol_pa, paste0(data_folder, "attendance_enrol_", source, ".csv"))
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
