library(readr)
library(dplyr)
library(lubridate)
library(stringr)
source("R/ees-functions.R")
# Convert times to attendance structure
# Clean up abbreviations and acronyms
# Make sure filter_grouping_columns are being used

data_dir <- "../../offline-data/further-education/"

process_monthly_starts_ees <- function(
    ees_file = "../../offline-data/further-education/app-monthly-starts-202425-jan.csv") {
  app_monthly <- read_csv(paste0(data_dir, ees_file))
  app_monthly_api <- app_monthly |>
    rename(
      age_group = age_summary,
      assessment_type = std_fwk_flag,
      apprenticeship_level_desc = apps_level,
      apprentice_start_count = starts
    ) |>
    mutate(
      start_month = if_else(
        start_month != "Total",
        paste("01", start_month, "2024") |>
          dmy() |>
          month(label = TRUE, abbr = FALSE),
        start_month
      ),
      apprenticeship_level_desc = stringr::str_to_sentence(apprenticeship_level_desc),
      age_group = age_group |> stringr::str_replace_all("-", " to ")
    )

  api_file <- str_replace_all(ees_file, ".csv", "_api.csv")
  meta_file <- str_replace_all(api_file, ".csv", ".meta.csv")

  app_monthly_api |> write_csv(paste0(data_dir, api_file))

  app_monthly_api_meta <- app_monthly_api |>
    meta_template(add_filter_default = FALSE) |>
    filter(!(col_name %in% c("snapshot", "assessment_code"))) |>
    mutate(
      col_type = if_else(col_name == "apprentice_start_count", "Indicator", col_type),
      indicator_dp = if_else(col_name == "apprentice_start_count", "0", indicator_dp)
    )
  print(app_monthly_api_meta)

  app_monthly_api_meta |> write_csv(
    paste0(
      data_dir, meta_file
    )
  )
  return(app_monthly_api)
}

process_monthly_starts_ud <- function(
    ud_file = "app-underlying-data-monthly-starts-202425-jan.csv") {
  app_monthly_starts_ud <- read_csv(paste0(data_dir, ud_file))

  app_monthly_starts_api <- app_monthly_starts_ud |>
    mutate(
      time_period = year |> stringr::str_extract("[0-9]{6}"),
      time_identifier = "Academic year",
      time_frame = year |>
        stringr::str_replace("[0-9]{6}", "") |>
        stringr::str_trim(side = "both") |>
        stringr::str_to_sentence(),
      start_month = start_month |>
        paste("2024") |>
        lubridate::dmy() |>
        lubridate::month(abbr = "FALSE", label = TRUE),
      geographic_level = "National",
      country_code = "E92000001",
      country_name = "England",
      age_group = age_summary |> stringr::str_replace_all("-", " to ")
    ) |>
    select(
      time_period,
      time_identifier,
      time_frame,
      snapshot,
      geographic_level,
      country_code,
      country_name,
      apprenticeship_level_group = apps_level,
      apprenticeship_level = apps_level_detailed,
      ssa_tier_1,
      ssa_tier_2,
      apprenticeship_mandatory_degree = apps_level,
      assessment_type = std_fwk_flag,
      assessment_code = st_code,
      assessment_name = std_fwk_name,
      funding_type,
      start_month,
      age_group,
      apprentice_start_count = starts
    )

  message("# Underlying data")
  message("Number of columns: ", ncol(app_monthly_starts_ud))
  message("Number of rows: ", nrow(app_monthly_starts_ud))
  message(paste(names(app_monthly_starts_ud), collapse = ", "))

  message("# API data")
  message("Number of columns: ", ncol(app_monthly_starts_api))
  message("Number of rows: ", nrow(app_monthly_starts_api))
  message(paste(names(app_monthly_starts_api), collapse = ", "))

  out_file <- file |>
    str_replace_all("underlying", "api")
  meta_file <- out_file |>
    str_replace_all(".csv", ".meta.csv")

  app_monthly_starts_api |> write_csv(
    paste0(
      data_dir, out_file
    )
  )

  app_monthly_starts_api_meta <- app_monthly_starts_api |>
    meta_template(add_filter_default = FALSE) |>
    filter(!(col_name %in% c("snapshot", "assessment_code"))) |>
    mutate(
      col_type = if_else(col_name == "apprentice_start_count", "Indicator", col_type),
      label = label |>
        str_replace_all("Ssa", "SSA"),
      indicator_dp = if_else(col_name == "apprentice_start_count", "0", indicator_dp),
      filter_grouping_column = case_when(
        col_name == "ssa_tier_2" ~ "ssa_tier_1",
        col_name == "assessment_type" ~ "ssa_tier_2",
        col_name == "apprenticeship_level" ~ "apprenticeship_level_group",
        .default = filter_grouping_column
      )
    )

  app_monthly_starts_api_meta |> write_csv(
    paste0(
      data_dir, meta_file
    )
  )

  return(list(data = app_monthly_starts_api, meta = app_monthly_starts_api_meta))
}
