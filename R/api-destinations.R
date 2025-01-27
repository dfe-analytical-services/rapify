library(dplyr)
library(tidyr)
library(readr)

destination_description_lookup <- data.frame(
  original_label = c(
    "overall",
    "education", "fe", "ssf", "sfc", "other_edu", "ind", "appru", "special", "spi", "edu_combo", "he",
    "appren", "appl3", "appl2",
    "all_work",
    "all_notsust", "notsust", "notsust_beneet", "only_beneet",
    "all_unknown", "unknown_id", "unknown"
  ),
  destination_group = c(
    "Overall",
    "Education", "Education", "Education", "Education", "Education", "Education", "Education", "Education", "Education", "Education", "Education",
    "Apprenticeships", "Apprenticeships", "Apprenticeships",
    "Employment",
    "Not sustained / beneet", "Not sustained / beneet", "Not sustained / beneet", "Not sustained / beneet",
    "Unknown", "Unknown", "Unknown"
  ),
  destination_description = c(
    "Overall",
    "All education", "Further education", "ssf", "Sixth form college", "other_edu", "ind", "Alternative provision / Pupil referral unit", "Special", "spi", "edu_combo", "Higher education",
    "All apprenticeships", "Level 3", "Level 2",
    "All employment",
    "All notsust", "notsust", "notsust_beneet", "only_beneet",
    "All unknown", "unknown_id", "unknown"
  )
)



destination_tidier <- function(file) {
  ks4_destinations <- readr::read_csv(file)

  out_file <- gsub(
    ".csv",
    "_eestidy.csv",
    file
  )

  non_pivot_columns <- c(
    "time_period", "time_identifier", "geographic_level", "country_code", "country_name",
    "region_code", "region_name", "old_la_code", "new_la_code", "la_name", "local_authority_selection_status",
    "institution_group", "institution_type", "school_laestab", "school_name", "admission_policy", "entry_gender",
    "breakdown_topic", "breakdown", "data_type", "version", "inst_count", "cohort"
  )

  non_pivot_columns_present <- intersect(
    non_pivot_columns,
    names(ks4_destinations)
  )
  
  ees_tidy_data <- ks4_destinations |>
    mutate(across(everything(), as.character)) |>
    tidyr::pivot_longer(
      -all_of(non_pivot_columns_present),
      names_to = "original_label"
    ) |>
    left_join(destination_description_lookup) |>
    pivot_wider(
      names_from = data_type
    ) |>
    select(-original_label)
  ees_tidy_with_data_dictionary_naming <- ees_tidy_data |>
    rename(
      cohort_count = "cohort",
      pupil_count = `Number of pupils`,
      pupil_percent = "Percentage"
    )
  if ("inst_count" %in% names(ees_tidy_with_data_dictionary_naming)) {
    ees_tidy_with_data_dictionary_naming <- ees_tidy_with_data_dictionary_naming |>
      rename(institution_count = "inst_count")
  }
  ees_tidy_with_data_dictionary_naming |>
    write_csv(out_file)
}

national_file <- "../../offline-data/api-destinations/ks4_dm_ud_202223_nat_prov.csv"
destination_tidier(national_file)

institution_file <- "../../offline-data/api-destinations/ks4_dm_ud_202223_inst_prov.csv"
destination_tidier(institution_file)
