library(dplyr)
library(tidyr)
library(readr)


destination_tidier <- function(file) {
  ks4_destinations <- readr::read_csv(file)

  out_file <- gsub(
    ".csv",
    "eestidy.csv",
    file
  )

  ks4_destinations |>
    mutate(across(everything(), as.character)) |>
    tidyr::pivot_longer(
      -c(time_period, time_identifier, geographic_level, country_code, country_name, institution_group, institution_type, breakdown_topic, breakdown, data_type, version, inst_count, cohort)
    ) |>
    pivot_wider(
      names_from = data_type
    ) |>
    write_csv(out_file)
}

national_file <- "../../offline-data/api-destinations/ks4_dm_ud_202223_nat_prov.csv"
destination_tidier(national_file)

institution_file <- "../../offline-data/api-destinations/ks4_dm_ud_202223_inst_prov.csv"
destination_tidier(national_file)
