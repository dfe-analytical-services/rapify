meta_template <- function(data) {
  standard_indicators <- c("pupil_count", "pupil_fte", "pupil_percent")
  base_filters <- c(
    "time_period", "time_identifier", "geographic_level",
    "country_code", "country_name", 
    "region_code", "region_name", 
    "old_la_code", "new_la_code", "la_name", 
    "school_laestab",
    "school_urn", "school_name"
  )
  meta <- data.frame(col_name = names(data)) %>%
    filter(
      !(col_name %in% base_filters)
    ) %>%
    mutate(
      col_type = case_when(col_name %in% standard_indicators ~ "Indicator",
      .default = "Filter"),
      label = stringr::str_to_sentence(gsub("_", " ", col_name)),
      indicator_grouping = "",
      indicator_unit = "",
      indicator_dp = "",
      filter_hint = "",
      filter_grouping_column = ""
    )
}
