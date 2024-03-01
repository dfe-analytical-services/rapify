library(readr)
library(dplyr)

la_country_lookup <- read_csv("data/reference/uk_las.csv")
la_lookup <- read_csv("https://raw.githubusercontent.com/dfe-analytical-services/dfe-published-data-qa/master/data/las.csv")

clean_full_cycle_data <- function() {
  full_cycle_raw <- read_csv("data/leo_raw/LEO_provider_full_cycle_movement_data_2021.csv")

  # Set appropriate columns
  full_cycle_clean <- full_cycle_raw %>%
    mutate(
      time_period = gsub("/20", "", tax_year),
      time_identifier = "Tax year",
      geographic_level = "Provider",
      country_pre_he = case_when(
        home_region_name %in% c("Wales", "Scotland") ~ home_region_name,
        .default = "England"
      ),
      region_pre_he = case_when(
        home_region_name == "Scotland" ~ "All Scottish regions",
        home_region_name == "Wales" ~ "All Welsh regions",
        .default = home_region_name
      ),
      country_post_he = case_when(
        current_region %in% c("Wales", "Scotland") ~ current_region,
        .default = "England"
      ),
      region_post_he = case_when(
        current_region == "Scotland" ~ "All Scottish regions",
        current_region == "Wales" ~ "All Welsh regions",
        .default = current_region
      ),
      la_name = provider_LA,
    ) %>%
    left_join(
      la_country_lookup
    ) %>%
    left_join(
      la_lookup %>% select(old_la_code, new_la_code, la_name)
    ) %>%
    mutate(
      old_la_code = if_else(is.na(old_la_code), "z", old_la_code),
      new_la_code = if_else(is.na(new_la_code), "z", new_la_code)
    ) %>%
    select(
      time_period,
      time_identifier,
      geographic_level,
      country_code,
      country_name,
      provider_ukprn = ukprn,
      provider_name,
      country_pre_he,
      region_pre_he,
      country_post_he,
      region_post_he,
      sex,
      years_after_graduation = YAG,
      academic_year,
      count_graduates = grads_uk
    )


  full_cycle_clean %>% write.csv("data/ees_tidy/leo_provider_full_cycle_movement_ees.csv", row.names = FALSE)

  meta <- data.frame(
    col_name = c("region_pre_he", "region_post_he", "sex", "years_after_graduation", "count_graduates"),
    col_type = c("Filter", "Filter", "Filter", "Filter", "Indicator"),
    label = c("Region prior to Higher Education", "Region after Higher Education", "Sex", "Years after graduation", "Number of graduates"),
    indicator_grouping = c("", "", "", "", ""),
    indicator_unit = c("", "", "", "", ""),
    indicator_dp = c("", "", "", "", "0"),
    filter_hint = c("", "", "", "", ""),
    filter_grouping_column = c("country_pre_he", "country_post_he", "", "", "")
  ) %>% write.csv("data/ees_tidy/leo_provider_full_cycle_movement_ees.meta.csv", row.names = FALSE)
}


leo_outcomes_raw <- read_csv("data/leo_raw/data-leo-graduate-and-postgraduate-outcomes.csv")