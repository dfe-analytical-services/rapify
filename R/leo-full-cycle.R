library(readr)
library(dplyr)

full_cycle_raw <- read_csv("data/leo_raw/LEO_provider_full_cycle_movement_data_2021.csv")
la_country_lookup <- read_csv('data/reference/uk_las.csv')


# Set appropriate columns
full_cycle_clean <- full_cycle_raw %>%
  mutate(
    time_period = gsub("/20","", tax_year),
    time_identifier = "Tax year",
    geographic_level = 'Provider',
    country_pre_he = case_when(
      home_region_name %in% c('Wales', "Scotland") ~ home_region_name,
      .default = "England"
    ),
    region_pre_he = case_when(
      home_region_name %in% c('Wales', "Scotland") ~ "Total",
      .default = home_region_name
    ),
    country_post_he = case_when(
      current_region %in% c('Wales', "Scotland") ~ current_region,
      .default = "England"
    ),
    region_post_he = case_when(
      current_region %in% c('Wales', "Scotland") ~ "Total",
      .default = current_region
    ),
    la_name = provider_LA,
  ) %>%
  left_join(
    la_country_lookup
  ) %>%
  select(
    time_period,
    time_identifier,
    geographic_level,
    country_code,
    country_name,
    la_name,
    provider_ukprn = ukprn,
    provider_name,
    country_pre_he,
    region_pre_he,
    country_post_he,
    region_post_he,
    years_after_graduation = YAG,
    academic_year,
    count_graduates = grads_uk
    )

