library("readr")
library("dplyr")
library("dfeR")

geographic_lookup <- read_csv()

file <- "../../test-data/leo_graduate_outcomes/LEO_provider_full_cycle_movement_data_2021.csv"

movement_data <- read_csv(file)

movement_data %>% 
  mutate(
   time_period = gsub("/20", "", tax_year),
   time_identifier = "Tax year",
   geographic_level = "Provider",
   ukprn = paste(ukprn)
  ) %>%
  select(
    tim_period, time_identifier, 
    geographic_level, country_name, country_code, 
    region_name, region_code, la_name, new_la_code, old_la_code,
    provider_name, ukprn,
    country_name_home, country_code_home, region_name_home, region_code_home, 
    country_name_current, country_code_current, region_name_current, region_code_current, 
    years_after_graduation, sex, graduates_count
  )
