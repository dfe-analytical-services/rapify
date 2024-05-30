library("readr")
library("dplyr")
library("dfeR")

la_code_lookup_raw <- read_csv("https://raw.githubusercontent.com/dfe-analytical-services/dfe-published-data-qa/master/data/ward_lad_la_pcon_hierarchy.csv") 

la_code_lookup <- la_code_lookup_raw %>% 
  filter(most_recent_year_included>=2023) %>%
  select(new_la_code, la_name) %>% distinct()

file <- "../../test-data/leo_graduate_outcomes/LEO_provider_full_cycle_movement_data_2021.csv"

movement_data <- read_csv(file)

ees_version <- movement_data %>% 
  mutate(
   time_period = gsub("/20", "", tax_year),
   time_identifier = "Tax year",
   geographic_level = "Provider",
   ukprn = paste(ukprn)
  ) %>%
  left_join(la_code_lookup) %>%
  mutate(
    country_name = case_when(
      grepl("S", new_la_code) ~ 'Scotland',
      grepl("W", new_la_code) ~ 'Wales',
      grepl("N", new_la_code) ~ 'Northern Ireland',
      .default = 'England'
    ),
    country_code = case_when(
      grepl("S", new_la_code) ~ 'S92000003',
      grepl("W", new_la_code) ~ 'W92000004',
      grepl("N", new_la_code) ~ 'N92000002',
      .default = 'E92000001'
    )
    
  )


  select(
    time_period, time_identifier, 
    geographic_level, country_name, country_code, 
    region_name, region_code, la_name, new_la_code, old_la_code,
    provider_name, ukprn,
    country_name_home, country_code_home, region_name_home, region_code_home, 
    country_name_current, country_code_current, region_name_current, region_code_current, 
    years_after_graduation, sex, graduates_count
  )
