library(tidyr)
library(dplyr)
library(stringr)

data_file <- "../../test-data/elective_home_education_data/ehe_census.csv"

ehe_wide <- read.csv(data_file, stringsAsFactors = FALSE)


ehe_tidy <- ehe_wide %>%
  rename_with(~ gsub("ehe_census_date","total-total",.)) %>%
  rename_with(~ gsub("ehe_reason_","reason-",.)) %>%
  rename_with(~ gsub("ehe_year","year group-",.)) %>%
  rename_with(~ gsub("ehe_reception","year group-reception",.)) %>%
  rename_with(~ gsub("ehe_","sex-",.)) %>%
  pivot_longer(
    !c(time_period, time_identifier, 
       geographic_level, country_code, country_name, 
       region_code, region_name,old_la_code, new_la_code, la_name),
    names_to='breakdown',
    values_to = 'pupils_count'
  ) %>%
  separate(breakdown, c('breakdown_topic', 'breakdown'), '-') %>%
  mutate(
    breakdown_topic = str_to_sentence(breakdown_topic),
    breakdown = str_to_sentence(
      gsub("year", "year ", gsub("sex_", "", gsub("_", " ", breakdown))))
  )
