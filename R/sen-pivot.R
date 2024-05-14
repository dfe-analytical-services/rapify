library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(dfeR)
standard_filters <- c(
  "time_period", "time_identifier", "geographic_level", "country_code", "country_name",
  "region_name", "region_code", "old_la_code", "la_name", "new_la_code"  )

context_filters <- c("phase_type_grouping", "pupil_sen_status", "primary_need")

pivot_sen_data <- function(file){
  data <- read_csv(file)
  pivot <- data %>% 
    mutate(across(where(is.character), ~ if_else(is.na(.x), "", .x))) %>%
    pivot_longer(!any_of(c(standard_filters, context_filters)),
                        names_to = c("master_filter"),
                        values_to = 'value'
  )
  tidied <- pivot %>% mutate(
    metric = case_when(
      grepl("percent", master_filter) ~ 'pupil_percent',
      .default = 'pupil_count'
    ),
    breakdown_topic = case_when(
      grepl("age", master_filter) ~ "Age",
      grepl("sex", master_filter) ~ "Sex",
      .default = 'Total'
    ),
    breakdown = case_when(
      grepl("number_of_pupils", master_filter) ~ "Total",
      grepl("female", master_filter) ~ "Female",
      grepl("male", master_filter) ~ "Male",
      .default = str_to_sentence(
        gsub('_',' ',
          gsub('_percent','', master_filter),
          )
        )
      ) 
    ) %>%
    select(-master_filter) %>% 
    pivot_wider(
      names_from = metric,
      values_from = value
    ) %>%
    mutate(
      pupil_percent = case_when(breakdown_topic == 'Total' ~ 100.,
      .default = round_five_up(pupil_percent, 2)
      )
    )
  
}

sen_file <- '../../test-data/sen/sen_age_sex_.csv'
sen_pivot <- pivot_sen_data(sen_file)
write.csv(sen_pivot, gsub('.csv','tidy.csv', sen_file), row.names=FALSE)


