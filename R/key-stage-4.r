library(dplyr)
library(tidyr)
library(stringr)


ks4_characteristics <- read.csv(
  '../../test-data/key-stage-4/gcse-subject-characteristics-wide.csv', 
  stringsAsFactors = FALSE
)

tidied <- ks4_characteristics %>% 
  mutate(across(where(is.numeric), as.character)) %>% 
  rename(`total_subject entry`=`subject_entry`) %>%
  rename_with(~ gsub("total_", "total-",.)) %>%
  rename_with(~ gsub("percentage_", "percent-",.)) %>%
  pivot_longer(
    !c(time_period, time_identifier, version, 
       geographic_level, country_code, country_name,
       characteristic, secondary_characteristic, gender, establishment_type,
       subject, qualification_type),
    names_pattern = '(.*)-(.*)',
    names_to = c(".value", "attainment_metric")
    ) %>% 
  mutate(
    attainment_metric = gsub("_([9a])_", " \\1 - ", attainment_metric),
    attainment_metric = str_to_title(gsub("_", " ", attainment_metric)),
    percent = percent %>% if_else(attainment_metric == 'Subject Entry', 'z', .)
  )

tidied_wbreakdown <- tidied %>% 
  filter(secondary_characteristic=='') %>%
  select(-secondary_characteristic) %>%
  rename(breakdown=characteristic) %>%
  mutate(breakdown_topic = case_when(
    breakdown %in% c("Any SEN", "No identified SEN") ~ "Special educational needs status",
    breakdown %in% c("FSM", "FSM all Other") ~ "Free school meal status",
    breakdown %in% c("Disadvantaged", "Disadvantaged all Other") ~ "Disadvantaged status",
    breakdown %in% c("English", "Other than English") ~ "English as a foreign language status",
    .default = 'Total'
    ),
    .before = breakdown) %>%
  mutate(
    breakdown = breakdown %>% gsub("FSM all Other", "Not eligible for FSM", .),
    breakdown = breakdown %>% gsub("Disadvanted all Other", "Not disadvantaged", .)
    ) %>%
  rbind(
    tidied %>% 
      filter(secondary_characteristic!='') %>% 
      select(-characteristic) %>% 
      rename(breakdown=secondary_characteristic) %>%
      mutate(
        breakdown_topic=case_when(
          breakdown=='Total' ~ 'Ethnicity major',
          .default='Ethnicity minor'
        ),
        .before = 'breakdown'
      )
  ) %>% 
  select(time_period, time_identifier, version, geographic_level, 
         country_code, country_name, establishment_type, 
         breakdown_topic, breakdown, sex=gender, 
         qualification_type, subject, attainment_metric,
         pupil_count=total, pupil_percent=percent) %>%
  # filter(pupil_count != 'z') %>% 
  arrange(time_period, geographic_level, country_name, establishment_type, 
          breakdown_topic, breakdown, sex, qualification_type, subject, attainment_metric)

tidied_wbreakdown %>% write.csv('../../test-data/key-stage-4/gcse-subject-characteristics-tidy.csv', row.names = F)