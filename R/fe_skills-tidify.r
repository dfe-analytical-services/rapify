library(tidyr)
library(dplyr)

adv_learn_loans <- read.csv('data/adv-learner-loans-applications-202122.csv', stringsAsFactors = FALSE)

adv_learn_loans_sd <- adv_learn_loans %>% 
  mutate(
    breakdown_topic = case_when(
      ssa_t1_desc != 'Total' ~ 'Sector subject area (tier 1)',
      qualification_type != 'Total' ~ 'Qualification type',
      provider_type != 'Total' ~ 'Provider type',
      TRUE ~ characteristic_group      
    ),
    breakdown = case_when(
      ssa_t1_desc != 'Total' ~ ssa_t1_desc,
      qualification_type != 'Total' ~ qualification_type,
      provider_type != 'Total' ~ provider_type,
      TRUE ~ characteristic      
    )
  ) %>%
  select(time_period, time_identifier, geographic_level, country_code, country_name,
         region_name, region_code, breakdown_topic, breakdown, 
         apps_received, apps_approved, loan_amount_awarded_in_thousands) %>%
  arrange(time_period, geographic_level, region_code, breakdown_topic, breakdown)

adv_learn_loans_sd_meta <- data.frame(
  
)

write.csv(adv_learn_loans_sd, 'data/adv-learner-loans-applications-202122-breakdown.csv', row.names = FALSE)