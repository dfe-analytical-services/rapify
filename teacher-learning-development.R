library(readr)
source('R/tidify.R') 

teacher_leader_development_ecf_2022_23 <- read_csv("data/teacher_leader_development_ecf_2022-23.csv")
tidydata <-pivot_filter_indicator(teacher_leader_development_ecf_2022_23 ,
                                  filter_name = "type_ects_training",
                                  keep_filters = c("time_period"   ,                                      "time_identifier"  ,                                  
                                                   "geographic_level"     ,                               "country_code"  ,                                     
                                                   "country_name" ,                                       "region_code" ,                                       
                                                 "region_name"  ,                                       "la_name"   ,                                         
                                              "old_la_code"  ,                                       "new_la_code",                                        
                                            "breakdown"  ,                                         "group_name"  ) )

# Functions used
# mutate() - creates or changes a column
# case_when() - change an entry in a cell based on a set of criteria
# grepl() - return true if a sub-string is found in a string
# %>% - pipe (send output from one function as input for the next)
cleaned <- tidydata %>% mutate(
dummy_column = 23846 ,
  filter_ecfs = case_when( 
    type_ects_training == "swfc_ects" ~ "Early career teachers in School Workforce Census", 
    type_ects_training == "swfc_ects_school_led" ~ "Percentage of ECTs who started school-led ECF-based induction",
    type_ects_training == "swfc_ects_pending_confirmation_provider_led" ~ "Percentage of ECTs who started provider-led ECF-based",
    type_ects_training == "swfc_ects_provider_led" ~ "Percentage of ECTs who started provider-led ECF-based induction",
    type_ects_training == "ects_count_provider_led" ~ "Total number of ECTs who started provider-led ECF-based induction",
    type_ects_training == "mentors_trained_count_provider_led" ~ "Mentors trained for provider-led ECF-based induction",
    .default = type_ects_training
  ),
  school_provider_led = case_when(
    grepl("provider_led", type_ects_training) ~ "Provider led",
    grepl("Total", type_ects_training) ~ "School led",
    .default = 'Total'  )
) 
View(cleaned)




View(tidydata)
print(tidydata$type_ects_training %>% unique())

# Read in meta file
ecf_meta <- read_csv("data/teacher_leader_development_ecf_2022-23.meta.csv")
View(ecf_meta)
library(dplyr)
