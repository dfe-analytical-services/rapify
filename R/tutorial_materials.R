
get_wide_data <- function(){
x <- read.csv("C:\\Users\\RBIELBY\\Department\ for\ Education\\Data\ Insights\ and\ Statistics\ -\ CSSU\\Statistics\ Development\ Team\\Explore\ Education\ Statistics\\Tutorials\\data-structures\\materials\\level_2_3_ages_16_19.csv")

wide <- x %>% pivot_wider(names_from = 'number_or_percentage', 
                          values_from = c("L2", "L3", "L2_eng_GCSE_ac", "L2_eng_GCSE_othL2", "L2_maths_GCSE_ac", "L2_maths_GCSE_othL2", "L2_em_GCSE_ac", "L2_em_GCSE_othL2",
                                          "L2_with_EM", "L2_eng_FSQ", "L2_maths_FSQ"),
                          names_glue = "{number_or_percentage}_{.value}"
                          ) %>%
  filter(!(breakdown_topic %in% c("J Ethnic group and FSM (Free School Meal) status", 
                                  "L Ethnic group, FSM (Free School Meal) status and gender",
                                  "K Detailed ethnic group and FSM (Free School Meal) status",
                                  "F SEN (Special Educational Need) Provision",
                                  "G Primary SEN (Special Educational Need)",
                                  "H Ethnic group",
                                  "I Ethnic group - detailed"))) %>%
  mutate(breakdown_topic=case_when(breakdown_topic=="A Gender" ~ "Sex",
                                   breakdown_topic=="B Disadvantaged status" ~ "Disadvantaged status",
                                   breakdown_topic=="C FSM (Free School Meal) status" ~ "Free School Meal status",
                                   breakdown_topic=="D IDACI (Income Deprivation Affecting Children Index)" ~ "Income Deprivation Affecting Children Index",
                                   breakdown_topic=="E SEN (Special Educational Need)" ~ "Special Educational Need",
                                   TRUE ~ 'Total'
                                     )
         )
  colnames(wide) <- tolower(colnames(wide))
  wide[wide == ':'] <- NA
  wide
}

wide <- get_wide_data()

tidy <- wide %>% 
  pivot_longer(
    cols=!c("time_period", "time_identifier", "geographic_level", "country_code", "country_name",
            "age", "breakdown_topic", "breakdown"),
    names_to = c(".value", "achievement",
    names_pattern = "(.*)_(.*)"
    )
  
)





















