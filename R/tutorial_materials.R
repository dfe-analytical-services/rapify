get_wide_data <- function() {
  x <- read.csv("C:\\Users\\RBIELBY\\Department\ for\ Education\\Data\ Insights\ and\ Statistics\ -\ CSSU\\Statistics\ Development\ Team\\Explore\ Education\ Statistics\\Tutorials\\data-structures\\materials\\level_2_3_ages_16_19_characteristics.csv")

  wide <- x %>%
    rename(breakdown_topic = characteristic_group, breakdown = characteristic) %>%
    pivot_wider(
      names_from = "number_or_percentage",
      values_from = c(
        "Level_2", "Level_3", "L2_eng_GCSE_ac", "L2_eng_GCSE_othL2", "L2_maths_GCSE_ac", "L2_maths_GCSE_othL2", "L2_em_GCSE_ac", "L2_em_GCSE_othL2",
        "L2_with_EM", "L2_eng_FSQ", "L2_maths_FSQ"
      ),
      names_glue = "{number_or_percentage}-{.value}"
    ) %>%
    filter(
      !(breakdown_topic %in% c(
        "J Ethnic group and FSM (Free School Meal) status",
        "L Ethnic group, FSM (Free School Meal) status and gender",
        "K Detailed ethnic group and FSM (Free School Meal) status",
        "F SEN (Special Educational Need) Provision",
        "G Primary SEN (Special Educational Need)",
        "H Ethnic group",
        "I Ethnic group - detailed"
      )),
      breakdown != "Attainment gap",
      time_period <= 201819 & time_period >= 201011
    ) %>%
    mutate(breakdown_topic = case_when(
      breakdown_topic == "A Gender" ~ "Sex",
      breakdown_topic == "B Disadvantaged status" ~ "Disadvantaged status",
      breakdown_topic == "C FSM (Free School Meal) status" ~ "Free School Meal status",
      breakdown_topic == "D IDACI (Income Deprivation Affecting Children Index)" ~ "Income Deprivation Affecting Children Index",
      breakdown_topic == "E SEN (Special Educational Need)" ~ "Special Educational Need",
      TRUE ~ "Total"
    )) %>%
    rename(`number-in_ss_cohort` = number_in_ss_cohort)
  colnames(wide) <- tolower(colnames(wide))
  wide[wide == ":"] <- NA
  wide
}




tidy_the_data <- function() {
  wide <- get_wide_data()

  tidy <- wide %>%
    pivot_longer(
      cols = !any_of(c(
        "time_period", "time_identifier", "geographic_level", "country_code", "country_name",
        "age", "breakdown_topic", "breakdown"
      )),
      names_to = c(".value", "achievement"),
      names_pattern = "(.*)-(.*)"
    )
}







clean_data <- function(wide, tidy) {
  wide[is.na(wide)] <- "x"
  wide %>% write.csv("data/tutorials/wide2tidy_input.csv", row.names = FALSE)
  tidy$achievement <- gsub("_", " ", tidy$achievement)
  tidy$achievement <- gsub("othl2", "OTHL", tidy$achievement)
  tidy$achievement <- gsub(" eng ", " Engineering ", tidy$achievement)
  tidy$achievement <- gsub(" maths ", " Maths ", tidy$achievement)
  tidy$achievement <- gsub("fsq", "FSQ", tidy$achievement)
  tidy$achievement <- gsub(" ac", " AC", tidy$achievement)
  tidy$achievement <- gsub(" em", " EM", tidy$achievement)
  tidy$achievement <- gsub("l2", "Level 2", tidy$achievement)
  tidy$achievement <- gsub("l3", "Level 3", tidy$achievement)
  tidy$achievement <- gsub("level", "Level", tidy$achievement)
  tidy$achievement <- gsub("in ss cohort", "State sector cohort", tidy$achievement)
  tidy$achievement <- gsub("gcse", "GCSE", tidy$achievement)
  tidy[is.na(tidy)] <- "x"
  tidy %>% write.csv("data/tutorials/wide2tidy_result.csv", row.names = FALSE)
}
