
scenario_characteristics <- function(
    original='data/workforce_teacher_characteristics_NatReg_202022.csv'){
  dfchars <- read.csv(original, stringsAsFactors = FALSE)
  dfnational <- dfchars %>% 
    filter(geographic_level %in% c('National','Regional'),
           time_period %in% c(202122,202021))

  df_filters <- dfnational %>% select(-characteristic,-characteristic_group)
  write.csv(df_filters,'data/data-structure-testing/workforce_teacher_characteristics_NatReg_202022_filters.csv',row.names=FALSE)
  df_characs <- dfnational %>% select(-grade,-gender,-age_group,-working_pattern,
                                      -qts_status,-on_route,-ethnicity_major,
                                      -ethnicity_minor,-ethnic_minority)
  write.csv(df_characs,'data/data-structure-testing/workforce_teacher_characteristics_NatReg_202022_characteristics.csv',row.names=FALSE)
  return(dfnational)
}


write.csv(dfexmp,'data/ks4_subject_main_wide_202122.csv',row.names=FALSE)


x<-pivot_filter_indicator(dfexmp,
                          keep_filters=c("time_period","time_identifier",
                                         "geographic_level","country_code","country_name",
                                         "gender","school_type",
                                         "admission_type",
                                         "religious_denomination","subject",
                                         "qualification_type","subject_entry"))
write.csv(x,'data/ks4_subject_main_tidy_202122.csv',row.names=FALSE)


